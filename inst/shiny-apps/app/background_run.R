# Background batch-run helpers (Setup & Run tab).
#
# Kept separate from setup_helpers.R (P9-01's dry-run preview logic, which
# stays Shiny-free and independently testable) and from app.R itself, so the
# launch/poll mechanism can also be exercised directly outside of a running
# Shiny session -- start_background_batch()/poll_batch_progress() take plain
# arguments and return plain values, no reactives.
#
# Scope note (P9-05 vs P9-06): this file provides the actual async execution
# primitive (start_background_batch()) and a filesystem-based progress poll
# (poll_batch_progress()), originally wired into app.R behind a minimal
# launch button and "N of M files processed" status line. P9-06 added the
# richer display support below (get_failed_file_details(),
# estimate_remaining_seconds(), format_duration_seconds()) -- still plain
# functions with no reactives, so they stay callable/testable outside a
# running Shiny session -- and app.R now renders a progress bar, live counts,
# an ETA, and per-file failure detail on top of the same polling loop P9-05
# built. Neither eyeQualityBatch() nor poll_batch_progress()'s own logic
# changed for this.

# ensure_future_plan: set future::plan(multisession) once for this R process,
# if it isn't already set to a multisession-family plan. Idempotent so it's
# safe to call on every launch rather than requiring app.R to set the plan at
# source time.
#
# Scoping: set once per R *process* (i.e. effectively once per running Shiny
# app, not once per browser tab/session) rather than per Shiny session. For
# the typical way this app is run (`eyeQualityApp()` -> a single `runApp()`
# call in one R process), that's the same thing in practice. Multisession
# workers are OS processes owned by that R process, not by any one browser
# connection, which matters for what happens if a user closes the tab
# mid-run: the running future keeps executing on its worker process
# regardless -- Shiny tears down the *session* environment (so this app's
# reactiveVal holding the future/promise is lost, and the polling UI has
# nothing left to display against), but does not touch the worker running
# eyeQualityBatch(). Files already written by the time of tab-close, and any
# the run goes on to write afterward, land on disk exactly as they would if
# the tab had stayed open; there is just no UI left watching. If the whole R
# process hosting the app is killed (not just the tab), the multisession
# workers die with it and the run stops. This is a known v1 limitation
# consistent with the P9-05 decision note: processx's main edge over
# future/promises is exactly this survives-a-session-loss case, and it was
# judged unnecessary to satisfy this phase's gate.
ensure_future_plan <- function() {
  if (!inherits(future::plan(), "multisession")) {
    future::plan(future::multisession)
  }
  invisible(NULL)
}

# start_background_batch: launch eyeQualityBatch() on a future::multisession
# worker and return a promises::promise. The promise resolves to
# list(status = "ok") on a clean run, or list(status = "error", message = ...)
# if eyeQualityBatch() itself raised an error that escaped its own per-file
# tryCatch (e.g. a bad argument caught by its own upfront validation, or
# something before/after the per-file parLapply loop) -- this is a distinct,
# coarser failure mode from the per-file failures eyeQualityBatch() already
# tracks itself (see get_qcsummary_output_path()/"failedfiles" handling in
# R/eyeQualityBatch.R); both are surfaced by poll_batch_progress()/app.R, but
# a promise rejection would otherwise be an unhandled-error stop for the
# Shiny session, hence the wrapping tryCatch here.
#
# numberCores defaults to 2 rather than eyeQualityBatch()'s own NULL
# (85%-of-cores auto-detect): eyeQualityBatch() spawns its own
# parallel::makeCluster(numberCores) *inside* whichever process runs it, so a
# GUI-launched run already sits inside one future::multisession worker
# process, which then spawns a further cluster of its own. Letting that inner
# cluster default to ~85% of the machine's cores, launched casually from a
# button in a GUI by a user who may not be thinking about resource contention
# the way someone invoking eyeQualityBatch() directly from a script would, is
# an easy way to oversubscribe a shared/interactive machine. 2 is a
# conservative default that keeps a GUI-launched run well-behaved without
# building a numberCores input into the form -- P9-03 (the full parameter
# form) is deferred to v2, and a single hardcoded default is the simpler
# option judgment call here; revisit with an actual input if this proves too
# conservative in practice.
#
# directoryBIDS, batchName, outputDir, force, ...: passed through unchanged
# to eyeQualityBatch(); see R/eyeQualityBatch.R for their meaning.
start_background_batch <- function(directoryBIDS,
                                    batchName,
                                    numberCores = 2L,
                                    outputDir = NULL,
                                    force = FALSE,
                                    ...) {
  ensure_future_plan()

  promises::future_promise({
    # A fresh future::multisession worker starts with no packages attached
    # (only base R), so the bare `eyeQualityBatch(...)` call below is
    # unresolvable without this -- confirmed by first running this
    # end-to-end without the explicit library() call and hitting "could not
    # find function eyeQualityBatch". Attaching via library() rather than
    # calling eyeQuality::eyeQualityBatch(...) matters for a second reason
    # too: eyeQualityBatch() itself does `parallel::clusterExport(cl,
    # "eyeQuality")` internally, to give its own worker pool access to the
    # eyeQuality() function, and that clusterExport() call looks the object
    # up by name in .GlobalEnv (the default `envir`) of whatever process is
    # running eyeQualityBatch() -- i.e. this multisession worker, not the
    # Shiny app's main session. A qualified call would resolve on its own
    # but leave that internal clusterExport() unable to find "eyeQuality" on
    # a worker that never ran library() itself, since namespace-loading
    # alone doesn't attach a package to the search path. Both failure modes
    # point at the same fix: worth knowing about for anyone else scripting
    # eyeQualityBatch() from a fresh process rather than an interactive
    # session, but addressed here at the call site rather than by touching
    # eyeQualityBatch() itself.
    library(eyeQuality)

    tryCatch(
      {
        eyeQualityBatch(
          directoryBIDS = directoryBIDS,
          batchName = batchName,
          numberCores = numberCores,
          outputDir = outputDir,
          force = force,
          ...
        )
        list(status = "ok")
      },
      error = function(e) list(status = "error", message = conditionMessage(e))
    )
  })
}

# count_completed_qcsummary_files: find every qcsummary output already on
# disk for a given directoryBIDS/batchName/outputDir combination, using the
# same naming convention eyeQualityBatch() writes to (and checks for its own
# resumability logic) -- see get_qcsummary_output_path() in
# R/eyeQualityBatch.R, kept in sync there for the authoritative per-file path
# construction. This function doesn't need the actual candidate file list;
# it recognizes completed output purely by the shared filename convention
# (recursively under outputDir if set, else under directoryBIDS, which covers
# every file's own nested "<file_dir>/derivatives/eyeQuality-v1/" location
# the default outputDir = NULL case writes to).
#
# batchName is free-text a user types into the Setup form (a study/session
# label), not a regex -- it used to be interpolated directly into
# list.files()'s own `pattern` argument (a regex), which silently broke
# matching for a batchName containing any regex metacharacter. Confirmed by
# direct reproduction: batch names as ordinary as "pilot (v2)", "study+2", or
# "cohort[A]" made this function return character(0) for the ENTIRE run, even
# though eyeQualityBatch() was genuinely writing real qcsummary.tsv output to
# disk the whole time -- the live-polling observe() in app.R (see its
# invalidateLater(2000, session) loop) would then show the progress bar stuck
# at 0% for the whole run before jumping straight to the final count once the
# batch's own completion promise resolves (which reaches "done" independent
# of this file count). A plain "." in batchName does NOT reproduce this (a
# literal "." coincidentally still matches "." in the real filename, since
# unescaped "." is a regex superset, not a narrower match) -- it takes an
# actual grouping/quantifier/class metacharacter to break the match outright,
# which is why this was worth confirming with more than one example rather
# than concluding "." alone was safe.
#
# Fixed by escaping batchName's own regex metacharacters (escape_regex_literal(),
# below) before it's interpolated into list.files()'s `pattern` regex, rather
# than restructuring the match into a separate broad-scan-then-filter pass --
# an earlier version of this fix did exactly that (list every
# "*_preproc_qcsummary.tsv" file under `root` unconditionally, then filter by
# a literal endsWith() check against each candidate's basename), which is
# also correct, but measurably changes what a single recursive list.files()
# call over `root` can return; when `root` is a heavily-populated, long-lived
# directory (as it can be against a real, months-old study directory a user
# points this at repeatedly), a broader first-pass match returns a bigger
# intermediate candidate list than a batchName-scoped one, for no benefit --
# escaping keeps this function's cost profile identical to a single,
# specifically-scoped list.files() call, the same shape it always was.
#
# Returns a character vector of matched qcsummary filepaths (possibly empty).
count_completed_qcsummary_files <- function(directoryBIDS, batchName, outputDir = NULL) {
  root <- if (!is.null(outputDir)) outputDir else directoryBIDS
  if (is.null(root) || !dir.exists(root)) {
    return(character(0))
  }

  pattern <- paste0("_desc-", escape_regex_literal(batchName), "_preproc_qcsummary\\.tsv$")
  list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE)
}

# escape_regex_literal: escape every POSIX extended regex metacharacter in a
# plain string so it can be interpolated into another regex (e.g. list.files()'s
# own `pattern` argument) and only ever match itself literally. Base R has no
# built-in equivalent (unlike e.g. Python's re.escape()) -- this covers every
# character TRE (R's default regex engine, see ?regex) treats specially:
# . \ | ( ) [ ] { } ^ $ * + ?. Note the `]` placed immediately after the
# opening `[` in the character class below is a standard regex idiom for
# including a literal "]" as the class's first member without needing its own
# escape, since a `]` immediately following `[` can't close the class yet.
escape_regex_literal <- function(x) {
  gsub("([]\\[{}()+*^$.|?\\\\])", "\\\\\\1", x)
}

# poll_batch_progress: filesystem-based progress snapshot for an in-flight or
# completed batch run -- safe to call at any point (before a run starts,
# mid-run, or after completion), and independent of holding a live
# future/promise reference (see ensure_future_plan()'s scoping note above for
# why that independence matters).
#
# directoryBIDS, batchName, outputDir: identify the run, same meaning as
#   passed to start_background_batch()/eyeQualityBatch().
# n_expected: total candidate file count this run is working against (the
#   dry-run preview's matched_count from build_dry_run_preview(), computed
#   before the run started).
#
# Returns a list: n_done (qcsummary outputs found so far), n_expected (echoed
# back), n_failed (integer count from parsePreprocessingBatchSummary()'s
# "failedfiles" section if that section is parseable yet, else NA_integer_ --
# eyeQualityBatch() only writes that section once all per-file processing has
# finished, so this stays NA for the entire duration of a still-running batch
# and only becomes available once the batch summary file has it).
poll_batch_progress <- function(directoryBIDS, batchName, n_expected, outputDir = NULL) {
  n_done <- length(count_completed_qcsummary_files(directoryBIDS, batchName, outputDir))

  n_failed <- NA_integer_
  summary_file <- file.path(
    directoryBIDS,
    paste0("preprocessing_batch_summary_desc-", batchName, ".txt")
  )
  if (file.exists(summary_file)) {
    failed <- tryCatch(
      eyeQuality::parsePreprocessingBatchSummary(summary_file, info_to_extract = "failedfiles"),
      error = function(e) NULL
    )
    if (!is.null(failed)) {
      n_failed <- nrow(failed)
    }
  }

  list(
    n_done = n_done,
    n_expected = n_expected,
    n_failed = n_failed
  )
}

# get_failed_file_details: per-file failure detail (filepath + error message)
# for a run whose batch summary is already parseable -- reads the same
# summary file poll_batch_progress() locates by convention, via
# parsePreprocessingBatchSummary(info_to_extract = "failedfiles") (see
# vignettes/batch-processing.Rmd for the same calling convention against a
# completed run). Like poll_batch_progress()'s n_failed, this is only
# meaningful once the batch summary exists and is parseable -- i.e.
# effectively only after the whole run finishes, since eyeQualityBatch()
# writes the "failedfiles" section once at the end, not incrementally.
#
# Returns a data.frame with "file"/"error" columns (parsePreprocessingBatchSummary()'s
# own shape for this section), or NULL if the summary file doesn't exist yet
# or isn't parseable (e.g. called mid-run).
get_failed_file_details <- function(directoryBIDS, batchName) {
  summary_file <- file.path(
    directoryBIDS,
    paste0("preprocessing_batch_summary_desc-", batchName, ".txt")
  )
  if (!file.exists(summary_file)) {
    return(NULL)
  }
  tryCatch(
    eyeQuality::parsePreprocessingBatchSummary(summary_file, info_to_extract = "failedfiles"),
    error = function(e) NULL
  )
}

# estimate_remaining_seconds: a deliberately simple linear-rate ETA for an
# in-progress run, extrapolating from wall-clock elapsed time and files
# completed so far (average per-file rate so far, times files remaining).
# Every input is real, already-polled data (no fabricated numbers), but the
# estimate itself is naive -- it doesn't account for per-file duration
# variance, uneven scheduling across the underlying parallel::parLapply
# workers, files that are slower/faster than average, etc. With only one or
# two files done it will be noisy; that's an honest property of the method,
# not a bug to work around.
#
# start_time: a Sys.time()-style POSIXct marking when the run was launched.
# n_done, n_expected: current poll_batch_progress() counts.
#
# Returns NA_real_ (seconds) when no meaningful estimate is available yet --
# before any file has completed (undefined rate), once nothing remains, or
# given otherwise-invalid input -- rather than guessing.
estimate_remaining_seconds <- function(start_time, n_done, n_expected) {
  if (is.null(start_time) || is.null(n_done) || is.null(n_expected)) {
    return(NA_real_)
  }
  if (is.na(n_done) || is.na(n_expected) || n_done <= 0 || n_done >= n_expected) {
    return(NA_real_)
  }

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (elapsed <= 0) {
    return(NA_real_)
  }

  rate <- n_done / elapsed
  (n_expected - n_done) / rate
}

# format_duration_seconds: human-readable rendering of estimate_remaining_seconds()'s
# output, for direct use in the UI. Returns NA_character_ (rather than a
# formatted string) for NA/negative input, so callers can decide whether to
# omit the ETA line entirely instead of showing something misleading.
format_duration_seconds <- function(secs) {
  if (is.null(secs) || is.na(secs) || secs < 0) {
    return(NA_character_)
  }
  if (secs < 60) {
    return(sprintf("~%d sec", max(1L, round(secs))))
  }
  mins <- secs / 60
  if (mins < 60) {
    return(sprintf("~%.1f min", mins))
  }
  sprintf("~%.1f hr", mins / 60)
}
