# Background batch-run helpers.
#
# Kept separate from helpers.R (P9-01's dry-run preview logic, which stays
# Shiny-free and independently testable) and from app.R itself, so the
# launch/poll mechanism can also be exercised directly outside of a running
# Shiny session -- start_background_batch()/poll_batch_progress() take plain
# arguments and return plain values, no reactives.
#
# Scope note (P9-05 vs P9-06): this file provides the actual async execution
# primitive (start_background_batch()) and a filesystem-based progress poll
# (poll_batch_progress()), wired into app.R behind a minimal launch button and
# "N of M files processed" status line. A full live-progress UI (progress bar,
# ETA, per-file detail table) is P9-06's job, not this one -- the status
# surface here is deliberately plain text.

# ensure_future_plan: set future::plan(multisession) once for this R process,
# if it isn't already set to a multisession-family plan. Idempotent so it's
# safe to call on every launch rather than requiring app.R to set the plan at
# source time.
#
# Scoping: set once per R *process* (i.e. effectively once per running Shiny
# app, not once per browser tab/session) rather than per Shiny session. For
# the typical way this app is run (`runSetupApp()` -> a single `runApp()`
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
# Returns a character vector of matched qcsummary filepaths (possibly empty).
count_completed_qcsummary_files <- function(directoryBIDS, batchName, outputDir = NULL) {
  root <- if (!is.null(outputDir)) outputDir else directoryBIDS
  if (is.null(root) || !dir.exists(root)) {
    return(character(0))
  }

  pattern <- paste0("_desc-", batchName, "_preproc_qcsummary\\.tsv$")
  list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE)
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
