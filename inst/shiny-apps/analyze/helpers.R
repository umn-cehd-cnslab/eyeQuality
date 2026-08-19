# Analyze/QC Explorer app helper functions.
#
# Kept separate from app.R and free of Shiny-specific code (no reactives, no
# input/output objects) so load_qcsummary_table() can be sourced and called
# directly against a real output directory outside of a running Shiny
# session, for testing or scripting -- same pattern as the Setup app's
# helpers.R/build_dry_run_preview().

# qcsummary_filename_pattern: the shared naming convention every qcsummary
# output is written under -- see get_qcsummary_output_path()/saveFiles() in
# R/eyeQualityBatch.R and R/saveFiles.R, which construct
# "<...>_desc-<batchName>_preproc_qcsummary.tsv" (batchName present) or
# "<...>_desc-preproc_qcsummary.tsv" (batchName NULL). Note the second form has
# no underscore directly before "preproc" -- it's "desc-preproc", not
# "desc-_preproc" -- because saveFiles()'s qcsummarydesc is built as
# paste0("_desc-", if (is.null(batchName)) "" else paste0(batchName, "_"),
# "preproc_qcsummary"), so the batchName segment (and its trailing "_")
# simply isn't there when batchName is NULL. A fixed "_preproc_qcsummary.tsv"
# suffix (with a required leading underscore) therefore only matches the
# batchName-present form and silently misses every batchName == NULL output --
# the naming form eyeQuality()'s single-file (non-batch) entry point produces
# by default. This app has no a-priori batchName to filter on -- unlike the
# Setup app's count_completed_qcsummary_files(), which polls a single known
# run -- so the pattern below deliberately leaves the "_desc-<batchName>_"
# portion unanchored (matching zero or more characters between "_desc-" and
# "preproc_qcsummary.tsv$", covering both naming forms), picking up every
# batch/run found under the chosen directory.
qcsummary_filename_pattern <- "_desc-.*preproc_qcsummary\\.tsv$"

# Human-readable rendering of qcsummary_filename_pattern for diagnostic
# messages -- the regex form above is right for list.files()'s pattern
# argument, but showing its escape characters (`\\.`, `$`) verbatim to a user
# reading a diagnostic panel is just noise.
qcsummary_filename_pattern_display <- "..._desc-...preproc_qcsummary.tsv"

# discover_qcsummary_files: find every qcsummary.tsv output under a directory,
# using qcsummary_filename_pattern. Recursive by default, since
# eyeQualityBatch()'s default outputDir = NULL writes each file's qcsummary
# output into a "derivatives/eyeQuality-v1/" subfolder nested under that
# file's own directory (see get_qcsummary_output_path()), not flat at the top
# level of a BIDS-like directory tree.
#
# Returns a character vector of full paths (possibly empty), sorted for
# stable, deterministic ordering across calls.
discover_qcsummary_files <- function(directory, recursive = TRUE) {
  if (!is.character(directory) || length(directory) != 1 || is.na(directory) || !nzchar(directory)) {
    stop("discover_qcsummary_files: 'directory' must be a non-empty single path")
  }
  if (!dir.exists(directory)) {
    stop("discover_qcsummary_files: directory does not exist: ", directory)
  }

  sort(list.files(
    directory,
    pattern = qcsummary_filename_pattern,
    recursive = recursive,
    full.names = TRUE
  ))
}

# derive_recording_label: strip the "_desc-<batchName>_preproc_qcsummary.tsv"
# (or "_desc-preproc_qcsummary.tsv") suffix off a qcsummary output's basename,
# leaving the BIDS-like recording identifier the file was generated from
# (e.g. "sub-01_ses-1_task-x_recording-eyetracking_physio") -- this is what
# lets a user tell participants/recordings apart in the combined table, since
# it directly reflects the original input filename rather than an opaque
# batch-internal id.
#
# Uses qcsummary_filename_pattern directly (rather than re-deriving a
# "_desc-.*" + fixed-suffix pattern locally) so this stays in lockstep with
# discover_qcsummary_files() -- anything discover_qcsummary_files() matches is
# guaranteed to have its "_desc-...preproc_qcsummary.tsv" suffix stripped
# here too, for either naming form.
#
# Returns a single character string.
derive_recording_label <- function(qcsummary_path) {
  sub(qcsummary_filename_pattern, "", basename(qcsummary_path))
}

# derive_batch_name: pull the batchName back out of a qcsummary output's
# filename (the "_desc-<batchName>_preproc_qcsummary.tsv" convention) --
# useful for telling apart multiple runs/batches over the same recordings
# that may coexist under one output directory (e.g. reprocessed with
# different parameters under a different batchName). Returns NA_character_
# for the batchName == NULL naming form ("_desc-preproc_qcsummary.tsv"),
# where there is no batch name to recover.
#
# Unlike derive_recording_label() above, this one's regex genuinely does need
# its own literal "_preproc_qcsummary\\.tsv$" (WITH the leading underscore),
# not qcsummary_filename_pattern's unanchored form -- that underscore is
# exactly what distinguishes the two naming forms here. saveFiles()'s
# qcsummarydesc only inserts a "_" between batchName and "preproc" when
# batchName is present (paste0(batchName, "_")); when batchName is NULL that
# separator never gets added, so "preproc" sits directly after "desc-" with no
# underscore ("_desc-preproc_qcsummary.tsv"). Requiring the underscore before
# "preproc" is therefore what correctly fails to match (falling through to the
# NA_character_ branch below) for the batchName-NULL form, while still
# capturing batchName correctly when it's present. Verified against real
# output from both eyeQuality() (batchName NULL) and eyeQualityBatch() (a real
# batchName) -- see the P10-01 follow-up fix for derive_recording_label(),
# which required the fix above precisely because its old regex made the
# opposite (incorrect) assumption.
derive_batch_name <- function(qcsummary_path) {
  m <- regmatches(
    basename(qcsummary_path),
    regexec("_desc-(.*)_preproc_qcsummary\\.tsv$", basename(qcsummary_path))
  )[[1]]
  if (length(m) < 2 || !nzchar(m[2])) {
    return(NA_character_)
  }
  m[2]
}

# build_zero_match_diagnostic: human-readable explanation for why
# discover_qcsummary_files() found nothing under a directory, in the same
# diagnostic spirit as the Setup app's zero-match handling (P7-06) -- this
# app's UI has no console for a user to read list.files()'s silence from, so
# a blank table needs an explicit reason attached instead. Distinguishes two
# cases: no .tsv files at all under the directory (likely the wrong
# directory entirely), vs. .tsv files present but none matching the
# qcsummary naming convention (likely raw/preproc files, but no completed
# batch run yet, or output written somewhere else via a custom outputDir).
#
# Returns a single character string.
build_zero_match_diagnostic <- function(directory, recursive) {
  all_tsv <- list.files(directory, pattern = "\\.tsv$", recursive = recursive, full.names = FALSE)

  scope <- if (isTRUE(recursive)) "directory (searched recursively)" else "directory (not searched recursively)"

  if (length(all_tsv) == 0) {
    sprintf(
      paste0(
        "No .tsv files of any kind were found under this %s. eyeQuality writes qcsummary ",
        "outputs matching \"%s\" under a \"derivatives/eyeQuality-v1/\" subfolder of each ",
        "processed file's directory by default (or under a custom outputDir, if one was used ",
        "when the batch was run). Double check this is the directory a batch run was pointed ",
        "at (or its outputDir), and that a batch run has completed."
      ),
      scope, qcsummary_filename_pattern_display
    )
  } else {
    sprintf(
      paste0(
        "Found %d .tsv file(s) under this %s, but none matched the qcsummary output naming ",
        "convention (\"%s\"). This usually means a batch run either hasn't completed yet, ",
        "or wrote its outputs (via a custom outputDir) somewhere other than this directory."
      ),
      length(all_tsv), scope, qcsummary_filename_pattern_display
    )
  }
}

# read_one_qcsummary: read a single qcsummary.tsv file and attach the
# identifying columns (recording, batch_name, source_file) a combined
# multi-file table needs to tell rows from different files apart.
# calculateOutputMetrics()'s output (see R/calculateOutputMetrics.R) is one
# row per qc_metric, not one row per file -- saveFiles() writes it with a
# leading "qc_metric" column plus whatever stat columns
# calculateOutputMetrics() computed (n, percent, group_mean, etc.), so each
# qcsummary.tsv already comes in a long, metric-per-row shape rather than a
# single-row summary. That shape is preserved here (no reshaping to one row
# per file), since collapsing ~12 stat columns x ~28 metrics per file into a
# single wide row per file would need an artificial pivot with no clear
# canonical column ordering, whereas the identifying columns added below are
# enough to filter/sort/compare across files while keeping every original
# qc_metric row and column intact and generic to whatever
# calculateOutputMetrics() happens to compute (including future
# adapter-specific columns, e.g. head-mounted metrics -- P10-06).
#
# Returns a data.frame, or raises an error (via readr's own parse failure)
# if the file isn't parseable as a delimited qcsummary.tsv.
read_one_qcsummary <- function(path) {
  qc <- readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
  qc$recording <- derive_recording_label(path)
  qc$batch_name <- derive_batch_name(path)
  qc$source_file <- path

  # Identifying columns first, then whatever calculateOutputMetrics() wrote
  # (led by "qc_metric"), so the combined table reads recording/batch/metric
  # left-to-right rather than burying them after a dozen stat columns.
  id_cols <- c("recording", "batch_name", "source_file")
  dplyr::relocate(qc, dplyr::all_of(id_cols), .before = 1)
}

# load_qcsummary_table: discover and load every qcsummary.tsv output under a
# directory into one combined table, for the Analyze/QC Explorer app's main
# table view.
#
# directory: top-level output directory to search (P1-06's outputDir
#   convention -- the same directory a batch run's outputDir pointed at, or
#   the raw data directory itself when outputDir was left NULL and outputs
#   landed in each file's own nested derivatives/eyeQuality-v1/ folder).
# recursive: search directory recursively (default TRUE) -- matches the
#   nested derivatives/eyeQuality-v1/ layout eyeQualityBatch()'s default
#   outputDir = NULL produces.
#
# Returns a list:
#   n_files: number of qcsummary.tsv files found
#   files: character vector of the file paths found (possibly empty)
#   table: combined data.frame (one row per qc_metric per file), or NULL if
#     n_files == 0
#   diagnostic_message: NULL, or a human-readable string explaining why zero
#     files were found (see build_zero_match_diagnostic())
#   read_errors: named character vector of per-file error messages for any
#     matched file that failed to parse (possibly empty) -- a malformed file
#     doesn't abort the whole load, it's just excluded from `table` and
#     surfaced here instead.
load_qcsummary_table <- function(directory, recursive = TRUE) {
  files <- discover_qcsummary_files(directory, recursive = recursive)

  if (length(files) == 0) {
    return(list(
      n_files = 0L,
      files = files,
      table = NULL,
      diagnostic_message = build_zero_match_diagnostic(directory, recursive),
      read_errors = character(0)
    ))
  }

  read_errors <- character(0)
  rows <- list()
  for (f in files) {
    result <- tryCatch(
      list(ok = TRUE, value = read_one_qcsummary(f)),
      error = function(e) list(ok = FALSE, value = conditionMessage(e))
    )
    if (result$ok) {
      rows[[f]] <- result$value
    } else {
      read_errors[f] <- result$value
    }
  }

  # dplyr::bind_rows() (not rbind()) deliberately: tolerates files whose
  # columns don't exactly match (e.g. different eyeQuality versions, or
  # future adapter-specific qc_metric columns -- P10-06), filling NA for any
  # column missing from a given file rather than erroring the whole load.
  combined <- if (length(rows) > 0) dplyr::bind_rows(rows) else NULL

  list(
    n_files = length(files),
    files = files,
    table = combined,
    diagnostic_message = NULL,
    read_errors = read_errors
  )
}

# resolve_preproc_data_path: given a qcsummary.tsv output's full path (as
# stored in the combined table's source_file column -- see
# read_one_qcsummary() above), return the full path of its sibling
# *_preproc.tsv data file: the full per-sample preprocessed data saveFiles()
# writes alongside qcsummary.tsv (see R/saveFiles.R), and the actual input
# generateEyeTrackingPlots() (R/generateEyeTrackingPlots.R) needs -- a
# per-timestamp data.frame with gazeLeftX/gazeRightX/pupilLeft/etc. and
# gazeX.preprocessed_px/gazeY.preprocessed_px columns, not
# calculateOutputMetrics()'s one-row-per-qc_metric summary table qcsummary.tsv
# itself holds.
#
# saveFiles() builds both filenames off the same "_desc-<batchName>_preproc"
# stem (its local `preprocdesc`), with qcsummary.tsv's name just appending
# "_qcsummary" onto that same stem before the extension (its local
# `qcsummarydesc` is literally `paste0(preprocdesc-equivalent, "_qcsummary")`)
# -- so the sibling preproc data file is recovered by stripping the trailing
# "_qcsummary" immediately before ".tsv". Verified against a real
# eyeQualityBatch() run's actual output directory for P10-03 (both the
# batchName-present and batchName-NULL naming forms produce this same
# stem/suffix relationship), not just inferred from reading saveFiles()'s
# source.
#
# Returns a single character string. The returned path may not exist on
# disk (e.g. someone deleted or moved it after the batch run completed) --
# callers should check file.exists() themselves; see load_plot_data() below,
# which does exactly that.
resolve_preproc_data_path <- function(qcsummary_path) {
  sub("_qcsummary\\.tsv$", ".tsv", qcsummary_path)
}

# load_plot_data: resolve a selected qc_table row's source_file (a
# qcsummary.tsv path) to its sibling preproc data file, load that file, and
# generate its diagnostic plots via generateEyeTrackingPlots() -- reused
# directly from R/generateEyeTrackingPlots.R, not reimplemented here, per
# this app's scope (P10-03).
#
# Handles two failure modes gracefully instead of letting either crash the
# app:
#   - the missing-sibling-file case (the *_preproc.tsv was deleted, moved, or
#     renamed since the batch run completed, while its *_qcsummary.tsv sat
#     untouched)
#   - any read/plot failure once the file is found (e.g. a preproc file from
#     a different adapter/geometry missing a column
#     generateEyeTrackingPlots() expects -- see P10-06)
#
# Returns a list:
#   ok: TRUE/FALSE
#   preproc_path: the resolved sibling path (always populated, even when ok
#     == FALSE, so callers can surface it in an error message)
#   data: the loaded data.frame (only when ok == TRUE)
#   plots: the list returned by generateEyeTrackingPlots() (only when ok ==
#     TRUE)
#   error: human-readable string (only when ok == FALSE)
load_plot_data <- function(qcsummary_path) {
  preproc_path <- resolve_preproc_data_path(qcsummary_path)

  if (!file.exists(preproc_path)) {
    return(list(
      ok = FALSE,
      preproc_path = preproc_path,
      error = sprintf(
        paste0(
          "The preprocessed data file this row's plots depend on is missing: %s. ",
          "It should sit alongside %s (the qcsummary.tsv this row was loaded from) ",
          "in the same derivatives/eyeQuality-v1/ folder -- it may have been moved, ",
          "renamed, or deleted since the batch run completed."
        ),
        preproc_path, basename(qcsummary_path)
      )
    ))
  }

  tryCatch(
    {
      data <- readr::read_tsv(preproc_path, show_col_types = FALSE, progress = FALSE)
      plots <- generateEyeTrackingPlots(data)
      list(ok = TRUE, preproc_path = preproc_path, data = data, plots = plots)
    },
    error = function(e) {
      list(
        ok = FALSE,
        preproc_path = preproc_path,
        error = sprintf(
          "Failed to load or plot %s: %s",
          preproc_path, conditionMessage(e)
        )
      )
    }
  )
}
