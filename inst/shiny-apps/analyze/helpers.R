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

# ---------------------------------------------------------------------------
# P10-02: configurable QC thresholds with visual flagging
# ---------------------------------------------------------------------------

# qc_threshold_config: the fixed set of qc_metric rows this app treats as
# sensible pass/fail thresholding candidates, out of the ~32 rows
# calculateOutputMetrics() (R/calculateOutputMetrics.R) writes per file.
#
# Deliberately NOT every metric -- most of the 32 rows are descriptive
# (eye-selection source, smoothing magnitude, fixation/saccade/blink
# duration) rather than universal pass/fail signals: they characterize *how*
# the pipeline processed a recording, not whether the recording is good or
# bad, and several (e.g. blink rate, fixation duration) have no single
# "more/less is better" direction at all -- a high blink rate isn't
# inherently a QC failure the way a low percentage of valid data is. The
# three chosen here are the ones with an unambiguous, universal direction
# and a direct bearing on "is this recording usable":
#   - valid_raw_data: the most basic gate -- what fraction of all samples
#     had any raw gaze data at all (higher is better).
#   - robustness_proportion_valid_data_to_all_data: calculateOutputMetrics()
#     itself names this with a "robustness_" prefix, and it's a distinct
#     signal from valid_raw_data -- it's computed post-IVT-classification
#     (fixation + saccade + unclassified samples, i.e. excluding blinks and
#     IVT-flagged missing time, as a fraction of all samples), so it
#     captures data quality *after* accounting for blinks/classification
#     rather than raw non-NA-ness (higher is better).
#   - interpolated_LeftEye / interpolated_RightEye: the fraction of final
#     (post-eye-selection) samples that needed gap-filling via
#     interpolation -- heavy interpolation is a classic eye-tracking QC
#     concern independent of whether the underlying raw/robustness
#     percentages already look fine (lower is better). Left and right share
#     one threshold_id/UI control below since they're the same underlying
#     concern split by eye, and a QC reviewer thinks of "how much
#     interpolation" as one question, not two.
# Explicitly NOT included (with reasoning, so this isn't just an oversight):
#   - missing_raw_data_BothEyes / final_na are exact complements of
#     valid_raw_data / final_valid (percent values sum to 1 by construction
#     in calculateOutputMetrics()) -- thresholding both directions of the
#     same underlying quantity would just double the UI for no new
#     information.
#   - blinks_*, eye_select_*, smoothed_*, ivt_fixations/saccades/
#     unclassified/missing (as raw classification proportions rather than
#     the robustness rollup above), and the duration-based
#     robustness_fixation_duration row are descriptive/paradigm-dependent
#     (e.g. what counts as a "normal" blink rate or fixation duration
#     varies by task) rather than metrics with an obvious universal
#     pass/fail cutoff -- flagging them by default would be asserting a
#     QC opinion this app has no basis for.
#
# Columns:
#   threshold_id: the UI input/reactive key this row's flagging is
#     controlled by. Multiple qc_metric rows can share one threshold_id
#     (interpolated_LeftEye/RightEye above) when they represent the same
#     underlying QC question.
#   qc_metric: the exact value in the table's qc_metric column this row
#     applies to.
#   label: UI label shown next to the threshold input.
#   direction: "min" (flag when the metric's value is BELOW the threshold --
#     for higher-is-better metrics) or "max" (flag when ABOVE -- for
#     lower-is-better metrics).
#   default_percent: default threshold, as a 0-100 percentage (matching the
#     numericInput UI unit) -- converted to the table's underlying 0-1
#     fraction (see calculateOutputMetrics()'s "percent" column, which is a
#     raw ratio, not multiplied by 100) at comparison time.
qc_threshold_config <- data.frame(
  threshold_id = c("valid_pct", "robust_pct", "interp_pct", "interp_pct"),
  qc_metric = c(
    "valid_raw_data",
    "robustness_proportion_valid_data_to_all_data",
    "interpolated_LeftEye",
    "interpolated_RightEye"
  ),
  label = c(
    "Minimum % valid raw data",
    "Minimum % robust (classifiable) data",
    "Maximum % interpolated data (either eye)",
    "Maximum % interpolated data (either eye)"
  ),
  direction = c("min", "min", "max", "max"),
  default_percent = c(80, 80, 20, 20),
  stringsAsFactors = FALSE
)

# default_qc_thresholds: qc_threshold_config's default_percent values,
# collapsed to one entry per threshold_id and converted from a 0-100
# percentage to the 0-1 fraction the table's "percent" column actually uses
# -- the form compute_qc_flags() expects. Used both to seed the UI's
# numericInput default values and as the fallback when an input is
# temporarily NULL/NA (e.g. mid-typing in the UI).
#
# Returns a named list, one entry per unique threshold_id.
default_qc_thresholds <- function() {
  unique_cfg <- qc_threshold_config[!duplicated(qc_threshold_config$threshold_id), ]
  stats::setNames(as.list(unique_cfg$default_percent / 100), unique_cfg$threshold_id)
}

# compute_qc_flags: given the combined qcsummary table (one row per
# qc_metric per file, as built by load_qcsummary_table()) and a set of
# current threshold values, return a logical vector (same length and row
# order as `table`) marking which rows cross their configured threshold.
#
# Row-level, not a per-recording rollup, deliberately: the table is already
# long-format (one row per metric per file, per P10-01's design -- see
# read_one_qcsummary()'s comment on why that shape was kept rather than
# pivoted), and a QC reviewer scanning the table wants to see exactly which
# metric(s) tripped the threshold for a given recording, not just a single
# opaque pass/fail badge that would need drilling back into the long table
# to explain. Flagging the specific row(s) that crossed keeps that
# information visible without needing a second, reshaped table. A
# per-recording rollup (e.g. "does this recording have >=1 flagged metric")
# is still one line away from this vector -- see the "flagged_recordings"
# reactive in app.R, built for P10-05's export feature to consume -- so nothing
# here forecloses that view; it's just not the table's own row unit.
#
# Rows whose qc_metric isn't one of qc_threshold_config's configured metrics
# are never flagged. Rows with an NA value for the relevant column (e.g. a
# future adapter that doesn't populate "percent" for that metric -- see
# P10-06) are also never flagged, rather than propagating NA into the
# comparison and erroring/blanking DT's styling.
#
# table: a data.frame with at least "qc_metric" and "percent" columns (as
#   produced by read_one_qcsummary()/load_qcsummary_table()). NULL, 0-row,
#   or missing either column returns an all-FALSE (or length-0) vector
#   rather than erroring.
# thresholds: a named list/vector of 0-1 fractions, keyed by threshold_id
#   (see default_qc_thresholds() for the expected shape). A missing or
#   NA/NULL entry for a given threshold_id skips flagging for that
#   threshold_id's metric(s) entirely (falls through, no comparison made)
#   rather than guessing a default -- callers (app.R) are expected to have
#   already substituted default_qc_thresholds() for any blank UI input
#   before calling this.
#
# Returns a logical vector.
compute_qc_flags <- function(table, thresholds) {
  if (is.null(table)) {
    return(logical(0))
  }
  n <- nrow(table)
  if (n == 0 || !all(c("qc_metric", "percent") %in% names(table))) {
    return(rep(FALSE, n))
  }

  flagged <- rep(FALSE, n)
  for (i in seq_len(nrow(qc_threshold_config))) {
    cfg <- qc_threshold_config[i, ]
    threshold_value <- thresholds[[cfg$threshold_id]]
    if (is.null(threshold_value) || is.na(threshold_value)) {
      next
    }

    rows <- which(table$qc_metric == cfg$qc_metric)
    if (length(rows) == 0) {
      next
    }

    values <- table$percent[rows]
    crosses <- if (identical(cfg$direction, "min")) {
      !is.na(values) & values < threshold_value
    } else {
      !is.na(values) & values > threshold_value
    }
    flagged[rows] <- flagged[rows] | crosses
  }
  flagged
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

# ---------------------------------------------------------------------------
# P10-07: save/load QC thresholds via the same batch_config.yaml the Setup
# app (P9-04) reads/writes -- one shared config file per study covering both
# run parameters and QC thresholds, not a second, fragmented config format.
# ---------------------------------------------------------------------------

# recognized_qc_threshold_ids: qc_threshold_config's unique threshold_id
# values -- the single source of truth for which qcThresholds keys this app
# (and R/batchConfig.R's validate_batch_config(), which sources this exact
# file via .known_qc_threshold_ids() to build the same list) recognizes. A
# thin wrapper around qc_threshold_config (defined above) so nothing here
# duplicates that definition.
recognized_qc_threshold_ids <- function() {
  unique(qc_threshold_config$threshold_id)
}

# blank_to_null / null_to_blank: the same "blank field <-> NULL" convention
# the Setup app's helpers.R (P9-04) uses for its own optional text/numeric
# fields. Duplicated here rather than shared -- each app's helpers.R is
# already a self-contained, independently sourced file (see this file's own
# header comment), and these two converters are simple enough that
# duplication carries no real drift risk.
blank_to_null <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (length(x) == 1 && is.na(x)) {
    return(NULL)
  }
  if (is.character(x) && length(x) == 1 && !nzchar(trimws(x))) {
    return(NULL)
  }
  x
}

null_to_blank <- function(x) {
  if (is.null(x)) "" else x
}

# qc_thresholds_to_percent: convert a named list of 0-1 fractions keyed by
# threshold_id (the shape app.R's qc_thresholds() reactive produces, and
# compute_qc_flags() consumes) to the 0-100 percentage scale
# batch_config.yaml's qcThresholds section is stored on -- the same unit
# qc_threshold_config$default_percent and the numericInput controls
# themselves use, so a saved config's numbers read back exactly what a user
# typed rather than an internal fraction.
qc_thresholds_to_percent <- function(thresholds) {
  stats::setNames(
    lapply(thresholds, function(v) if (is.null(v) || is.na(v)) NA_real_ else v * 100),
    names(thresholds)
  )
}

# filter_recognized_qc_thresholds: drop any qcThresholds entry (as read from
# a batch_config.yaml, percent-scale) whose key isn't a currently recognized
# threshold_id, or whose value isn't a sane 0-100 percentage -- e.g. a config
# hand-edited with a typo, or written by a future eyeQuality version that
# supports a QC metric this version doesn't (P10-07's forward-compatibility
# requirement).
#
# Unlike R/batchConfig.R's validate_batch_config(), which treats either
# problem as a hard error (the right behavior when a config is about to be
# programmatically relied on or explicitly re-saved as-is), the Analyze
# app's own "Load config" flow needs to tolerate this instead: a stray or
# future threshold entry in an otherwise-fine batch_config.yaml shouldn't
# block loading the rest of that file (its run parameters, carried forward
# via app.R's loaded_config_extra, or its still-recognized threshold
# entries) into the app. That's why app.R's load handler reads via
# read_batch_config(path, validate = FALSE) and calls this function itself,
# rather than relying on validate_batch_config() to have already screened
# qcThresholds.
#
# qcThresholds: NULL, or a named list/vector as read from a batch_config.yaml
#   (percent-scale, per validate_batch_config()'s convention). A config with
#   no qcThresholds section at all (every config written before this field
#   existed, or one written by the Setup app alone) reads back as NULL here
#   too, via read_batch_config()'s default-filling -- handled the same as an
#   empty list, not an error.
#
# Returns a list: kept (named list of only the recognized, sane-valued
# entries, possibly empty), dropped (character vector of the entry names
# that were dropped, possibly empty).
filter_recognized_qc_thresholds <- function(qcThresholds) {
  if (is.null(qcThresholds) || length(qcThresholds) == 0) {
    return(list(kept = list(), dropped = character(0)))
  }

  known_ids <- recognized_qc_threshold_ids()
  is_sane_percent <- function(v) {
    is.numeric(v) && length(v) == 1 && !is.na(v) && v >= 0 && v <= 100
  }
  entry_names <- names(qcThresholds)
  if (is.null(entry_names)) {
    entry_names <- rep("", length(qcThresholds))
  }
  keep_flags <- vapply(seq_along(qcThresholds), function(i) {
    nzchar(entry_names[i]) && entry_names[i] %in% known_ids && is_sane_percent(qcThresholds[[i]])
  }, logical(1))

  list(
    kept = qcThresholds[keep_flags],
    dropped = entry_names[!keep_flags]
  )
}

# ---------------------------------------------------------------------------
# P10-04: cross-file QC metric comparison view
# ---------------------------------------------------------------------------
#
# P10-03's row-click plot view answers "what does this one file's data look
# like", and the QC table itself (P10-01/P10-02) is already sortable/
# filterable/flagged, but neither makes it easy to eyeball how one qc_metric
# stacks up *across* every loaded file at a glance, or to line several
# metrics up side by side per file. The two helpers below back a new
# "Compare files" tab in app.R that does exactly that: a per-metric bar chart
# across recordings (build_qc_comparison_plot()), and a wide, one-row-per-
# file comparison table for a chosen set of metrics (build_qc_comparison_table()).
#
# Both are read-only views over the same qc_threshold_config/compute_qc_flags()
# machinery P10-02 already built -- neither reimplements threshold direction
# logic; build_qc_comparison_plot() in particular is deliberately handed the
# already-flagged table (qc_table_flagged() in app.R) rather than recomputing
# pass/fail itself, so this view can never drift from the QC table's own
# flagging.

# build_qc_comparison_plot: a horizontal bar chart of one qc_metric's value
# across every loaded recording, for spotting outlier files at a glance
# (rather than scanning the long QC table row by row). Reuses ggplot2 (already
# a package Import, and already the plotting library
# generateEyeTrackingPlots() depends on -- see R/generateEyeTrackingPlots.R)
# rather than introducing a new plotting dependency for this one view.
#
# table_flagged: the combined qcsummary table with a "qc_flag" column already
#   attached (app.R's qc_table_flagged() reactive, i.e. load_result()$table
#   run through compute_qc_flags()) -- reused directly here so bar coloring
#   always matches the QC table's own row highlighting for the same metric,
#   rather than this function re-deriving pass/fail itself.
# metric: a single qc_metric value to plot (one of table_flagged$qc_metric).
# thresholds: the current threshold values (app.R's qc_thresholds() reactive,
#   0-1 fractions keyed by threshold_id) -- used only to position the dashed
#   reference line at the metric's live threshold, when one is configured;
#   the bar coloring itself comes from table_flagged$qc_flag, not from
#   recomputing against `thresholds` here.
#
# Returns a ggplot object, or NULL if `metric` has no rows in table_flagged
# (e.g. a stale selection left over from a previous, since-reloaded table).
build_qc_comparison_plot <- function(table_flagged, metric, thresholds) {
  if (is.null(table_flagged) || is.null(metric) || !nzchar(metric)) {
    return(NULL)
  }
  rows <- table_flagged[table_flagged$qc_metric == metric, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }

  cfg_row <- qc_threshold_config[qc_threshold_config$qc_metric == metric, , drop = FALSE]
  has_threshold <- nrow(cfg_row) == 1

  # Status is read straight off table_flagged's own qc_flag column (see
  # header comment above) for any metric that has a configured threshold; a
  # metric with no configured threshold (most of calculateOutputMetrics()'s
  # ~32 rows -- see qc_threshold_config's own comment on why only 3 are
  # thresholdable) gets a distinct third status rather than being
  # miscategorized as "OK", since compute_qc_flags() never sets qc_flag TRUE
  # for an unconfigured metric.
  rows$comparison_status <- if (has_threshold) {
    ifelse(rows$qc_flag, "Flagged", "OK")
  } else {
    "No threshold configured"
  }
  rows$comparison_status <- factor(
    rows$comparison_status,
    levels = c("Flagged", "OK", "No threshold configured")
  )

  # bar_label: recording alone is NOT a unique file identity -- it's derived
  # by stripping the _desc-<batchName>_ segment off the filename (see
  # derive_recording_label()), so the same subject/session reprocessed under
  # two different batchNames (e.g. re-run with different parameters, or
  # genuinely repeated recordings) produces two rows with the IDENTICAL
  # recording value. Plotting x = recording alone let ggplot2::geom_col()'s
  # default position = "stack" silently sum those rows' percent values into
  # one bar -- a real correctness bug found in field testing (a "150%
  # robustness" value that's only possible as the sum of two runs, not a
  # single file's true value). Appending batch_name whenever it's genuinely
  # ambiguous (>1 distinct batch_name sharing the same recording) keeps each
  # row on its own bar and makes which run produced which bar explicit,
  # rather than just deduplicating labels cosmetically.
  ambiguous_recordings <- names(which(tapply(
    rows$batch_name, rows$recording, function(b) length(unique(b))
  ) > 1))
  rows$bar_label <- ifelse(
    rows$recording %in% ambiguous_recordings,
    paste0(rows$recording, " [", ifelse(is.na(rows$batch_name), "NA", rows$batch_name), "]"),
    rows$recording
  )

  # Sorted (not left in table row order) so outlier files land visibly at
  # either end of the chart rather than scattered through an arbitrary
  # file-discovery order -- coord_flip() below then reads the sorted axis
  # top-to-bottom, with recording labels legible instead of overlapping
  # x-axis text.
  plot <- ggplot2::ggplot(
    rows,
    ggplot2::aes(
      x = stats::reorder(bar_label, percent),
      y = percent,
      fill = comparison_status
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        "Flagged" = "#c0392b",
        "OK" = "#27ae60",
        "No threshold configured" = "#7f8c8d"
      ),
      drop = FALSE
    ) +
    # percent's underlying values are 0-1 fractions (see
    # calculateOutputMetrics.R), not already scaled to 0-100 -- labeled as a
    # percentage here purely for axis display, matching the numericInput
    # thresholds' own 0-100 unit.
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::labs(
      x = NULL,
      y = metric,
      fill = "QC status",
      title = paste("Across-file comparison:", metric)
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    # Legibility at real-world scale (found too small in field testing):
    # base_size above scales most text, but axis tick labels (the file/
    # recording names on the flipped y-axis -- the actual "which file is
    # this" text a reviewer reads) and the title need bumping past what
    # base_size alone gives them.
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 13),
      axis.title = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      legend.text = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14)
    )

  if (has_threshold) {
    threshold_value <- thresholds[[cfg_row$threshold_id[1]]]
    if (!is.null(threshold_value) && !is.na(threshold_value)) {
      plot <- plot + ggplot2::geom_hline(
        yintercept = threshold_value, linetype = "dashed", color = "black"
      )
    }
  }

  plot
}

# build_qc_comparison_table: reshape the combined long qcsummary table (one
# row per qc_metric per file) to wide -- one row per file, one column per
# selected qc_metric -- so a user can line several metrics up side by side
# per recording, rather than only viewing one metric at a time (the bar chart
# above) or scrolling through the long table's every metric row per file.
#
# table: the combined qcsummary table (load_result()$table -- pre- or
#   post-qc_flag, this function only reads qc_metric/percent so either works,
#   but app.R passes the plain, un-flagged table since qc_flag isn't a
#   per-metric-column concept in wide form -- see app.R's compare_table
#   render for how threshold crossing is instead shown per output column via
#   DT::formatStyle()).
# metrics: character vector of qc_metric values to include as columns.
#
# Returns a data.frame with recording/batch_name/source_file plus one column
# per (present) entry of `metrics`, or NULL if `table` is empty/NULL, or none
# of `metrics` has any matching rows.
build_qc_comparison_table <- function(table, metrics) {
  if (is.null(table) || nrow(table) == 0 || length(metrics) == 0) {
    return(NULL)
  }
  if (!all(c("recording", "batch_name", "source_file", "qc_metric", "percent") %in% names(table))) {
    return(NULL)
  }

  subset_tbl <- table[table$qc_metric %in% metrics, c("recording", "batch_name", "source_file", "qc_metric", "percent")]
  if (nrow(subset_tbl) == 0) {
    return(NULL)
  }

  # values_fn = first-value-wins rather than the default list-column
  # behavior: guards against a (theoretically possible, e.g. a hand-edited
  # qcsummary.tsv with a duplicated qc_metric row) duplicate id/metric
  # combination producing a nested list-cell DT can't render, at the cost of
  # silently keeping just the first such row -- an edge case worth not
  # crashing on rather than one this table is trying to detect/report.
  tidyr::pivot_wider(
    subset_tbl,
    id_cols = c("recording", "batch_name", "source_file"),
    names_from = "qc_metric",
    values_from = "percent",
    values_fn = function(x) x[1]
  )
}

# --- P10-05: export "flagged for review" file list ---
#
# build_flagged_export_table: per-recording rollup of the currently flagged
# rows in table_flagged, shaped for a triage handoff -- a colleague opening
# the exported CSV can see which recordings were flagged, by how many
# metrics, and which metrics/values tripped them, without cross-referencing
# the full long QC table. Deliberately "flagged by any configured threshold"
# rather than a single metric -- see app.R's download handler for why that's
# the more useful default for an exclusion/re-review list.
#
# Reuses table_flagged's own qc_flag column (compute_qc_flags(), the same
# source of truth every other P10-02/P10-04 view already reads) rather than
# recomputing threshold-crossing logic a third time -- this function only
# filters and reshapes rows compute_qc_flags() already flagged. It also
# doesn't duplicate app.R's own flagged_recordings() reactive (a distinct-
# recordings list with no per-metric detail, built as a P10-05 hook back in
# P10-02) -- that reactive is a fine "how many recordings" summary, but this
# function's row-per-recording-with-detail shape is what actually belongs in
# an exported file, so it's built directly off table_flagged instead of
# wrapping that narrower reactive.
#
# table_flagged: the combined qcsummary table with a "qc_flag" column already
#   attached (app.R's qc_table_flagged() reactive). NULL, 0-row, or missing
#   any of the required columns returns NULL (nothing to export).
#
# Returns a data.frame with one row per recording that has >=1 flagged
# qc_metric row, columns:
#   recording, batch_name, source_file: the same identifying columns every
#     other view in this app keys on.
#   n_flagged_metrics: count of flagged qc_metric rows for that recording.
#   flagged_metrics: comma-separated qc_metric names that were flagged, so a
#     reviewer opening the CSV can see *why* without reopening this app.
#   flagged_values: comma-separated percent values, converted to the 0-100
#     scale the sidebar's numericInput thresholds already use (matching
#     qc_thresholds_to_percent()'s convention), in the same order as
#     flagged_metrics.
# Or NULL if table_flagged has no usable rows, or none are currently flagged.
build_flagged_export_table <- function(table_flagged) {
  required_cols <- c("recording", "batch_name", "source_file", "qc_metric", "percent", "qc_flag")
  if (is.null(table_flagged) || nrow(table_flagged) == 0 || !all(required_cols %in% names(table_flagged))) {
    return(NULL)
  }

  flagged_rows <- table_flagged[table_flagged$qc_flag, required_cols, drop = FALSE]
  if (nrow(flagged_rows) == 0) {
    return(NULL)
  }

  flagged_rows %>%
    dplyr::group_by(recording, batch_name, source_file) %>%
    dplyr::summarise(
      n_flagged_metrics = dplyr::n(),
      flagged_metrics = paste(qc_metric, collapse = ", "),
      flagged_values = paste(round(percent * 100, 1), collapse = ", "),
      .groups = "drop"
    ) %>%
    as.data.frame()
}
