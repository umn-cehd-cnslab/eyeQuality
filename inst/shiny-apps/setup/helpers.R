# Setup app helper functions.
#
# Kept separate from app.R and free of Shiny-specific code (no reactives, no
# input/output objects) so build_dry_run_preview() can be sourced and called
# directly against a real directory outside of a running Shiny session, for
# testing or scripting.

# build_dry_run_preview: run listBidsFiles() against a candidate directory
# and summarize the result the way the Setup app's preview panel needs --
# matched file count, a small sample of matched filenames, and the
# skipped-item count/detail (attr(result, "skipped"), see listBidsFiles()).
#
# directory: path to the top-level data directory to scan
# layout: "bids" or "glob", passed through to listBidsFiles()
# subjectPattern_regex, sessionPattern_regex, recursiveSearch: bids-layout
#   options, passed through to listBidsFiles() when layout == "bids"
# pathPattern, excludePattern_regex: glob-layout options, passed through to
#   listBidsFiles() when layout == "glob"
# modalityPattern_regex: used in either layout mode
# sample_n: how many sample filenames to include in the returned preview
#
# Returns a list: matched_count, matched_files (full paths), sample_files
# (basenames only, length <= sample_n), skipped_count, skipped_items (full
# detail of what was skipped, e.g. skipped directories or excluded paths),
# and diagnostic_message (NULL, or a single human-readable string when
# matched_count == 0 explaining *why* -- e.g. "found N subfolders but none
# matched subjectPattern_regex" -- since the app's UI has no console for a
# user to read listBidsFiles()'s print()/message() diagnostics from; see
# attr(matched, "diagnostics") on listBidsFiles()'s return value).
build_dry_run_preview <- function(directory,
                                  layout = "bids",
                                  subjectPattern_regex = "sub-[A-Z0-9]+",
                                  sessionPattern_regex = "ses-[0-9]+",
                                  modalityPattern_regex = NULL,
                                  recursiveSearch = FALSE,
                                  pathPattern = NULL,
                                  excludePattern_regex = NULL,
                                  sample_n = 10) {
  if (!is.character(directory) || length(directory) != 1 || is.na(directory) || !nzchar(directory)) {
    stop("build_dry_run_preview: 'directory' must be a non-empty single path")
  }
  if (!dir.exists(directory)) {
    stop("build_dry_run_preview: directory does not exist: ", directory)
  }
  if (!layout %in% c("bids", "glob")) {
    stop("build_dry_run_preview: 'layout' must be 'bids' or 'glob'")
  }

  # NOTE (P7-06): the bids-layout options below are built into a single
  # list(...) call, not assembled incrementally via `args$name <- value`.
  # `args$name <- NULL` *removes* that element from the list rather than
  # setting it to NULL, so an incremental build silently drops a NULL
  # subjectPattern_regex/sessionPattern_regex -- meaning do.call() below
  # would fall back to listBidsFiles()'s own non-NULL defaults
  # ("sub-[A-Z0-9]+"/"ses-[0-9]+") instead of actually passing NULL through.
  # That defeats the UI's own "blank = every subfolder" hint text: a user
  # blanking the field would see the preview keep failing with no clue why.
  # A single list(...) call preserves NULL-valued elements correctly.
  args <- if (identical(layout, "bids")) {
    list(
      directory = directory,
      layout = layout,
      modalityPattern_regex = modalityPattern_regex,
      subjectPattern_regex = subjectPattern_regex,
      sessionPattern_regex = sessionPattern_regex,
      recursiveSearch = recursiveSearch
    )
  } else {
    list(
      directory = directory,
      layout = layout,
      modalityPattern_regex = modalityPattern_regex,
      pathPattern = pathPattern,
      excludePattern_regex = excludePattern_regex
    )
  }

  matched <- do.call(eyeQuality::listBidsFiles, args)
  skipped <- attr(matched, "skipped")
  if (is.null(skipped)) {
    skipped <- character(0)
  }
  diagnostics <- attr(matched, "diagnostics")

  list(
    matched_count = length(matched),
    matched_files = as.character(matched),
    sample_files = utils::head(basename(as.character(matched)), sample_n),
    skipped_count = length(skipped),
    skipped_items = as.character(skipped),
    diagnostic_message = if (is.null(diagnostics)) NULL else diagnostics$hint
  )
}

# --- P9-04: save/load named batch_config.yaml files -------------------------

# blank_to_null: the Setup form's convention for "not specified" is a blank
# text field or an emptied numericInput (which Shiny reports as NA, not
# missing) -- both should become NULL when building a batch_config.yaml
# (R/batchConfig.R), matching what listBidsFiles()/eyeQuality() themselves
# treat as "use the default" for these optional fields. A length-0 value
# (e.g. an as-yet-unset input) is treated the same way.
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

# null_to_blank: the inverse conversion, for repopulating a text input from a
# loaded config's NULL-valued optional field -- updateTextInput()'s `value`
# must be a character string, not NULL.
null_to_blank <- function(x) {
  if (is.null(x)) "" else x
}

# build_batch_config_from_form: assemble a batch_config.yaml-shaped named
# list (see R/batchConfig.R) from the Setup app's current form input values,
# for the "Save config" control (P9-04).
#
# The form doesn't expose every field batch_config.yaml's schema supports --
# notably `adapterType` (P9-02's device selector is the dedicated task for
# that; adding it here would duplicate/preempt that work) and `numberCores`
# (deliberately kept out of the launch form itself, see background_run.R's
# note on the GUI's conservative hardcoded default). Rather than dropping
# those fields on every save, `extra` carries forward whatever config was
# most recently loaded this session (see app.R's `loaded_config_extra`) --
# its fields are the starting point, then every field the form DOES control
# is overlaid on top (so the form's current value always wins for the fields
# it owns, even when that value is NULL/blank).
#
# `numberCores` is handled as a special case rather than a plain form field:
# if `extra` already has a `numberCores` (i.e. this session loaded a config
# that set one), that value is preserved untouched; otherwise
# `defaultNumberCores` is used, so a freshly-saved config (never loaded from
# a file) still records the actual core count the "Start batch run" button
# would use (see app.R), for the same reproducibility purpose the plan calls
# for -- without adding a numberCores input to the launch form itself.
#
# extra: NULL, or a full config list (typically from `read_batch_config()`)
#   whose fields the form doesn't itself control should be preserved through
#   this save.
# defaultNumberCores: value to record for `numberCores` when `extra` doesn't
#   already have one.
#
# Returns a named list suitable for `write_batch_config()`; does not fill in
# schema defaults or validate -- that's `write_batch_config()`'s job.
build_batch_config_from_form <- function(directoryBIDS,
                                          batchName,
                                          layout,
                                          subjectPattern_regex,
                                          sessionPattern_regex,
                                          recursiveSearch,
                                          pathPattern,
                                          excludePattern_regex,
                                          modalityPattern_regex,
                                          displayDimensionX_mm,
                                          displayDimensionY_mm,
                                          outputDir,
                                          validityThreshold,
                                          eyeSelection_method,
                                          extra = NULL,
                                          defaultNumberCores = NULL) {
  extra <- if (is.null(extra)) list() else extra

  numberCores <- if (!is.null(extra[["numberCores"]])) extra[["numberCores"]] else defaultNumberCores

  form_fields <- list(
    directoryBIDS = blank_to_null(directoryBIDS),
    batchName = blank_to_null(batchName),
    layout = layout,
    subjectPattern_regex = blank_to_null(subjectPattern_regex),
    sessionPattern_regex = blank_to_null(sessionPattern_regex),
    recursiveSearch = isTRUE(recursiveSearch),
    pathPattern = blank_to_null(pathPattern),
    excludePattern_regex = blank_to_null(excludePattern_regex),
    modalityPattern_regex = blank_to_null(modalityPattern_regex),
    displayDimensionX_mm = blank_to_null(displayDimensionX_mm),
    displayDimensionY_mm = blank_to_null(displayDimensionY_mm),
    outputDir = blank_to_null(outputDir),
    validityThreshold = blank_to_null(validityThreshold),
    eyeSelection_method = eyeSelection_method,
    numberCores = numberCores
  )

  # keep.null = TRUE: a form field explicitly set to NULL (e.g. a blanked
  # outputDir) must overwrite whatever `extra` had for that field, not be
  # skipped -- see write_batch_config()'s own use of modifyList() for the
  # same reasoning.
  utils::modifyList(extra, form_fields, keep.null = TRUE)
}

# --- P9-07: post-run summary linking to the Analyze app --------------------
#
# The Setup and Analyze apps are two separately-launched local Shiny
# processes (each its own shiny::runApp() call), not tabs of one app -- a
# running Shiny session can't safely launch a second, separate Shiny process
# for itself. So "linking" here means: once a batch run finishes, tell the
# user exactly where its outputs landed and hand them a ready-to-run
# runAnalyzeApp() call (see R/runAnalyzeApp.R's `initialDirectory` argument)
# to paste into a separate R console/session to review them, rather than
# attempting any in-process app-to-app handoff.

# resolve_analyze_directory: which directory the Analyze app should be
# pointed at once a batch run finishes -- outputDir if the run used one, else
# the run's own directoryBIDS, since eyeQualityBatch()'s default outputDir =
# NULL writes each file's output into that file's own nested
# derivatives/eyeQuality-v1/ subfolder underneath directoryBIDS rather than
# one central location (same directoryBIDS-vs-outputDir convention
# background_run.R's poll_batch_progress() already resolves against for its
# own progress polling). The Analyze app's own load_qcsummary_table()
# defaults to a recursive search, so either directory form this returns is
# enough for it to find every qcsummary.tsv output from the run.
#
# outputDir: the run's outputDir value (possibly NULL/blank, meaning "not
#   set" -- run through blank_to_null() here so a blank string and NULL are
#   treated identically).
# directoryBIDS: the run's directoryBIDS value (always required, the input
#   data directory the run was launched against).
resolve_analyze_directory <- function(outputDir, directoryBIDS) {
  out <- blank_to_null(outputDir)
  if (!is.null(out)) {
    return(out)
  }
  directoryBIDS
}

# build_analyze_launch_command: a copy-pasteable eyeQuality::runAnalyzeApp()
# call pre-pointed at `directory`, for the Setup app's post-run panel
# (P9-07). Backslashes and double quotes are escaped so the printed call is a
# valid R string literal to paste into a console verbatim, not just an
# approximate display -- matters most for a raw Windows path
# (backslash-separated); shinyFiles::parseDirPath()'s own paths are always
# forward-slash and wouldn't need it, but directoryBIDS/outputDir can also
# come from a hand-typed or loaded-config value.
build_analyze_launch_command <- function(directory) {
  escaped <- gsub("\\", "\\\\", directory, fixed = TRUE)
  escaped <- gsub('"', '\\"', escaped, fixed = TRUE)
  sprintf('eyeQuality::runAnalyzeApp(initialDirectory = "%s")', escaped)
}
