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
