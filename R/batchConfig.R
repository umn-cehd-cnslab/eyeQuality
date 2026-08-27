# batch_config.yaml schema (P7-02): read/write/validate machinery for
# reproducible batch runs -- save a run's parameters to a YAML file, reload
# it later to drive an identical run. Field names deliberately match the
# real `listBidsFiles()`/`eyeQuality()`/`eyeQualityBatch()` parameters they
# feed, so a config isn't a second vocabulary a user (or a GUI built on top
# of this) has to learn separately from the functions it configures.
#
# v1 scope covers what's real today: directory/layout, adapter/device type
# (informational only -- see adapterType below), display dimensions, eye
# selection method, validity threshold, output directory, batch name, and
# core count. No geometry-conditional fields, since only screen-type
# adapters (Tobii Studio/Pro) are registered; extend rather than redesign
# once a head-mounted adapter exists to condition against.

# CURRENT_SCHEMA_VERSION: bumped whenever a field is added, renamed, or
# removed in a way that would change how an older config should be
# interpreted. Stored in every written config so a future reader can detect
# and, if ever needed, migrate an older file rather than silently
# misinterpreting it.
CURRENT_SCHEMA_VERSION <- 1L

# Custom `yaml::as.yaml()`/`write_yaml()` handler so logical fields are
# written as `true`/`false` rather than the `yaml` package's YAML-1.1-style
# default (`yes`/`no`) -- `true`/`false` is what most hand-editors expect,
# and is what the schema's own documentation/template below uses.
.batch_config_yaml_handlers <- list(
  logical = function(x) {
    result <- ifelse(x, "true", "false")
    class(result) <- "verbatim"
    result
  }
)

#' Default `batch_config.yaml` contents
#'
#' @description
#' Returns a config with `schemaVersion` set and every optional field at the
#' same default `listBidsFiles()`/`eyeQuality()`/`eyeQualityBatch()` would
#' use if that field were left unspecified. Fields with no sensible
#' package-wide default (`batchName`, `directoryBIDS`,
#' `displayDimensionX_mm`, `displayDimensionY_mm`) are `NULL` -- a fresh
#' config built from this is a starting point to fill in, not already a
#' valid one (`validate_batch_config()` will say so). Used internally by
#' `read_batch_config()`/`write_batch_config()` to fill in any field a
#' caller/file omits, and exported as a convenience for anything (e.g. a
#' future Shiny form) that wants to pre-populate a blank config.
#'
#' @return a named list; see `?read_batch_config` for the full field list
#' @export
default_batch_config <- function() {
  list(
    schemaVersion = CURRENT_SCHEMA_VERSION,
    batchName = NULL,
    directoryBIDS = NULL,
    layout = "bids",
    subjectPattern_regex = "sub-[A-Z0-9]+",
    sessionPattern_regex = "ses-[0-9]+",
    recursiveSearch = FALSE,
    pathPattern = NULL,
    excludePattern_regex = NULL,
    modalityPattern_regex = NULL,
    adapterType = NULL,
    displayDimensionX_mm = NULL,
    displayDimensionY_mm = NULL,
    eyeSelection_method = "Maximize",
    validityThreshold = NULL,
    outputDir = NULL,
    numberCores = NULL,
    qcThresholds = NULL
  )
}

# .known_qc_threshold_ids: the qcThresholds keys validate_batch_config()
# below recognizes, read directly from the Analyze tabs' qc_threshold_config
# (inst/shiny-apps/app/analyze_helpers.R, P10-02; P10-12 merged the
# formerly-separate Setup/Analyze apps into one inst/shiny-apps/app/ app,
# moving this file from inst/shiny-apps/analyze/helpers.R) rather than
# duplicated here -- that object (and its own comment explaining what each
# threshold_id means and why those particular metrics were chosen) is the
# single source of truth for which QC metrics are thresholdable, and this
# function exists purely so validate_batch_config() doesn't have to keep a
# second id list in sync with it by hand. Sourced fresh into an isolated
# environment (parented to baseenv(), not the caller's search path) on every
# call -- analyze_helpers.R is a tiny, dependency-free file (its top-level
# statements only define functions and one data.frame() literal; every
# package/other-file call inside those function bodies is fully
# namespace-qualified, so it neither needs nor picks up anything from the
# caller's environment) with no meaningful cost to re-source, so this avoids
# a stale in-memory copy rather than trading correctness for a cache.
#
# Returns a character vector of unique threshold_id values, or character(0)
# if inst/shiny-apps/app/analyze_helpers.R can't be located (e.g. an
# unusual install without the Shiny apps present) -- validate_batch_config()
# treats that as "can't verify, don't block" rather than rejecting every
# qcThresholds entry as unrecognized.
.known_qc_threshold_ids <- function() {
  helpers_path <- system.file("shiny-apps", "app", "analyze_helpers.R", package = "eyeQuality")
  if (!nzchar(helpers_path)) {
    return(character(0))
  }
  env <- new.env(parent = baseenv())
  source(helpers_path, local = env)
  if (!exists("qc_threshold_config", envir = env, inherits = FALSE)) {
    return(character(0))
  }
  unique(env$qc_threshold_config$threshold_id)
}

#' Validate a `batch_config.yaml`-shaped list
#'
#' @description
#' Checks that `config` has the structure and types
#' `read_batch_config()`/`write_batch_config()`/`eyeQualityBatch()` expect,
#' collecting every problem found (rather than stopping at the first) so a
#' broken or hand-edited config produces one clear, complete error instead
#' of a cryptic failure several call frames later -- the same approach
#' `validate_adapter()` (`R/adapter-interface.R`) takes for adapters.
#'
#' Deliberately does not check that `directoryBIDS`/`outputDir` exist on
#' disk: a config is meant to be portable (e.g. authored on one machine,
#' run on another, or saved before the target directory is created), so
#' filesystem existence is left to `listBidsFiles()`/`eyeQualityBatch()` to
#' report when the config is actually used.
#'
#' @param config a named list, typically from `read_batch_config()`,
#'   `default_batch_config()`, or built by hand/by a GUI form.
#'
#' @return `TRUE` (invisibly) if `config` is valid; otherwise throws an
#'   error listing every problem found.
#' @export
validate_batch_config <- function(config) {
  if (!is.list(config)) {
    stop("validate_batch_config: `config` must be a list, got: ", class(config)[1])
  }

  problems <- character(0)

  add_problem <- function(...) {
    problems <<- c(problems, paste0(...))
  }

  is_scalar_character <- function(x) {
    is.character(x) && length(x) == 1 && !is.na(x)
  }
  is_optional_nonempty_string <- function(x) {
    is.null(x) || (is_scalar_character(x) && nzchar(x))
  }
  is_scalar_numeric <- function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x)
  }

  # --- schemaVersion --------------------------------------------------
  schemaVersion <- config[["schemaVersion"]]
  if (is.null(schemaVersion) || !is_scalar_numeric(schemaVersion) || schemaVersion %% 1 != 0) {
    add_problem("`schemaVersion` must be a single integer, got: ", paste(deparse(schemaVersion), collapse = ""))
  } else if (schemaVersion > CURRENT_SCHEMA_VERSION) {
    add_problem(
      "`schemaVersion` (", schemaVersion, ") is newer than this package version supports (",
      CURRENT_SCHEMA_VERSION, "); update eyeQuality to read this config"
    )
  }

  # --- required fields (no sensible package-wide default) -------------
  batchName <- config[["batchName"]]
  if (!is_scalar_character(batchName) || !nzchar(batchName)) {
    add_problem("`batchName` is required and must be a non-empty character string")
  }

  directoryBIDS <- config[["directoryBIDS"]]
  if (!is_scalar_character(directoryBIDS) || !nzchar(directoryBIDS)) {
    add_problem("`directoryBIDS` is required and must be a non-empty character string (the top-level data directory)")
  }

  displayDimensionX_mm <- config[["displayDimensionX_mm"]]
  if (!is_scalar_numeric(displayDimensionX_mm) || displayDimensionX_mm <= 0) {
    add_problem("`displayDimensionX_mm` is required and must be a single positive number")
  }

  displayDimensionY_mm <- config[["displayDimensionY_mm"]]
  if (!is_scalar_numeric(displayDimensionY_mm) || displayDimensionY_mm <= 0) {
    add_problem("`displayDimensionY_mm` is required and must be a single positive number")
  }

  # --- directory / layout (listBidsFiles()) ----------------------------
  layout <- config[["layout"]]
  valid_layouts <- c("bids", "glob")
  if (!is_scalar_character(layout) || !(layout %in% valid_layouts)) {
    add_problem("`layout` must be one of: ", paste(shQuote(valid_layouts), collapse = ", "))
    layout <- NA_character_ # so the layout-conditional checks below are skipped cleanly
  }

  for (field in c("subjectPattern_regex", "sessionPattern_regex", "pathPattern", "excludePattern_regex", "modalityPattern_regex", "outputDir", "adapterType")) {
    if (!is_optional_nonempty_string(config[[field]])) {
      add_problem("`", field, "` must be NULL or a non-empty character string, got: ", paste(deparse(config[[field]]), collapse = ""))
    }
  }

  recursiveSearch <- config[["recursiveSearch"]]
  if (!is.logical(recursiveSearch) || length(recursiveSearch) != 1 || is.na(recursiveSearch)) {
    add_problem("`recursiveSearch` must be a single TRUE/FALSE")
  }

  if (identical(layout, "glob") && is.null(config[["pathPattern"]])) {
    add_problem("`pathPattern` is required when `layout` is 'glob'")
  }

  # --- adapter/device type (informational -- see roxygen note) --------
  adapterType <- config[["adapterType"]]
  if (!is.null(adapterType) && is_scalar_character(adapterType)) {
    known_adapters <- names(registered_adapters())
    if (!(adapterType %in% known_adapters)) {
      add_problem(
        "`adapterType` ('", adapterType, "') is not a currently registered adapter (known: ",
        paste(known_adapters, collapse = ", "), ")"
      )
    }
  }

  # --- eye selection / validity threshold / cores ----------------------
  eyeSelection_method <- config[["eyeSelection_method"]]
  valid_eye_selection_methods <- c("Maximize", "Strict", "Left", "Right")
  if (!is_scalar_character(eyeSelection_method) || !(eyeSelection_method %in% valid_eye_selection_methods)) {
    add_problem("`eyeSelection_method` must be one of: ", paste(shQuote(valid_eye_selection_methods), collapse = ", "))
  }

  validityThreshold <- config[["validityThreshold"]]
  if (!is.null(validityThreshold) &&
    (!is_scalar_numeric(validityThreshold) || validityThreshold < 0 || validityThreshold > 1)) {
    add_problem("`validityThreshold` must be NULL or a single number between 0 and 1 (generic confidence scale)")
  }

  numberCores <- config[["numberCores"]]
  if (!is.null(numberCores) &&
    (!is_scalar_numeric(numberCores) || numberCores %% 1 != 0 || numberCores < 1)) {
    add_problem("`numberCores` must be NULL or a single positive integer")
  }

  # --- qcThresholds (P10-07, optional) ---------------------------------
  # Analyze app (P10-02) QC threshold overrides, saved alongside a batch
  # run's own parameters in the same file rather than a second config
  # format. NULL -- what every config written before this field existed
  # reads back as, via default_batch_config()'s fill-in in
  # read_batch_config()/write_batch_config() -- is valid: thresholds simply
  # stay at the Analyze app's own defaults. When present, each entry is
  # keyed by a threshold_id and must be a recognized one (.known_qc_threshold_ids(),
  # sourced from the Analyze app's qc_threshold_config so this can't drift
  # out of sync with it) with a sane 0-100 percentage value.
  qcThresholds <- config[["qcThresholds"]]
  if (!is.null(qcThresholds)) {
    if (!is.list(qcThresholds)) {
      add_problem("`qcThresholds` must be NULL or a named list mapping threshold_id -> percent (0-100)")
    } else if (length(qcThresholds) > 0 && (is.null(names(qcThresholds)) || any(!nzchar(names(qcThresholds))))) {
      add_problem("`qcThresholds` entries must all be named (each name a threshold_id)")
    } else if (length(qcThresholds) > 0) {
      known_threshold_ids <- .known_qc_threshold_ids()
      for (threshold_id in names(qcThresholds)) {
        threshold_value <- qcThresholds[[threshold_id]]
        if (length(known_threshold_ids) > 0 && !(threshold_id %in% known_threshold_ids)) {
          add_problem(
            "`qcThresholds` has an unrecognized threshold_id: '", threshold_id, "' (known: ",
            paste(known_threshold_ids, collapse = ", "), ")"
          )
        } else if (!is_scalar_numeric(threshold_value) || threshold_value < 0 || threshold_value > 100) {
          add_problem(
            "`qcThresholds[[\"", threshold_id, "\"]]` must be a single number between 0 and 100 (a percentage), got: ",
            paste(deparse(threshold_value), collapse = "")
          )
        }
      }
    }
  }

  if (length(problems) > 0) {
    stop(
      "validate_batch_config: invalid batch_config:\n  - ",
      paste(problems, collapse = "\n  - ")
    )
  }

  invisible(TRUE)
}

#' Write a `batch_config.yaml` file
#'
#' @description
#' Fills in any field `config` omits with `default_batch_config()`'s
#' defaults, validates the result (`validate_batch_config()`), and writes it
#' to `path` as YAML. Writing an invalid config (e.g. missing `batchName`)
#' is refused with the same clear, complete error `validate_batch_config()`
#' would raise, rather than writing a file that would only fail later on
#' load.
#'
#' @param config a named list; see `?read_batch_config` for the full field
#'   list. Any field omitted is filled in from `default_batch_config()`
#'   before writing -- explicit `NULL` values in `config` are preserved
#'   (not overwritten by a non-`NULL` default).
#' @param path character(1) file path to write to (parent directory created
#'   if it doesn't already exist).
#'
#' @return `path`, invisibly.
#' @importFrom utils modifyList
#' @export
write_batch_config <- function(config, path) {
  if (!is.list(config)) {
    stop("write_batch_config: `config` must be a list, got: ", class(config)[1])
  }
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("write_batch_config: `path` must be a single non-empty character string")
  }

  # keep.null = TRUE: a config that explicitly sets e.g.
  # `subjectPattern_regex = NULL` (meaning "search every subfolder",
  # different from the non-NULL default) must have that NULL preserved
  # through the merge, not silently replaced by the default -- plain
  # modifyList() (keep.null = FALSE) would delete the field instead of
  # keeping it NULL, which for a required-with-a-default field would
  # resurrect the default rather than honoring the caller's explicit NULL.
  full_config <- modifyList(default_batch_config(), config, keep.null = TRUE)

  validate_batch_config(full_config)

  fs::dir_create(fs::path_dir(path))
  yaml::write_yaml(full_config, path, handlers = .batch_config_yaml_handlers)

  invisible(path)
}

#' Read a `batch_config.yaml` file
#'
#' @description
#' Reads `path` as YAML, fills in any field the file omits with
#' `default_batch_config()`'s defaults (so a config saved before a new
#' optional field was added still loads with sensible behavior for that
#' field), and by default validates the result before returning it.
#'
#' @param path character(1) file path to read.
#' @param validate logical(1). When `TRUE` (the default), the merged config
#'   is passed to `validate_batch_config()` before being returned, so a
#'   structurally broken config is caught here rather than surfacing as a
#'   confusing failure inside `listBidsFiles()`/`eyeQualityBatch()` later.
#'   Set to `FALSE` to load a possibly-incomplete config as-is (e.g. to
#'   repopulate a GUI form and let the user finish filling it in, then
#'   validate separately once they're done).
#'
#' @return a named list; see the field list below. Every field
#'   `default_batch_config()` defines is present (either from the file or
#'   from that default).
#'
#'   \describe{
#'     \item{`schemaVersion`}{integer, config schema version.}
#'     \item{`batchName`}{character, passed to `eyeQualityBatch(batchName =)`.}
#'     \item{`directoryBIDS`}{character, passed to
#'       `eyeQualityBatch(directoryBIDS =)` / `listBidsFiles(directory =)`.}
#'     \item{`layout`}{`"bids"` or `"glob"`, passed to
#'       `listBidsFiles(layout =)`.}
#'     \item{`subjectPattern_regex`, `sessionPattern_regex`,
#'       `recursiveSearch`}{`listBidsFiles()` `layout = "bids"` options.}
#'     \item{`pathPattern`, `excludePattern_regex`}{`listBidsFiles()`
#'       `layout = "glob"` options.}
#'     \item{`modalityPattern_regex`}{`listBidsFiles()` option, used in
#'       either layout mode.}
#'     \item{`adapterType`}{character or `NULL`; the expected
#'       `registered_adapters()` name (e.g. `"TobiiStudio"`) for this
#'       batch's data. Informational only -- `eyeQuality()` detects the
#'       adapter per file from file content (`detectImportSourceType()`)
#'       and this is not passed through as an override; recorded here for
#'       reproducibility/documentation and checked against
#'       `registered_adapters()` when set.}
#'     \item{`displayDimensionX_mm`, `displayDimensionY_mm`}{numeric,
#'       passed to `eyeQualityBatch()`/`eyeQuality()`.}
#'     \item{`eyeSelection_method`}{one of `"Maximize"`, `"Strict"`,
#'       `"Left"`, `"Right"`, passed to `eyeQuality(eyeSelection_method =)`.}
#'     \item{`validityThreshold`}{numeric on the 0-1 confidence scale, or
#'       `NULL`, passed to `eyeQuality(validityThreshold =)`.}
#'     \item{`outputDir`}{character or `NULL`, passed to
#'       `eyeQualityBatch(outputDir =)`.}
#'     \item{`numberCores`}{positive integer or `NULL`, passed to
#'       `eyeQualityBatch(numberCores =)`.}
#'     \item{`qcThresholds`}{named list or `NULL` (P10-07). Optional QC
#'       threshold overrides from the Analyze tabs' threshold controls
#'       (`inst/shiny-apps/app/analyze_helpers.R`'s `qc_threshold_config`),
#'       keyed by `threshold_id` with each value a 0-100 percentage. Lets one
#'       `batch_config.yaml` cover both a study's run parameters (Setup tab)
#'       and its QC flagging settings (Analyze tabs) instead of splitting
#'       into two files (both live in one app, `eyeQuality::runSetupApp()`/
#'       `eyeQuality::runAnalyzeApp()`, since P10-12). Absent entirely in any
#'       config written before this field existed -- `NULL` is the correct,
#'       fully backward-compatible reading for that case.}
#'   }
#' @importFrom utils modifyList
#' @export
read_batch_config <- function(path, validate = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("read_batch_config: `path` must be a single non-empty character string")
  }
  if (!file.exists(path)) {
    stop("read_batch_config: file does not exist: ", path)
  }

  raw_config <- yaml::read_yaml(path)
  if (is.null(raw_config)) {
    raw_config <- list()
  }
  if (!is.list(raw_config)) {
    stop("read_batch_config: '", path, "' did not parse as a YAML mapping (got: ", class(raw_config)[1], ")")
  }

  # keep.null = TRUE: see write_batch_config() for why an explicit NULL in
  # the file must be preserved rather than papered over by the default.
  full_config <- modifyList(default_batch_config(), raw_config, keep.null = TRUE)

  if (isTRUE(validate)) {
    validate_batch_config(full_config)
  }

  full_config
}
