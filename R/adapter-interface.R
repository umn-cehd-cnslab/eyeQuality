#' Construct an eye tracker adapter
#'
#' @description
#' An eye tracker adapter is a plain named list of five functions plus two
#' fields — deliberately S3-style (no S4/R6) to keep the dependency
#' footprint small. It is the unit that `register_eyetracker_adapter()`
#' stores and `registered_adapters()` returns. See `?eyeQuality-schema` for
#' the column contract every `standardize()`, `extract_events()`, and
#' `normalize_validity()` implementation must satisfy.
#'
#' The five functions are:
#' - `detect(data) -> logical` — does `data` match this adapter's raw
#'   (pre-standardization) column layout? Called on newly-imported data,
#'   before any renaming, to pick which adapter should handle a given file.
#' - `standardize(data, verbose = FALSE) -> data` — rename device-native
#'   columns onto the generic schema (`?eyeQuality-schema`).
#' - `extract_events(data, verbose = FALSE) -> list(gaze, events)` — split
#'   standardized data into a gaze-stream data frame and an event-stream
#'   data frame, using whatever device-specific discriminator identifies
#'   event rows (e.g. a sentinel timestamp value, a dedicated sensor/channel
#'   column).
#' - `normalize_validity(data, threshold = NULL, verbose = FALSE) -> data` —
#'   compute the `.valid`-suffixed masked columns and the generic
#'   `confidence` (0-1) column from the device-native `validityLeft`/
#'   `validityRight` columns. `threshold` is on the device-native validity
#'   scale; when `NULL`, the adapter's own `default_thresholds` value is
#'   used. Operates on both eyes in a single call. NA-validity-to-confidence
#'   mapping is an implementation decision left to each adapter's
#'   `normalize_validity()` body (see the OPEN DECISION section in
#'   `R/eyeQuality-schema.R`); this interface only specifies the function
#'   signature, not adapter-specific mapping logic.
#'
#' `standardize()`, `extract_events()`, and `normalize_validity()` each also
#' accept an optional `verbose = FALSE` argument (P3-10): when `TRUE`, the
#' method emits non-fatal, opt-in, format-specific data-quality diagnostics
#' for the successful/non-aborting path (e.g. "column `PupilLeft` present
#' but 100% NA", a run of consecutive samples below a validity threshold) via
#' the shared `.emit_diagnostic()`/`.diagnose_consecutive_runs()`/
#' `.diagnose_all_na_columns()` helpers defined below. `detect()` does not
#' take `verbose` — it runs on raw, pre-rename data purely to answer "does
#' this adapter match", and has no row-level content worth diagnosing before
#' a matching adapter (and therefore a known column layout) has even been
#' selected. This is purely additive to the interface: existing adapters
#' that don't accept `verbose` continue to work for every call site that
#' doesn't pass it; `eyeQuality()` (P3-10) always passes it explicitly to
#' the three methods listed above.
#'
#' @param name character(1). Unique identifier for this adapter, used as the
#'   registry key (e.g. `"TobiiStudio"`).
#' @param detect function(data) -> logical.
#' @param standardize function(data, verbose = FALSE) -> data.
#' @param extract_events function(data, verbose = FALSE) -> list(gaze, events).
#' @param normalize_validity function(data, threshold = NULL, verbose = FALSE) -> data.
#'   NA-validity-to-confidence mapping is an implementation decision left to
#'   each adapter's `normalize_validity()` body (see the OPEN DECISION
#'   section in `R/eyeQuality-schema.R`); this interface only specifies the
#'   function signature, not adapter-specific mapping logic.
#' @param geometry_type character(1), one of `"screen"`, `"headmounted"`,
#'   `"none"`. Declares what kind of display/tracking geometry this
#'   adapter's data assumes; downstream geometry-dependent logic (pixel/
#'   visual-angle conversion, distance-based checks) is gated on this field.
#' @param default_thresholds named list of adapter-scoped defaults consumed
#'   by this adapter's own functions (e.g. `list(validityThreshold = 2)` for
#'   Tobii Studio's `0`-`4` validity scale). Not on the generic `confidence`
#'   (0-1) scale — that is the adapter-independent `validityThreshold`
#'   argument planned for `eyeQuality()` (P3-08).
#'
#' @return A list of class `"eyetracker_adapter"` with elements `name`,
#'   `detect`, `standardize`, `extract_events`, `normalize_validity`,
#'   `geometry_type`, `default_thresholds`.
#' @export
new_eyetracker_adapter <- function(name,
                                    detect,
                                    standardize,
                                    extract_events,
                                    normalize_validity,
                                    geometry_type,
                                    default_thresholds = list()) {
  adapter <- list(
    name = name,
    detect = detect,
    standardize = standardize,
    extract_events = extract_events,
    normalize_validity = normalize_validity,
    geometry_type = geometry_type,
    default_thresholds = default_thresholds
  )
  class(adapter) <- "eyetracker_adapter"
  validate_adapter(adapter)
  adapter
}

#' Emit an opt-in verbose data-quality diagnostic message (P3-10)
#'
#' @description
#' Common helper called by adapter methods (`standardize()`,
#' `extract_events()`, `normalize_validity()`) and by device-agnostic
#' pipeline stages (`classifyBlinks()`, `eyeSelection()`,
#' `removeOffscreenGaze()`, `interpolateGaze()`) to report row/column-level,
#' data-quality-relevant events an end user wouldn't necessarily think to
#' check for themselves. A no-op unless `verbose` is `TRUE`, so call sites
#' don't need to guard every call with their own `if (verbose)` check.
#'
#' Uses `print()` (not `message()`), matching the same reporting convention
#' as `eyeQuality()`'s existing per-stage progress lines, so that when
#' `eyeQuality(saveData = TRUE)` sinks console output to a run log
#' (`sinkToOutputFile()`), diagnostics land in that same log rather than a
#' separate, unsunk stream. This is an entirely separate, non-fatal, opt-in
#' channel from P1-14's fix (which made existing hard-abort `stop()` calls
#' always carry a message); nothing here changes error/abort behavior.
#'
#' @param text character(1). The diagnostic message to emit (without a
#'   leading "verbose:"-style tag -- this function adds a consistent prefix).
#' @param verbose logical(1). When not `TRUE`, this function is a no-op.
#' @return `invisible(NULL)`.
#' @keywords internal
.emit_diagnostic <- function(text, verbose = FALSE) {
  if (isTRUE(verbose)) {
    print(paste0("[verbose] ", text))
  }
  invisible(NULL)
}

#' Diagnose contiguous runs of flagged samples (P3-10)
#'
#' @description
#' Given a logical vector where `TRUE` marks a sample flagged for some
#' data-quality reason (below a validity threshold, offscreen, missing in
#' both eyes, etc.), reports both a total count and, for any contiguous run
#' at least `min_run_length` samples long, a "rows `start`-`end`: `n`
#' consecutive samples ..." line -- e.g. the plan's illustrative "rows
#' 1402-1389: N consecutive samples with validity below threshold" (row
#' order here is always ascending start-end, not the plan's example order).
#' Short, scattered single/few-sample flags are summarized only in the total
#' count, not individually, to keep verbose output genuinely diagnostic
#' rather than a line-per-row flood.
#'
#' @param flag logical vector, same length as the data being diagnosed;
#'   `NA` is treated as not-flagged (consistent with this package's existing
#'   `NA`-passes-through validity-masking behavior, see `?eyeQuality-schema`).
#' @param label character(1) description of what `flag == TRUE` means,
#'   inserted into the emitted message (e.g. `"left eye validity below
#'   threshold (2)"`).
#' @param verbose logical(1). When not `TRUE`, this function is a no-op.
#' @param min_run_length integer(1). Minimum contiguous run length to report
#'   individually. Default `5`.
#' @return `invisible(NULL)`.
#' @keywords internal
.diagnose_consecutive_runs <- function(flag, label, verbose, min_run_length = 5) {
  if (!isTRUE(verbose)) {
    return(invisible(NULL))
  }
  flag <- !is.na(flag) & flag
  total <- sum(flag)
  if (total == 0) {
    return(invisible(NULL))
  }

  .emit_diagnostic(paste0(total, " sample(s) flagged for: ", label), verbose)

  runs <- rle(flag)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  long_runs <- which(runs$values & runs$lengths >= min_run_length)

  for (i in long_runs) {
    .emit_diagnostic(
      paste0(
        "rows ", starts[i], "-", ends[i], ": ", runs$lengths[i],
        " consecutive samples flagged for: ", label
      ),
      verbose
    )
  }

  invisible(NULL)
}

#' Diagnose schema columns present but entirely NA (P3-10)
#'
#' @description
#' For each column name in `cols` that exists in `data`, reports (in
#' verbose mode) when every value in that column is `NA` -- the "column
#' `PupilLeft` present but 100% NA" case from the plan: the column made it
#' through import (so it isn't simply missing/misnamed), but carries no
#' usable data, which a user skimming only the final QC summary would not
#' otherwise be pointed at directly.
#'
#' @param data data.frame to check.
#' @param cols character vector of column names to check (only those
#'   actually present in `data` are checked; this deliberately does not
#'   flag columns that are missing entirely, since a required-but-absent
#'   column is a separate, already-loud failure mode elsewhere in the
#'   pipeline, not a silent-data-quality one).
#' @param verbose logical(1). When not `TRUE`, this function is a no-op.
#' @return `invisible(NULL)`.
#' @keywords internal
.diagnose_all_na_columns <- function(data, cols, verbose) {
  if (!isTRUE(verbose)) {
    return(invisible(NULL))
  }
  for (col in cols) {
    if (col %in% names(data) && length(data[[col]]) > 0 && all(is.na(data[[col]]))) {
      .emit_diagnostic(paste0("column '", col, "' present but 100% NA"), verbose)
    }
  }
  invisible(NULL)
}

#' Validate that an object satisfies the eye tracker adapter contract
#'
#' @description
#' Checks that `adapter` has all required fields, that the four interface
#' functions are actually functions accepting at least a `data` argument,
#' and that `geometry_type`/`default_thresholds` are well-formed. Collects
#' every problem found (rather than stopping at the first) so a broken
#' adapter definition produces one clear, complete error instead of a
#' cryptic failure several call frames later.
#'
#' @param adapter object to validate, typically produced by
#'   `new_eyetracker_adapter()`.
#'
#' @return `TRUE` (invisibly) if `adapter` is valid; otherwise throws an
#'   error listing every problem found.
#' @export
validate_adapter <- function(adapter) {
  problems <- character(0)

  if (!is.list(adapter)) {
    stop("validate_adapter: `adapter` must be a list, got: ", class(adapter)[1])
  }

  required_functions <- c("detect", "standardize", "extract_events", "normalize_validity")
  required_fields <- c("name", required_functions, "geometry_type", "default_thresholds")

  missing_fields <- setdiff(required_fields, names(adapter))
  if (length(missing_fields) > 0) {
    problems <- c(
      problems,
      paste0("missing required field(s): ", paste(missing_fields, collapse = ", "))
    )
  }

  if ("name" %in% names(adapter)) {
    if (!is.character(adapter$name) || length(adapter$name) != 1 || is.na(adapter$name) || !nzchar(adapter$name)) {
      problems <- c(problems, "`name` must be a single non-empty, non-NA character string")
    }
  }

  for (fn_name in required_functions) {
    if (!(fn_name %in% names(adapter))) next
    fn <- adapter[[fn_name]]
    if (!is.function(fn)) {
      problems <- c(problems, paste0("`", fn_name, "` must be a function, got: ", class(fn)[1]))
    } else if (length(formals(fn)) < 1) {
      problems <- c(problems, paste0("`", fn_name, "` must accept at least one argument (`data`)"))
    }
  }

  if ("geometry_type" %in% names(adapter)) {
    valid_geometry_types <- c("screen", "headmounted", "none")
    if (!is.character(adapter$geometry_type) || length(adapter$geometry_type) != 1 ||
      !(adapter$geometry_type %in% valid_geometry_types)) {
      problems <- c(
        problems,
        paste0(
          "`geometry_type` must be one of: ",
          paste(shQuote(valid_geometry_types), collapse = ", "),
          " — got: ", paste(deparse(adapter$geometry_type), collapse = "")
        )
      )
    }
  }

  if ("default_thresholds" %in% names(adapter)) {
    if (!is.list(adapter$default_thresholds)) {
      problems <- c(problems, "`default_thresholds` must be a list")
    }
  }

  if (length(problems) > 0) {
    stop(
      "validate_adapter: invalid eye tracker adapter",
      if ("name" %in% names(adapter) && is.character(adapter$name) && length(adapter$name) == 1) {
        paste0(" ('", adapter$name, "')")
      },
      ":\n  - ", paste(problems, collapse = "\n  - ")
    )
  }

  invisible(TRUE)
}

# Internal registry of eye tracker adapters, keyed by adapter name.
# `parent = emptyenv()` so this environment holds only what is explicitly
# assigned into it (no accidental lookup fallthrough to the calling scope).
.eyetracker_adapter_registry <- new.env(parent = emptyenv())

#' Register an eye tracker adapter
#'
#' @description
#' Adds `adapter` to the package's internal adapter registry, keyed by
#' `adapter$name`. Registering an adapter under a name that is already
#' registered overwrites the previous entry (with a warning) — this is
#' deliberate, so re-sourcing an adapter file during development or
#' re-registering a mock adapter in a test doesn't require restarting the
#' session.
#'
#' @param adapter an eye tracker adapter, typically produced by
#'   `new_eyetracker_adapter()`. Validated via `validate_adapter()` before
#'   being stored.
#'
#' @return `adapter`, invisibly.
#' @export
register_eyetracker_adapter <- function(adapter) {
  validate_adapter(adapter)

  if (exists(adapter$name, envir = .eyetracker_adapter_registry, inherits = FALSE)) {
    warning(
      "register_eyetracker_adapter: overwriting already-registered adapter '",
      adapter$name, "'"
    )
  }

  assign(adapter$name, adapter, envir = .eyetracker_adapter_registry)
  invisible(adapter)
}

#' List currently registered eye tracker adapters
#'
#' @description
#' Returns every adapter added via `register_eyetracker_adapter()`, as a
#' named list (names are adapter names, sorted alphabetically). Used by
#' `detectImportSourceType()` (P3-03) to find a matching adapter, and
#' intended as the source of truth for anywhere else in the package that
#' needs to enumerate supported devices (e.g. a future Shiny UI dropdown).
#'
#' @return A named list of `"eyetracker_adapter"` objects; empty list if
#'   none are registered.
#' @export
registered_adapters <- function() {
  adapter_names <- sort(ls(envir = .eyetracker_adapter_registry, all.names = TRUE))
  mget(adapter_names, envir = .eyetracker_adapter_registry)
}
