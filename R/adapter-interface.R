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
#' - `standardize(data) -> data` — rename device-native columns onto the
#'   generic schema (`?eyeQuality-schema`).
#' - `extract_events(data) -> list(gaze, events)` — split standardized data
#'   into a gaze-stream data frame and an event-stream data frame, using
#'   whatever device-specific discriminator identifies event rows (e.g. a
#'   sentinel timestamp value, a dedicated sensor/channel column).
#' - `normalize_validity(data, threshold = NULL) -> data` — compute the
#'   `.valid`-suffixed masked columns and the generic `confidence` (0-1)
#'   column from the device-native `validityLeft`/`validityRight` columns.
#'   `threshold` is on the device-native validity scale; when `NULL`, the
#'   adapter's own `default_thresholds` value is used. Operates on both eyes
#'   in a single call. NA-validity-to-confidence mapping is an implementation
#'   decision left to each adapter's `normalize_validity()` body (see the
#'   OPEN DECISION section in `R/eyeQuality-schema.R`); this interface only
#'   specifies the function signature, not adapter-specific mapping logic.
#'
#' @param name character(1). Unique identifier for this adapter, used as the
#'   registry key (e.g. `"TobiiStudio"`).
#' @param detect function(data) -> logical.
#' @param standardize function(data) -> data.
#' @param extract_events function(data) -> list(gaze, events).
#' @param normalize_validity function(data, threshold = NULL) -> data.
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
