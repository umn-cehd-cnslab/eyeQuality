#' Tobii Studio eye tracker adapter
#'
#' @description
#' `detect()` is ported unchanged from the pre-Phase-3 `detectImportSourceType()`
#' `if`/`else` branch: raw (pre-standardization) Tobii Studio exports carry a
#' `"StudioVersionRec"` column that no other supported device produces.
#'
#' `standardize()` is ported unchanged (P3-04) from the pre-Phase-3
#' `renameColumns()`'s `TobiiStudio` branch (`R/renameColumns.R`): renames
#' the device-native columns onto the generic schema (`?eyeQuality-schema`)
#' and splits `RecordingResolution` into `resolutionWidth`/`resolutionHeight`.
#'
#' `extract_events()` is ported unchanged (P3-05) from the pre-Phase-3
#' `extractEventRows()`'s `TobiiStudio` branch (`R/extractEventRows.R`):
#' splits standardized data into gaze-stream vs. event rows using the
#' `eyeTrackerTimestamp == -9999` sentinel. Unlike the old function, this
#' adapter method takes no `software` argument to dispatch on -- the
#' registry (`detectImportSourceType()`, P3-03) already selects this adapter
#' specifically for Tobii Studio data, so there is no "unrecognized software
#' value" branch to carry forward here; see the roxygen note on
#' `.tobii_studio_extract_events()` below for the full reasoning.
#'
#' `normalize_validity()` is ported unchanged (P3-06) from the pre-Phase-3
#' `removeInvalidGaze()`'s `TobiiStudio` branch (`R/removeInvalidGaze.R`):
#' for each eye, samples above `threshold` on Tobii Studio's native `0`-`4`
#' validity scale (`0` = best) are masked to `NA` in new `.valid`-suffixed
#' columns (P1-05's decision; raw columns are left untouched), plus the
#' `-9999` missing-value sentinel is masked the same way. It additionally
#' computes the new `confidenceLeft`/`confidenceRight` (0-1) columns from
#' `?eyeQuality-schema` -- see `.tobii_studio_confidence()` below for the
#' NA-validity mapping decision this task resolves.
#'
#' @name tobii_studio_adapter
#' @keywords internal
NULL

.tobii_studio_detect <- function(data) {
  "StudioVersionRec" %in% names(data)
}

# Raw (pre-rename) Tobii Studio columns worth checking for the "present but
# 100% NA" diagnostic (P3-10) -- the measurement columns a user would
# actually want to know are empty, not every column on the raw export
# (e.g. StudioEvent is legitimately all-NA on files with no logged events).
.tobii_studio_raw_measurement_cols <- c(
  "GazePointLeftX (ADCSpx)", "GazePointLeftY (ADCSpx)",
  "GazePointRightX (ADCSpx)", "GazePointRightY (ADCSpx)",
  "EyePosLeftZ (ADCSmm)", "EyePosRightZ (ADCSmm)",
  "PupilLeft", "PupilRight",
  "ValidityLeft", "ValidityRight"
)

.tobii_studio_standardize <- function(data, verbose = FALSE) {
  .diagnose_all_na_columns(data, .tobii_studio_raw_measurement_cols, verbose)

  data <- data %>%
    dplyr::rename(
      event = "StudioEvent",
      eventValue = "StudioEventData",
      recordingDuration_ms = "RecordingDuration",
      eyeTrackerTimestamp = "EyeTrackerTimestamp",
      recordingTimestamp_ms = "RecordingTimestamp",
      gazeLeftX = "GazePointLeftX (ADCSpx)",
      gazeLeftY = "GazePointLeftY (ADCSpx)",
      gazeRightX = "GazePointRightX (ADCSpx)",
      gazeRightY = "GazePointRightY (ADCSpx)",
      distanceLeftZ = "EyePosLeftZ (ADCSmm)",
      distanceRightZ = "EyePosRightZ (ADCSmm)",
      pupilLeft = "PupilLeft",
      pupilRight = "PupilRight",
      validityLeft = "ValidityLeft",
      validityRight = "ValidityRight"
    )

  # split RecordingResolution column into resolutionWidth and resolutionHeight
  data <- data %>% tidyr::separate(
    .data$RecordingResolution,
    c("resolutionWidth", "resolutionHeight"),
    sep = " x ",
    convert = TRUE
  )

  data
}

#' Split standardized Tobii Studio data into gaze-stream and event rows
#'
#' @description
#' Ported unchanged from the pre-Phase-3 `extractEventRows()`'s
#' `TobiiStudio` branch (`R/extractEventRows.R`): gaze-stream rows are those
#' with a real `eyeTrackerTimestamp` (event/message rows carry the `-9999`
#' sentinel, or `NA`).
#'
#' P1-03 added a final `else stop(...)` to the old `extractEventRows()` so
#' an unrecognized `software` value would fail loudly and immediately,
#' instead of leaving `gazeStreamData`/`eventData` unassigned and producing
#' an opaque "object not found" error downstream. That fix doesn't carry
#' forward as an explicit branch here: this method has no `software`
#' argument to dispatch on in the first place, because the adapter registry
#' (`detectImportSourceType()`, P3-03) is what selects *which* adapter runs
#' at all -- calling `tobii_studio_adapter$extract_events()` IS the
#' "software == TobiiStudio" case, by construction, so there is no
#' fallthrough branch left to guard. The underlying intent (fail clearly and
#' immediately rather than silently proceeding with bad/missing data) is
#' preserved structurally: if this method is ever called on data lacking an
#' `eyeTrackerTimestamp` column (e.g. mismatched/malformed input), `dplyr`'s
#' `.data$eyeTrackerTimestamp` lookup errors immediately, naming the missing
#' column, rather than silently returning wrong data.
#'
#' @param data standardized data.frame (post `standardize()`).
#' @param verbose logical(1). When `TRUE`, emits P3-10 diagnostics about the
#'   gaze/event split via `.emit_diagnostic()`. Default `FALSE`.
#' @return `list(gaze, events)` -- see `?eyeQuality-schema`.
#' @keywords internal
.tobii_studio_extract_events <- function(data, verbose = FALSE) {
  gazeStreamData <- data %>%
    dplyr::filter(.data$eyeTrackerTimestamp != -9999)
  eventData <- data %>%
    dplyr::filter(.data$eyeTrackerTimestamp == -9999 |
      is.na(.data$eyeTrackerTimestamp))

  .emit_diagnostic(
    paste0(
      "extract_events: ", nrow(data), " row(s) split into ", nrow(gazeStreamData),
      " gaze-stream row(s) and ", nrow(eventData), " event row(s)"
    ),
    verbose
  )
  if (nrow(data) > 0 && nrow(eventData) / nrow(data) > 0.5) {
    .emit_diagnostic(
      paste0(
        "unusually high proportion of rows classified as events (",
        round(100 * nrow(eventData) / nrow(data), 1),
        "%) -- check for unexpected eyeTrackerTimestamp values"
      ),
      verbose
    )
  }

  list(gaze = gazeStreamData, events = eventData)
}

#' Mask invalid/out-of-threshold samples for one eye (Tobii Studio)
#'
#' @description
#' Ported unchanged from the pre-Phase-3 `removeInvalidGaze()`'s
#' `TobiiStudio` branch (`R/removeInvalidGaze.R`): every raw column whose
#' name contains `whichEye` (case-insensitive) and is not itself a validity
#' column gets a `.valid`-suffixed copy, masked to `NA` wherever the
#' matching eye's `validityLeft`/`validityRight` value exceeds `threshold`,
#' plus the `-9999` missing-value sentinel masked to `NA`. Raw columns are
#' left byte-identical (P1-05).
#'
#' @param data standardized data.frame (post `standardize()`).
#' @param whichEye `"left"` or `"right"`.
#' @param threshold numeric, on Tobii Studio's native `0`-`4` validity scale.
#' @param verbose logical(1). When `TRUE`, emits P3-10 diagnostics: runs of
#'   consecutive below-threshold samples (via `.diagnose_consecutive_runs()`)
#'   and a note about the documented `NA`-validity quirk (see
#'   `?eyeQuality-schema`'s `validityLeft`/`validityRight` entry) when it
#'   actually affects this eye's data. Default `FALSE`.
#' @return `data` with new `.valid`-suffixed columns for `whichEye`.
#' @keywords internal
.tobii_studio_mask_eye <- function(data, whichEye, threshold, verbose = FALSE) {
  cols <- colnames(data)[grepl(whichEye, colnames(data), ignore.case = TRUE) &
    !grepl("valid", colnames(data))]
  validityCol <- colnames(data)[grepl(whichEye, colnames(data), ignore.case = TRUE) &
    grepl("valid", colnames(data))]
  validityVals <- data[[validityCol]]

  maskFlag <- !is.na(validityVals) & validityVals > threshold
  .diagnose_consecutive_runs(
    maskFlag,
    paste0(whichEye, " eye validity below threshold (", threshold, ")"),
    verbose
  )
  naFlag <- is.na(validityVals)
  if (any(naFlag)) {
    .emit_diagnostic(
      paste0(
        sum(naFlag), " sample(s) have NA validity for the ", whichEye,
        " eye -- these are NOT masked (treated as valid) due to a documented ",
        "quirk in NA-validity handling, see ?eyeQuality-schema"
      ),
      verbose
    )
  }

  for (i in cols) {
    newCol <- paste0(i, ".valid")
    data[[newCol]] <- data[[i]]
    data[[newCol]][validityVals > threshold] <- NA
    # -9999 used to mark missing values, changed to NA
    data[[newCol]][data[[newCol]] == -9999] <- NA
  }
  data
}

#' Compute the generic `confidence` (0-1) columns from Tobii Studio's native
#' `0`-`4` validity codes
#'
#' @description
#' `confidenceLeft`/`confidenceRight <- 1 - (validityLeft/validityRight / 4)`,
#' so `0` (best) maps to `1.0` and `4` (worst) maps to `0.0` -- see the
#' `confidence` entry in `?eyeQuality-schema` for the full 0-1 design
#' rationale. The schema documents `confidence` as a single conceptual
#' field but derives it separately per eye from `validityLeft`/
#' `validityRight`; this implementation follows every other per-eye pair in
#' the schema (`gazeLeftX`/`gazeRightX`, `pupilLeft`/`pupilRight`, etc.) and
#' emits `confidenceLeft`/`confidenceRight` rather than inventing an
#' unspecified left/right combination rule.
#'
#' **NA-validity-to-confidence mapping (P3-06 decision):** `confidence` is
#' set to `1` for `NA` validity, matching option (a) in the OPEN DECISION
#' section of `?eyeQuality-schema`. Rationale: the *existing* `.valid`-
#' masking behavior (`.tobii_studio_mask_eye()` above, ported unchanged from
#' `removeInvalidGaze()`) already treats `NA` validity as unmasked --
#' effectively "valid" -- because `NA > threshold` is `NA`, and R's
#' `x[NA] <- value` subset-assignment is a no-op at `NA`-indexed positions.
#' `confidence = 1` for `NA` validity keeps this new column in agreement
#' with what masking actually does today, rather than introducing a second,
#' disagreeing notion of validity for the same input. This is a deliberate
#' choice, not a default: `confidence = 0` (treating unknown validity as
#' low-confidence) was the documented alternative, but it would not be zero
#' behavior change relative to the pipeline's current *effective* semantics
#' for `NA`-validity samples, only zero behavior change relative to the
#' (arguably buggy) masking mechanism -- and since nothing downstream reads
#' `confidence` yet, internal consistency with `.valid`-masking was judged
#' the more conservative choice for a Phase 3 refactor whose explicit goal
#' is zero behavior change.
#'
#' @param data standardized data.frame with `validityLeft`/`validityRight`.
#' @return `data` with new `confidenceLeft`/`confidenceRight` columns.
#' @keywords internal
.tobii_studio_confidence <- function(data) {
  data$confidenceLeft <- 1 - (data$validityLeft / 4)
  data$confidenceLeft[is.na(data$validityLeft)] <- 1
  data$confidenceRight <- 1 - (data$validityRight / 4)
  data$confidenceRight[is.na(data$validityRight)] <- 1
  data
}

#' Normalize Tobii Studio validity into `.valid`-masked columns and
#' `confidence`
#'
#' @description
#' Ported unchanged from the pre-Phase-3 `removeInvalidGaze()`'s
#' `TobiiStudio` branch (`R/removeInvalidGaze.R`), operating on both eyes in
#' a single call per the adapter interface (`?new_eyetracker_adapter`).
#'
#' The old `removeInvalidGaze()` took a `software` argument and had a final
#' `else stop(...)` (P1-04) guarding against an unrecognized value, so bad
#' input failed loudly instead of silently returning unmodified data. That
#' explicit branch doesn't carry forward here for the same structural reason
#' documented on `.tobii_studio_extract_events()`: this method has no
#' `software` argument to dispatch on, because the adapter registry already
#' selected this adapter specifically for Tobii Studio data. The underlying
#' intent is preserved structurally instead: if this method is called on
#' data lacking `validityLeft`/`validityRight` columns (e.g. mismatched
#' input), `validityCol` resolves to `character(0)` inside
#' `.tobii_studio_mask_eye()` and the subsequent `data[[validityCol]]`
#' lookup errors immediately (`"subscript out of bounds"`), rather than
#' silently proceeding with no masking applied.
#'
#' @param data standardized data.frame (post `standardize()`).
#' @param threshold numeric or `NULL`. On Tobii Studio's native `0`-`4`
#'   validity scale. When `NULL`, uses this adapter's
#'   `default_thresholds$validityThreshold` (`2`), matching the pre-Phase-3
#'   `removeInvalidGaze()` default.
#' @param verbose logical(1). When `TRUE`, forwards to
#'   `.tobii_studio_mask_eye()` for both eyes to emit P3-10 diagnostics.
#'   Default `FALSE`.
#' @return `data` with new `.valid`-suffixed masked columns and new
#'   `confidenceLeft`/`confidenceRight` columns.
#' @keywords internal
.tobii_studio_norm_validity <- function(data, threshold = NULL, verbose = FALSE) {
  if (is.null(threshold)) {
    threshold <- tobii_studio_adapter$default_thresholds$validityThreshold
  }
  data <- .tobii_studio_mask_eye(data, "left", threshold, verbose)
  data <- .tobii_studio_mask_eye(data, "right", threshold, verbose)
  data <- .tobii_studio_confidence(data)
  data
}

# Tobii Studio's native validity scale is a discrete 0-4 code (0 = best),
# matching the pre-Phase-3 `removeInvalidGaze()` default of `threshold = 2`
# (R/removeInvalidGaze.R).
tobii_studio_adapter <- new_eyetracker_adapter(
  name = "TobiiStudio",
  detect = .tobii_studio_detect,
  standardize = .tobii_studio_standardize,
  extract_events = .tobii_studio_extract_events,
  normalize_validity = .tobii_studio_norm_validity,
  geometry_type = "screen",
  default_thresholds = list(validityThreshold = 2)
)
