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
#' `normalize_validity()` is a placeholder stub as of P3-05 — it is ported
#' from `removeInvalidGaze()` (P3-06), not implemented here. Calling it
#' before that work lands is an error by design, so a half-wired adapter
#' fails loudly instead of silently returning wrong data.
#'
#' @name tobii_studio_adapter
#' @keywords internal
NULL

.tobii_studio_detect <- function(data) {
  "StudioVersionRec" %in% names(data)
}

.tobii_studio_standardize <- function(data) {
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
#' @return `list(gaze, events)` -- see `?eyeQuality-schema`.
#' @keywords internal
.tobii_studio_extract_events <- function(data) {
  gazeStreamData <- data %>%
    dplyr::filter(.data$eyeTrackerTimestamp != -9999)
  eventData <- data %>%
    dplyr::filter(.data$eyeTrackerTimestamp == -9999 |
      is.na(.data$eyeTrackerTimestamp))
  list(gaze = gazeStreamData, events = eventData)
}

.tobii_studio_norm_validity <- function(data, threshold = NULL) {
  stop("tobii_studio_adapter$normalize_validity() is not yet implemented -- see P3-06", call. = FALSE)
}

# Tobii Studio's native validity scale is a discrete 0-4 code (0 = best),
# matching the pre-Phase-3 `removeInvalidGaze()` default of `threshold = 2`
# (R/removeInvalidGaze.R). Not yet consumed anywhere -- wiring this into
# `normalize_validity()`'s actual masking logic is P3-06's job.
tobii_studio_adapter <- new_eyetracker_adapter(
  name = "TobiiStudio",
  detect = .tobii_studio_detect,
  standardize = .tobii_studio_standardize,
  extract_events = .tobii_studio_extract_events,
  normalize_validity = .tobii_studio_norm_validity,
  geometry_type = "screen",
  default_thresholds = list(validityThreshold = 2)
)
