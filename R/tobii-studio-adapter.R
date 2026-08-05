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
#' `extract_events()` and `normalize_validity()` are placeholder stubs as of
#' P3-04 — they are ported from `extractEventRows()` (P3-05) and
#' `removeInvalidGaze()` (P3-06) respectively, not implemented here. Calling
#' either before that work lands is an error by design, so a half-wired
#' adapter fails loudly instead of silently returning wrong data.
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

.tobii_studio_extract_events <- function(data) {
  stop("tobii_studio_adapter$extract_events() is not yet implemented -- see P3-05", call. = FALSE)
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
