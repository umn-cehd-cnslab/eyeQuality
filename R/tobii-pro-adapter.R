#' Tobii Pro eye tracker adapter
#'
#' @description
#' `detect()` is ported unchanged from the pre-Phase-3 `detectImportSourceType()`
#' `if`/`else` branch: raw (pre-standardization) Tobii Pro exports carry a
#' `"Recording software version"` column that no other supported device
#' produces.
#'
#' `standardize()` is ported unchanged (P3-04) from the pre-Phase-3
#' `renameColumns()`'s `TobiiPro` branch (`R/renameColumns.R`): renames the
#' device-native columns onto the generic schema (`?eyeQuality-schema`).
#'
#' `extract_events()` and `normalize_validity()` are placeholder stubs as of
#' P3-04 — they are ported from `extractEventRows()` (P3-05) and
#' `removeInvalidGaze()` (P3-06) respectively, not implemented here. Calling
#' either before that work lands is an error by design, so a half-wired
#' adapter fails loudly instead of silently returning wrong data.
#'
#' @name tobii_pro_adapter
#' @keywords internal
NULL

.tobii_pro_detect <- function(data) {
  "Recording software version" %in% names(data)
}

.tobii_pro_standardize <- function(data) {
  data %>%
    dplyr::rename(
      event = "Event",
      eventValue = "Event value",
      recordingDuration_ms = "Recording duration",
      resolutionHeight = "Recording resolution height",
      resolutionWidth = "Recording resolution width",
      eyeTrackerTimestamp = "Eyetracker timestamp",
      recordingTimestamp_ms = "Recording timestamp",
      gazeLeftX = "Gaze point left X",
      gazeLeftY = "Gaze point left Y",
      gazeRightX = "Gaze point right X",
      gazeRightY = "Gaze point right Y",
      distanceLeftZ = "Eye position left Z (DACSmm)",
      distanceRightZ = "Eye position right Z (DACSmm)",
      pupilLeft = "Pupil diameter left",
      pupilRight = "Pupil diameter right",
      validityLeft = "Validity left",
      validityRight = "Validity right"
    )
}

.tobii_pro_extract_events <- function(data) {
  stop("tobii_pro_adapter$extract_events() is not yet implemented -- see P3-05", call. = FALSE)
}

.tobii_pro_normalize_validity <- function(data, threshold = NULL) {
  stop("tobii_pro_adapter$normalize_validity() is not yet implemented -- see P3-06", call. = FALSE)
}

# Tobii Pro's native validity scale is the binary "Valid"/"Invalid" string
# used by the pre-Phase-3 `removeInvalidGaze()` (R/removeInvalidGaze.R) --
# there is no numeric threshold concept for this device, so no
# `default_thresholds` entry is set here. Not yet consumed anywhere -- wiring
# this into `normalize_validity()`'s actual masking logic is P3-06's job.
tobii_pro_adapter <- new_eyetracker_adapter(
  name = "TobiiPro",
  detect = .tobii_pro_detect,
  standardize = .tobii_pro_standardize,
  extract_events = .tobii_pro_extract_events,
  normalize_validity = .tobii_pro_normalize_validity,
  geometry_type = "screen",
  default_thresholds = list()
)
