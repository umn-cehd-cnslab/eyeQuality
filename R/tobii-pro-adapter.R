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
#' `extract_events()` is ported unchanged (P3-05) from the pre-Phase-3
#' `extractEventRows()`'s `TobiiPro` branch (`R/extractEventRows.R`): splits
#' standardized data into gaze-stream vs. event rows using the `Sensor`
#' column. Unlike the old function, this adapter method takes no `software`
#' argument to dispatch on -- the registry (`detectImportSourceType()`,
#' P3-03) already selects this adapter specifically for Tobii Pro data, so
#' there is no "unrecognized software value" branch to carry forward here;
#' see the roxygen note on `.tobii_pro_extract_events()` below for the full
#' reasoning.
#'
#' `normalize_validity()` is a placeholder stub as of P3-05 — it is ported
#' from `removeInvalidGaze()` (P3-06), not implemented here. Calling it
#' before that work lands is an error by design, so a half-wired adapter
#' fails loudly instead of silently returning wrong data.
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

#' Split standardized Tobii Pro data into gaze-stream and event rows
#'
#' @description
#' Ported unchanged from the pre-Phase-3 `extractEventRows()`'s `TobiiPro`
#' branch (`R/extractEventRows.R`): gaze-stream rows are those where
#' `Sensor == "Eye Tracker"`; everything else (including `NA` `Sensor`) is
#' an event row.
#'
#' P1-03 added a final `else stop(...)` to the old `extractEventRows()` so
#' an unrecognized `software` value would fail loudly and immediately,
#' instead of leaving `gazeStreamData`/`eventData` unassigned and producing
#' an opaque "object not found" error downstream. That fix doesn't carry
#' forward as an explicit branch here: this method has no `software`
#' argument to dispatch on in the first place, because the adapter registry
#' (`detectImportSourceType()`, P3-03) is what selects *which* adapter runs
#' at all -- calling `tobii_pro_adapter$extract_events()` IS the
#' "software == TobiiPro" case, by construction, so there is no fallthrough
#' branch left to guard. The underlying intent (fail clearly and
#' immediately rather than silently proceeding with bad/missing data) is
#' preserved structurally: if this method is ever called on data lacking a
#' `Sensor` column (e.g. mismatched/malformed input), `dplyr`'s
#' `.data$Sensor` lookup errors immediately, naming the missing column,
#' rather than silently returning wrong data.
#'
#' @param data standardized data.frame (post `standardize()`).
#' @return `list(gaze, events)` -- see `?eyeQuality-schema`.
#' @keywords internal
.tobii_pro_extract_events <- function(data) {
  gazeStreamData <- data %>%
    dplyr::filter(.data$Sensor == "Eye Tracker")
  eventData <- data %>%
    dplyr::filter(.data$Sensor != "Eye Tracker" |
      is.na(.data$Sensor))
  list(gaze = gazeStreamData, events = eventData)
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
