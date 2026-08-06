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
#' `normalize_validity()` is ported unchanged (P3-06) from the pre-Phase-3
#' `removeInvalidGaze()`'s `TobiiPro` branch (`R/removeInvalidGaze.R`): for
#' each eye, samples with a `"Invalid"` `validityLeft`/`validityRight` value
#' are masked to `NA` in new `.valid`-suffixed columns (P1-05's decision;
#' raw columns are left untouched). It additionally computes the new
#' `confidenceLeft`/`confidenceRight` (0-1) columns from
#' `?eyeQuality-schema` -- see `.tobii_pro_confidence()` below for the
#' NA-validity mapping decision this task resolves.
#'
#' @name tobii_pro_adapter
#' @keywords internal
NULL

.tobii_pro_detect <- function(data) {
  "Recording software version" %in% names(data)
}

# Raw (pre-rename) Tobii Pro columns worth checking for the "present but
# 100% NA" diagnostic (P3-10) -- the measurement columns a user would
# actually want to know are empty, not every column on the raw export
# (e.g. Event is legitimately all-NA on files with no logged events).
.tobii_pro_raw_measurement_cols <- c(
  "Gaze point left X", "Gaze point left Y",
  "Gaze point right X", "Gaze point right Y",
  "Eye position left Z (DACSmm)", "Eye position right Z (DACSmm)",
  "Pupil diameter left", "Pupil diameter right",
  "Validity left", "Validity right"
)

.tobii_pro_standardize <- function(data, verbose = FALSE) {
  .diagnose_all_na_columns(data, .tobii_pro_raw_measurement_cols, verbose)

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
#' @param verbose logical(1). When `TRUE`, emits P3-10 diagnostics about the
#'   gaze/event split via `.emit_diagnostic()`. Default `FALSE`.
#' @return `list(gaze, events)` -- see `?eyeQuality-schema`.
#' @keywords internal
.tobii_pro_extract_events <- function(data, verbose = FALSE) {
  gazeStreamData <- data %>%
    dplyr::filter(.data$Sensor == "Eye Tracker")
  eventData <- data %>%
    dplyr::filter(.data$Sensor != "Eye Tracker" |
      is.na(.data$Sensor))

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
        "%) -- check the Sensor column for unexpected values"
      ),
      verbose
    )
  }

  list(gaze = gazeStreamData, events = eventData)
}

#' Mask invalid samples for one eye (Tobii Pro)
#'
#' @description
#' Ported unchanged from the pre-Phase-3 `removeInvalidGaze()`'s `TobiiPro`
#' branch (`R/removeInvalidGaze.R`): every raw column whose name contains
#' `whichEye` (case-insensitive) and is not itself a validity column gets a
#' `.valid`-suffixed copy, masked to `NA` wherever the matching eye's
#' `validityLeft`/`validityRight` value is `"Invalid"`. Raw columns are left
#' byte-identical (P1-05).
#'
#' @param data standardized data.frame (post `standardize()`).
#' @param whichEye `"left"` or `"right"`.
#' @param verbose logical(1). When `TRUE`, emits P3-10 diagnostics: runs of
#'   consecutive `"Invalid"` samples (via `.diagnose_consecutive_runs()`)
#'   and a note about the documented `NA`-validity quirk (see
#'   `?eyeQuality-schema`'s `validityLeft`/`validityRight` entry) when it
#'   actually affects this eye's data. Default `FALSE`.
#' @return `data` with new `.valid`-suffixed columns for `whichEye`.
#' @keywords internal
.tobii_pro_mask_eye <- function(data, whichEye, verbose = FALSE) {
  cols <- colnames(data)[grepl(whichEye, colnames(data), ignore.case = TRUE) &
    !grepl("valid", colnames(data))]
  validityCol <- colnames(data)[grepl(whichEye, colnames(data), ignore.case = TRUE) &
    grepl("valid", colnames(data))]
  rawValidity <- data[[validityCol]]

  maskFlag <- tidyr::replace_na(rawValidity == "Invalid", FALSE)
  .diagnose_consecutive_runs(
    maskFlag,
    paste0(whichEye, " eye validity == \"Invalid\""),
    verbose
  )
  naFlag <- is.na(rawValidity)
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
    replaceRows <- data[, validityCol] == "Invalid"
    data[[newCol]] <- data[[i]]
    data[[newCol]][tidyr::replace_na(replaceRows, FALSE)] <- NA
  }
  data
}

#' Compute the generic `confidence` (0-1) columns from Tobii Pro's native
#' `"Valid"`/`"Invalid"` validity strings
#'
#' @description
#' `confidenceLeft`/`confidenceRight <- ifelse(validityLeft/validityRight ==
#' "Valid", 1, 0)` -- see the `confidence` entry in `?eyeQuality-schema` for
#' the full 0-1 design rationale. As on the Tobii Studio adapter (see
#' `.tobii_studio_confidence()`), this emits per-eye `confidenceLeft`/
#' `confidenceRight` columns rather than a single combined `confidence`
#' value, consistent with every other per-eye pair in the schema.
#'
#' **NA-validity-to-confidence mapping (P3-06 decision):** `confidence` is
#' set to `1` for `NA` validity, matching option (a) in the OPEN DECISION
#' section of `?eyeQuality-schema`. Rationale: the *existing* `.valid`-
#' masking behavior (`.tobii_pro_mask_eye()` above, ported unchanged from
#' `removeInvalidGaze()`) already treats `NA` validity as unmasked --
#' effectively "valid" -- because `validityCol == "Invalid"` is `NA` when
#' `validityCol` is `NA`, and `tidyr::replace_na(replaceRows, FALSE)` then
#' coerces that `NA` to `FALSE`, so the sample is not masked.
#' `confidence = 1` for `NA` validity keeps this new column in agreement
#' with what masking actually does today, rather than introducing a second,
#' disagreeing notion of validity for the same input. See
#' `.tobii_studio_confidence()`'s roxygen for the full reasoning behind
#' choosing this option over `confidence = 0` -- identical logic applies
#' here; this is the same deliberate, documented choice made consistently
#' for both adapters, not an independent per-adapter decision.
#'
#' @param data standardized data.frame with `validityLeft`/`validityRight`.
#' @return `data` with new `confidenceLeft`/`confidenceRight` columns.
#' @keywords internal
.tobii_pro_confidence <- function(data) {
  data$confidenceLeft <- ifelse(data$validityLeft == "Valid", 1, 0)
  data$confidenceLeft[is.na(data$validityLeft)] <- 1
  data$confidenceRight <- ifelse(data$validityRight == "Valid", 1, 0)
  data$confidenceRight[is.na(data$validityRight)] <- 1
  data
}

#' Normalize Tobii Pro validity into `.valid`-masked columns and `confidence`
#'
#' @description
#' Ported unchanged from the pre-Phase-3 `removeInvalidGaze()`'s `TobiiPro`
#' branch (`R/removeInvalidGaze.R`), operating on both eyes in a single call
#' per the adapter interface (`?new_eyetracker_adapter`). `threshold` is
#' accepted for interface-signature compatibility but unused, matching the
#' pre-Phase-3 function: Tobii Pro's validity is a binary
#' `"Valid"`/`"Invalid"` string with no numeric threshold concept (see this
#' adapter's `default_thresholds`, an empty list).
#'
#' The old `removeInvalidGaze()` took a `software` argument and had a final
#' `else stop(...)` (P1-04) guarding against an unrecognized value, so bad
#' input failed loudly instead of silently returning unmodified data. That
#' explicit branch doesn't carry forward here for the same structural reason
#' documented on `.tobii_pro_extract_events()`: this method has no
#' `software` argument to dispatch on, because the adapter registry already
#' selected this adapter specifically for Tobii Pro data. The underlying
#' intent is preserved structurally instead: if this method is called on
#' data lacking `validityLeft`/`validityRight` columns (e.g. mismatched
#' input), `validityCol` resolves to `character(0)` inside
#' `.tobii_pro_mask_eye()` and the subsequent `data[, validityCol]` lookup
#' errors immediately (`"undefined columns selected"`), rather than silently
#' proceeding with no masking applied.
#'
#' @param data standardized data.frame (post `standardize()`).
#' @param threshold accepted but unused (see description).
#' @param verbose logical(1). When `TRUE`, forwards to
#'   `.tobii_pro_mask_eye()` for both eyes to emit P3-10 diagnostics.
#'   Default `FALSE`.
#' @return `data` with new `.valid`-suffixed masked columns and new
#'   `confidenceLeft`/`confidenceRight` columns.
#' @keywords internal
.tobii_pro_normalize_validity <- function(data, threshold = NULL, verbose = FALSE) {
  data <- .tobii_pro_mask_eye(data, "left", verbose)
  data <- .tobii_pro_mask_eye(data, "right", verbose)
  data <- .tobii_pro_confidence(data)
  data
}

# Tobii Pro's native validity scale is the binary "Valid"/"Invalid" string
# used by the pre-Phase-3 `removeInvalidGaze()` (R/removeInvalidGaze.R) --
# there is no numeric threshold concept for this device, so no
# `default_thresholds` entry is set here.
tobii_pro_adapter <- new_eyetracker_adapter(
  name = "TobiiPro",
  detect = .tobii_pro_detect,
  standardize = .tobii_pro_standardize,
  extract_events = .tobii_pro_extract_events,
  normalize_validity = .tobii_pro_normalize_validity,
  geometry_type = "screen",
  default_thresholds = list()
)
