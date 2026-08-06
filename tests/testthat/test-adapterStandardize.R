# Regression coverage for the adapter standardize() implementations
# (.tobii_studio_standardize() in R/tobii-studio-adapter.R,
# .tobii_pro_standardize() in R/tobii-pro-adapter.R), which rename the
# device-native raw columns onto the generic schema (?eyeQuality-schema).
#
# Originally (P3-04) these tests asserted the adapter output was equivalent
# to the old standalone standardizeColumnNames()/renameColumns() path, which
# was still live in the eyeQuality() pipeline at the time. P3-07 rewired the
# pipeline to call the adapter registry directly, retiring that old path from
# live use, so these tests now assert the adapter standardize() output
# against literal expected values on small hand-constructed data frames
# (using each device's real raw column names, since that's the whole point
# of standardize()) instead of re-deriving expected values by calling the
# now-pipeline-dead standardizeColumnNames()/renameColumns().

test_that("TobiiStudio adapter standardize() renames raw columns onto the generic schema and splits RecordingResolution", {
  data <- data.frame(
    StudioEvent = c(NA, "Task Start"),
    StudioEventData = c(NA, "value1"),
    RecordingDuration = c(100, 200),
    RecordingResolution = c("1920 x 1080", "1920 x 1080"),
    EyeTrackerTimestamp = c(0, 17),
    RecordingTimestamp = c(0, 17),
    "GazePointLeftX (ADCSpx)" = c(500, 510),
    "GazePointLeftY (ADCSpx)" = c(300, 310),
    "GazePointRightX (ADCSpx)" = c(505, 515),
    "GazePointRightY (ADCSpx)" = c(305, 315),
    "EyePosLeftZ (ADCSmm)" = c(600, 601),
    "EyePosRightZ (ADCSmm)" = c(602, 603),
    PupilLeft = c(3.1, 3.2),
    PupilRight = c(3.3, 3.4),
    ValidityLeft = c(0, 1),
    ValidityRight = c(0, 2),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- tobii_studio_adapter$standardize(data)

  expect_equal(result$event, c(NA, "Task Start"))
  expect_equal(result$eventValue, c(NA, "value1"))
  expect_equal(result$recordingDuration_ms, c(100, 200))
  expect_equal(result$eyeTrackerTimestamp, c(0, 17))
  expect_equal(result$recordingTimestamp_ms, c(0, 17))
  expect_equal(result$gazeLeftX, c(500, 510))
  expect_equal(result$gazeLeftY, c(300, 310))
  expect_equal(result$gazeRightX, c(505, 515))
  expect_equal(result$gazeRightY, c(305, 315))
  expect_equal(result$distanceLeftZ, c(600, 601))
  expect_equal(result$distanceRightZ, c(602, 603))
  expect_equal(result$pupilLeft, c(3.1, 3.2))
  expect_equal(result$pupilRight, c(3.3, 3.4))
  expect_equal(result$validityLeft, c(0, 1))
  expect_equal(result$validityRight, c(0, 2))
  # RecordingResolution is split into two integer columns rather than renamed
  expect_equal(result$resolutionWidth, c(1920L, 1920L))
  expect_equal(result$resolutionHeight, c(1080L, 1080L))
  expect_false("RecordingResolution" %in% names(result))
})

test_that("TobiiStudio adapter standardize() splits RecordingResolution values that differ by row", {
  data <- data.frame(
    StudioEvent = NA,
    StudioEventData = NA,
    RecordingDuration = 100,
    RecordingResolution = "1280 x 1024",
    EyeTrackerTimestamp = 0,
    RecordingTimestamp = 0,
    "GazePointLeftX (ADCSpx)" = 500,
    "GazePointLeftY (ADCSpx)" = 300,
    "GazePointRightX (ADCSpx)" = 505,
    "GazePointRightY (ADCSpx)" = 305,
    "EyePosLeftZ (ADCSmm)" = 600,
    "EyePosRightZ (ADCSmm)" = 602,
    PupilLeft = 3.1,
    PupilRight = 3.3,
    ValidityLeft = 0,
    ValidityRight = 0,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- tobii_studio_adapter$standardize(data)

  expect_equal(result$resolutionWidth, 1280L)
  expect_equal(result$resolutionHeight, 1024L)
})

test_that("TobiiPro adapter standardize() renames raw columns onto the generic schema", {
  data <- data.frame(
    Event = c(NA, "Task Start"),
    "Event value" = c(NA, "value1"),
    "Recording duration" = c(100, 200),
    "Recording resolution height" = c(1080, 1080),
    "Recording resolution width" = c(1920, 1920),
    "Eyetracker timestamp" = c(0, 17),
    "Recording timestamp" = c(0, 17),
    "Gaze point left X" = c(500, 510),
    "Gaze point left Y" = c(300, 310),
    "Gaze point right X" = c(505, 515),
    "Gaze point right Y" = c(305, 315),
    "Eye position left Z (DACSmm)" = c(600, 601),
    "Eye position right Z (DACSmm)" = c(602, 603),
    "Pupil diameter left" = c(3.1, 3.2),
    "Pupil diameter right" = c(3.3, 3.4),
    "Validity left" = c("Valid", "Invalid"),
    "Validity right" = c("Valid", "Valid"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- tobii_pro_adapter$standardize(data)

  expect_equal(result$event, c(NA, "Task Start"))
  expect_equal(result$eventValue, c(NA, "value1"))
  expect_equal(result$recordingDuration_ms, c(100, 200))
  expect_equal(result$resolutionHeight, c(1080, 1080))
  expect_equal(result$resolutionWidth, c(1920, 1920))
  expect_equal(result$eyeTrackerTimestamp, c(0, 17))
  expect_equal(result$recordingTimestamp_ms, c(0, 17))
  expect_equal(result$gazeLeftX, c(500, 510))
  expect_equal(result$gazeLeftY, c(300, 310))
  expect_equal(result$gazeRightX, c(505, 515))
  expect_equal(result$gazeRightY, c(305, 315))
  expect_equal(result$distanceLeftZ, c(600, 601))
  expect_equal(result$distanceRightZ, c(602, 603))
  expect_equal(result$pupilLeft, c(3.1, 3.2))
  expect_equal(result$pupilRight, c(3.3, 3.4))
  expect_equal(result$validityLeft, c("Valid", "Invalid"))
  expect_equal(result$validityRight, c("Valid", "Valid"))
})

# --- P3-10 verbose diagnostics ----------------------------------------------
# standardize()'s only verbose behavior is the "column present but 100% NA"
# check (.diagnose_all_na_columns()) against each adapter's raw measurement
# column list, run before renaming.

test_that("TobiiStudio adapter standardize(verbose = TRUE) reports a raw measurement column that is present but entirely NA", {
  data <- data.frame(
    StudioEvent = NA,
    StudioEventData = NA,
    RecordingDuration = 100,
    RecordingResolution = "1920 x 1080",
    EyeTrackerTimestamp = 0,
    RecordingTimestamp = 0,
    "GazePointLeftX (ADCSpx)" = 500,
    "GazePointLeftY (ADCSpx)" = 300,
    "GazePointRightX (ADCSpx)" = 505,
    "GazePointRightY (ADCSpx)" = 305,
    "EyePosLeftZ (ADCSmm)" = 600,
    "EyePosRightZ (ADCSmm)" = 602,
    PupilLeft = NA_real_,
    PupilRight = 3.3,
    ValidityLeft = 0,
    ValidityRight = 0,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_output(
    tobii_studio_adapter$standardize(data, verbose = TRUE),
    "column 'PupilLeft' present but 100% NA"
  )
})

test_that("TobiiStudio adapter standardize(verbose = FALSE) (default) emits no diagnostics even with an all-NA raw column", {
  data <- data.frame(
    StudioEvent = NA,
    StudioEventData = NA,
    RecordingDuration = 100,
    RecordingResolution = "1920 x 1080",
    EyeTrackerTimestamp = 0,
    RecordingTimestamp = 0,
    "GazePointLeftX (ADCSpx)" = 500,
    "GazePointLeftY (ADCSpx)" = 300,
    "GazePointRightX (ADCSpx)" = 505,
    "GazePointRightY (ADCSpx)" = 305,
    "EyePosLeftZ (ADCSmm)" = 600,
    "EyePosRightZ (ADCSmm)" = 602,
    PupilLeft = NA_real_,
    PupilRight = 3.3,
    ValidityLeft = 0,
    ValidityRight = 0,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_silent(tobii_studio_adapter$standardize(data))
})

test_that("TobiiPro adapter standardize(verbose = TRUE) reports a raw measurement column that is present but entirely NA", {
  data <- data.frame(
    Event = NA,
    "Event value" = NA,
    "Recording duration" = 100,
    "Recording resolution height" = 1080,
    "Recording resolution width" = 1920,
    "Eyetracker timestamp" = 0,
    "Recording timestamp" = 0,
    "Gaze point left X" = 500,
    "Gaze point left Y" = 300,
    "Gaze point right X" = 505,
    "Gaze point right Y" = 305,
    "Eye position left Z (DACSmm)" = 600,
    "Eye position right Z (DACSmm)" = 602,
    "Pupil diameter left" = NA_real_,
    "Pupil diameter right" = 3.3,
    "Validity left" = "Valid",
    "Validity right" = "Valid",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_output(
    tobii_pro_adapter$standardize(data, verbose = TRUE),
    "column 'Pupil diameter left' present but 100% NA"
  )
})

test_that("TobiiPro adapter standardize(verbose = FALSE) (default) emits no diagnostics even with an all-NA raw column", {
  data <- data.frame(
    Event = NA,
    "Event value" = NA,
    "Recording duration" = 100,
    "Recording resolution height" = 1080,
    "Recording resolution width" = 1920,
    "Eyetracker timestamp" = 0,
    "Recording timestamp" = 0,
    "Gaze point left X" = 500,
    "Gaze point left Y" = 300,
    "Gaze point right X" = 505,
    "Gaze point right Y" = 305,
    "Eye position left Z (DACSmm)" = 600,
    "Eye position right Z (DACSmm)" = 602,
    "Pupil diameter left" = NA_real_,
    "Pupil diameter right" = 3.3,
    "Validity left" = "Valid",
    "Validity right" = "Valid",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  expect_silent(tobii_pro_adapter$standardize(data))
})

test_that("TobiiPro adapter standardize() does not alter row count or introduce/lose columns beyond renaming", {
  data <- data.frame(
    Event = NA,
    "Event value" = NA,
    "Recording duration" = 100,
    "Recording resolution height" = 1080,
    "Recording resolution width" = 1920,
    "Eyetracker timestamp" = 0,
    "Recording timestamp" = 0,
    "Gaze point left X" = 500,
    "Gaze point left Y" = 300,
    "Gaze point right X" = 505,
    "Gaze point right Y" = 305,
    "Eye position left Z (DACSmm)" = 600,
    "Eye position right Z (DACSmm)" = 602,
    "Pupil diameter left" = 3.1,
    "Pupil diameter right" = 3.3,
    "Validity left" = "Valid",
    "Validity right" = "Valid",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- tobii_pro_adapter$standardize(data)

  expect_equal(nrow(result), nrow(data))
  expect_equal(ncol(result), ncol(data))
})
