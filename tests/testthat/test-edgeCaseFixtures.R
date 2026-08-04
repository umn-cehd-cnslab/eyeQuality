# P2-04: Edge-case fixtures for classifyBlinks() boundaries, out-of-order
# timestamps, empty/all-NA gaze data, unidentifiable start/end events, and an
# unrecognized column schema. Each fixture is paired with a test asserting
# the specific behavior it exists to exercise, verified against the actual
# current pipeline behavior (not an assumption) -- see individual test
# comments below for what was confirmed.
#
# P1-14 note: the (b)/(c)/(d) `eyeQuality aborts (via stop())` tests below
# assert on conditionMessage(err) content directly, covering
# R/eyeQuality.R's four diagnosticText call sites now that they call
# stop(diagnosticText) instead of print(diagnosticText) + bare stop().

# --- (a) blink boundary fixture -------------------------------------------
#
# tests/testthat/fixtures/blink_boundary_sample.csv holds pre-interpolated
# pupil data (the exact column shape classifyBlinks() expects: pupilLeft.int /
# pupilRight.int, as produced upstream by interpolateGaze()) at a clean
# 100 Hz sampling rate (10ms/sample), so classifyBlinks()'s default
# thresholds land on exact sample counts:
#   sampling_interval = 10ms
#   blink_length_min = minBlinkLength_ms(100) / 10  = 10 samples
#   blink_length_max = maxBlinkLength_ms(400) / 10  = 40 samples
#   artifact_interval = round(maxArtifactLength_ms(15) / 10) = 2 samples
#
# It contains three deliberately-sized missing-data segments, each against a
# smoothly oscillating (non-flat) pupil baseline -- classifyBlinks()'s onset/
# offset detection walks outward from a candidate gap looking for the nearest
# real rise/decline in the *smoothed* signal, so a perfectly flat baseline
# degenerates to "whole recording is one candidate blink" and defeats the
# thresholds entirely; a small sinusoidal baseline keeps onset/offset
# detection local to each gap the way real pupillometry noise would.
#   1. rows 301-320: a 200ms (20-sample) gap -- within [100, 400]ms, so it
#      IS classified as a blink (minBlinkLength_ms/maxBlinkLength_ms allow it).
#   2. rows 501-600: a 1000ms (100-sample) dropout -- far beyond
#      maxBlinkLength_ms(400ms), so it is NOT classified as a blink.
#   3. rows 701-708 / valid row 709 / rows 710-717: two 80ms (8-sample) gaps
#      either side of a single 10ms valid sample. Neither 80ms sub-gap alone
#      reaches minBlinkLength_ms(100ms), but the 10ms gap between them is
#      <= maxArtifactLength_ms(15ms) and gets merged into a single blink
#      span by classifyBlinks()'s artifact-removal step, so the *combined*
#      span (including the still-valid row 709) is classified as one blink.
#
# Exact expected row ranges below were established by running
# classifyBlinks() directly against this fixture and confirming the output.

test_that("classifyBlinks flags a clean within-threshold gap as a blink", {
  fp <- testthat::test_path("fixtures", "blink_boundary_sample.csv")
  pupilData <- read.csv(fp)

  result <- classifyBlinks(
    pupilData,
    pupilLeft = "pupilLeft.int",
    pupilRight = "pupilRight.int",
    recordingFrequency_hz = 100
  )

  expect_true(all(result$pupilLeft.blink[298:321] == 1))
  expect_true(all(result$bothEyes.blink[298:321] == 1))
  # nothing outside that span, up to the edge of the buffer, should be
  # flagged
  expect_true(all(result$pupilLeft.blink[280:297] == 0))
  expect_true(all(result$pupilLeft.blink[322:340] == 0))
})

test_that("classifyBlinks does NOT flag a dropout far beyond maxBlinkLength_ms as a blink", {
  fp <- testthat::test_path("fixtures", "blink_boundary_sample.csv")
  pupilData <- read.csv(fp)

  result <- classifyBlinks(
    pupilData,
    pupilLeft = "pupilLeft.int",
    pupilRight = "pupilRight.int",
    recordingFrequency_hz = 100
  )

  expect_true(all(result$pupilLeft.blink[501:600] == 0))
  expect_true(all(result$bothEyes.blink[501:600] == 0))
})

test_that("classifyBlinks merges two sub-threshold gaps across a maxArtifactLength_ms blip into one blink", {
  fp <- testthat::test_path("fixtures", "blink_boundary_sample.csv")
  pupilData <- read.csv(fp)

  result <- classifyBlinks(
    pupilData,
    pupilLeft = "pupilLeft.int",
    pupilRight = "pupilRight.int",
    recordingFrequency_hz = 100
  )

  # the still-valid "blip" row (709) gets absorbed into the merged blink span
  expect_equal(result$pupilLeft.blink[709], 1)
  expect_true(all(result$pupilLeft.blink[693:718] == 1))
  expect_true(all(result$pupilLeft.blink[680:692] == 0))
  expect_true(all(result$pupilLeft.blink[719:730] == 0))
})

# --- (b) out-of-order timestamps fixture -----------------------------------
#
# tests/testthat/fixtures/tobii_studio_out_of_order_timestamps.tsv is a
# 10-row Tobii Studio recording identical to tobii_studio_sample.tsv's first
# 10 rows, except rows 5 and 6's RecordingTimestamp values are swapped
# (..., 51, 85, 68, 102, ...), so the timestamp column is not monotonically
# increasing.
#
# checkOrderedTimestamps() (R/checkTimestamps.R) itself just returns a
# logical -- it does not error. eyeQuality() calls it and, on FALSE, prints
# and stop()s with the same descriptive diagnosticText (R/eyeQuality.R,
# post-P1-14 fix -- previously that text only ever reached print(), and the
# raised condition's message was empty). Confirmed by running the fixture
# through both checkOrderedTimestamps() directly and the full eyeQuality()
# pipeline.

test_that("checkOrderedTimestamps returns FALSE for the out-of-order timestamps fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_out_of_order_timestamps.tsv")
  data <- importData(fp)
  software <- detectImportSourceType(data)
  data <- standardizeColumnNames(data, software)

  expect_false(checkOrderedTimestamps(data, timestamps = "recordingTimestamp_ms"))
})

test_that("eyeQuality aborts (via stop()) on the out-of-order timestamps fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_out_of_order_timestamps.tsv")

  err <- tryCatch(
    {
      eyeQuality(
        fp,
        displayDimensionX_mm = 594,
        displayDimensionY_mm = 344,
        saveData = FALSE
      )
      NULL
    },
    error = function(e) e
  )

  expect_s3_class(err, "simpleError")
  expect_equal(
    conditionMessage(err),
    paste0(
      "Data is not chronologically ordered based on timestamp. Pre-processing for file ",
      fp,
      " has been aborted."
    )
  )
})

# --- (c) empty / all-NA gaze fixture ----------------------------------------
#
# tests/testthat/fixtures/tobii_studio_all_na_gaze.tsv has valid Tobii Studio
# headers and 10 data rows, but every GazePointLeftX/Y and
# GazePointRightX/Y value is NA -- i.e. structurally valid, but zero usable
# gaze data for either eye.
#
# checkGazeDataExists() (R/checkGazeData.R) returns FALSE for both eyes in
# this case. eyeQuality() checks this immediately after
# standardizeColumnNames() and, like the timestamp check, aborts via
# stop(diagnosticText) carrying its own descriptive text (R/eyeQuality.R,
# post-P1-14 fix). Note the source text itself has no space between "file"
# and the filepath it concatenates -- that's the real, un-fixed message
# content, asserted as-is below rather than "corrected" in the test.

test_that("checkGazeDataExists returns FALSE for both eyes on the all-NA gaze fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_all_na_gaze.tsv")
  data <- importData(fp)
  software <- detectImportSourceType(data)
  data <- standardizeColumnNames(data, software)

  expect_false(checkGazeDataExists(data, gazeColumn = "gazeLeftX"))
  expect_false(checkGazeDataExists(data, gazeColumn = "gazeRightX"))
})

test_that("eyeQuality aborts (via stop()) on the all-NA gaze fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_all_na_gaze.tsv")

  err <- tryCatch(
    {
      eyeQuality(
        fp,
        displayDimensionX_mm = 594,
        displayDimensionY_mm = 344,
        saveData = FALSE
      )
      NULL
    },
    error = function(e) e
  )

  expect_s3_class(err, "simpleError")
  expect_equal(
    conditionMessage(err),
    paste0(
      "No valid gaze data exists. Preprocessing for file",
      fp,
      " has been aborted."
    )
  )
})

# --- (d) unidentifiable start/end event timestamps -------------------------
#
# Reuses tests/testthat/fixtures/tobii_studio_sample.tsv (its StudioEvent
# column is all-NA -- no events are present at all). Passing studioEvents
# AND proEvents naming event labels that don't exist anywhere in the file
# makes getEventTimes() return NA for both start and end times, so the
# `length(taskTimes) == 2 && !anyNA(taskTimes) && taskTimes[[1]] <
# taskTimes[[2]]` guard in eyeQuality() (R/eyeQuality.R) fails and it
# stop()s with a descriptive diagnosticText (post-P1-14 fix; previously that
# text only reached print(), and the raised condition's message was empty).
# Note both studioEvents and proEvents must be supplied together for this
# branch to run at all -- `!isempty(studioEvents) && !isempty(proEvents)` --
# confirmed by direct execution against this fixture.

test_that("eyeQuality aborts (via stop()) when named start/end events cannot be found", {
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")

  err <- tryCatch(
    {
      eyeQuality(
        fp,
        displayDimensionX_mm = 594,
        displayDimensionY_mm = 344,
        studioEvents = c("TaskStart", "TaskEnd"),
        proEvents = c("TaskStart", "TaskEnd"),
        saveData = FALSE
      )
      NULL
    },
    error = function(e) e
  )

  expect_s3_class(err, "simpleError")
  expect_equal(
    conditionMessage(err),
    paste0(
      "Start or end timestamps could not be identified. Pre-processing for file ",
      fp,
      " aborted."
    )
  )
})

# --- (e) unrecognized column schema fixture --------------------------------
#
# tests/testthat/fixtures/unrecognized_schema.csv has plausible-looking
# eye-tracking column names (Timestamp, LeftEyeX/Y, RightEyeX/Y,
# PupilDiameterLeft/Right) that don't match either Tobii Studio's
# ("StudioVersionRec") or Tobii Pro's ("Recording software version")
# detection column, so detectImportSourceType() (R/detectImportSourceType.R)
# hits its else branch and stop()s with a specific, descriptive message.

test_that("detectImportSourceType errors with a descriptive message on an unrecognized schema", {
  fp <- testthat::test_path("fixtures", "unrecognized_schema.csv")
  data <- importData(fp)

  expect_error(
    detectImportSourceType(data),
    "Data import does not match column names expected from Tobii Studio or Tobii Pro",
    fixed = TRUE
  )
})

test_that("eyeQuality aborts with the detectImportSourceType error on an unrecognized schema", {
  fp <- testthat::test_path("fixtures", "unrecognized_schema.csv")

  expect_error(
    eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE
    ),
    "Data import does not match column names expected from Tobii Studio or Tobii Pro",
    fixed = TRUE
  )
})
