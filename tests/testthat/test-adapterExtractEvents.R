# Regression coverage for the adapter extract_events() implementations
# (.tobii_studio_extract_events() in R/tobii-studio-adapter.R,
# .tobii_pro_extract_events() in R/tobii-pro-adapter.R), which split
# standardized data into gaze-stream rows vs. event rows.
#
# Originally (P3-05) these tests asserted the adapter output was equivalent
# to the old standalone extractEventRows() path, which was still live in the
# eyeQuality() pipeline at the time. P3-07 rewired the pipeline to call the
# adapter registry directly, retiring that old path from live use, so these
# tests now assert the adapter extract_events() output against literal
# expected values on small hand-constructed data frames instead of
# re-deriving expected values by calling the now-pipeline-dead
# extractEventRows().

test_that("TobiiStudio adapter extract_events() splits gaze rows from event rows using the eyeTrackerTimestamp sentinel", {
  data <- data.frame(
    eyeTrackerTimestamp = c(0, 17, -9999, NA),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  result <- tobii_studio_adapter$extract_events(data)

  expect_equal(nrow(result$gaze), 2)
  expect_equal(nrow(result$events), 2)
  expect_equal(result$gaze$eyeTrackerTimestamp, c(0, 17))
  expect_equal(result$gaze$gazeLeftX, c(1, 2))
  expect_equal(result$events$eyeTrackerTimestamp, c(-9999, NA))
  expect_equal(result$events$gazeLeftX, c(3, 4))
})

test_that("TobiiStudio adapter extract_events() returns all rows as gaze when no sentinel/NA rows are present", {
  data <- data.frame(
    eyeTrackerTimestamp = c(0, 17, 34),
    gazeLeftX = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  result <- tobii_studio_adapter$extract_events(data)

  expect_equal(nrow(result$gaze), 3)
  expect_equal(nrow(result$events), 0)
})

test_that("TobiiPro adapter extract_events() splits gaze rows from event rows using the Sensor column", {
  data <- data.frame(
    Sensor = c("Eye Tracker", "Eye Tracker", "Mouse", NA),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  result <- tobii_pro_adapter$extract_events(data)

  expect_equal(nrow(result$gaze), 2)
  expect_equal(nrow(result$events), 2)
  expect_equal(result$gaze$gazeLeftX, c(1, 2))
  expect_equal(result$events$Sensor, c("Mouse", NA))
  expect_equal(result$events$gazeLeftX, c(3, 4))
})

test_that("TobiiPro adapter extract_events() returns all rows as gaze when Sensor is always 'Eye Tracker'", {
  data <- data.frame(
    Sensor = c("Eye Tracker", "Eye Tracker"),
    gazeLeftX = c(1, 2),
    stringsAsFactors = FALSE
  )

  result <- tobii_pro_adapter$extract_events(data)

  expect_equal(nrow(result$gaze), 2)
  expect_equal(nrow(result$events), 0)
})

# --- P3-10 verbose diagnostics ----------------------------------------------
# extract_events()'s verbose behavior: an unconditional gaze/event split
# summary line, plus an extra warning when more than half the rows are
# classified as events (a signal something is probably wrong with the
# device-specific discriminator, not a normal recording).

test_that("TobiiStudio adapter extract_events(verbose = TRUE) reports the gaze/event row split", {
  data <- data.frame(
    eyeTrackerTimestamp = c(0, 17, -9999, NA),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  expect_output(
    tobii_studio_adapter$extract_events(data, verbose = TRUE),
    "extract_events: 4 row\\(s\\) split into 2 gaze-stream row\\(s\\) and 2 event row\\(s\\)"
  )
})

test_that("TobiiStudio adapter extract_events(verbose = TRUE) warns when more than half the rows are classified as events", {
  data <- data.frame(
    eyeTrackerTimestamp = c(0, -9999, -9999, -9999),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  expect_output(
    tobii_studio_adapter$extract_events(data, verbose = TRUE),
    "unusually high proportion of rows classified as events \\(75%\\)"
  )
})

test_that("TobiiStudio adapter extract_events(verbose = TRUE) does not warn about event proportion when events are a normal minority", {
  data <- data.frame(
    eyeTrackerTimestamp = c(0, 17, 34, -9999),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  out <- capture.output(tobii_studio_adapter$extract_events(data, verbose = TRUE))

  expect_false(any(grepl("unusually high proportion", out)))
})

test_that("TobiiStudio adapter extract_events(verbose = FALSE) (default) emits no diagnostics", {
  data <- data.frame(
    eyeTrackerTimestamp = c(0, -9999, -9999, -9999),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  expect_silent(tobii_studio_adapter$extract_events(data))
})

test_that("TobiiPro adapter extract_events(verbose = TRUE) reports the gaze/event row split", {
  data <- data.frame(
    Sensor = c("Eye Tracker", "Eye Tracker", "Mouse", NA),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  expect_output(
    tobii_pro_adapter$extract_events(data, verbose = TRUE),
    "extract_events: 4 row\\(s\\) split into 2 gaze-stream row\\(s\\) and 2 event row\\(s\\)"
  )
})

test_that("TobiiPro adapter extract_events(verbose = TRUE) warns when more than half the rows are classified as events", {
  data <- data.frame(
    Sensor = c("Eye Tracker", "Mouse", "Mouse", "Mouse"),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  expect_output(
    tobii_pro_adapter$extract_events(data, verbose = TRUE),
    "unusually high proportion of rows classified as events \\(75%\\)"
  )
})

test_that("TobiiPro adapter extract_events(verbose = FALSE) (default) emits no diagnostics", {
  data <- data.frame(
    Sensor = c("Eye Tracker", "Mouse", "Mouse", "Mouse"),
    gazeLeftX = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  expect_silent(tobii_pro_adapter$extract_events(data))
})

# --- Error-path coverage --------------------------------------------------
# Adapter-world analog of P1-03's extractEventRows() regression test
# (test-extractEventRows.R's "errors immediately on an unrecognized software
# value" test). The new adapters have no `software` argument to dispatch on
# -- the registry already selects the right adapter -- so the equivalent
# failure mode is calling extract_events() on data that is missing the
# device-specific discriminator column it filters on. Both should fail
# loudly (naming the missing column) rather than silently returning garbage
# or an opaque downstream error.

test_that("TobiiStudio adapter extract_events() errors clearly when eyeTrackerTimestamp is missing", {
  data <- data.frame(someOtherColumn = 1:3)

  expect_error(
    tobii_studio_adapter$extract_events(data),
    regexp = "eyeTrackerTimestamp"
  )
})

test_that("TobiiPro adapter extract_events() errors clearly when Sensor is missing", {
  data <- data.frame(someOtherColumn = 1:3)

  expect_error(
    tobii_pro_adapter$extract_events(data),
    regexp = "Sensor"
  )
})
