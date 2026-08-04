# Regression test for P1-03: extractEventRows() previously fell through its
# if/else if chain with no final else, leaving gazeStreamData/eventData
# unassigned for any unrecognized `software` value. That produced an opaque
# "object not found" error later in the pipeline instead of a clear failure
# at the point of the actual problem. A final `else stop(...)` now closes
# that gap.

test_that("extractEventRows errors immediately on an unrecognized software value, naming it", {
  data <- data.frame(Sensor = "Eye Tracker", stringsAsFactors = FALSE)

  expect_error(
    extractEventRows(data, software = "bogus"),
    regexp = "bogus"
  )
})

test_that("extractEventRows still returns gaze/event data for TobiiPro", {
  data <- data.frame(
    Sensor = c("Eye Tracker", "Eye Tracker", "Mouse", NA),
    stringsAsFactors = FALSE
  )

  result <- expect_error(extractEventRows(data, software = "TobiiPro"), NA)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(nrow(result[[1]]), 2)
  expect_equal(nrow(result[[2]]), 2)
})

test_that("extractEventRows still returns gaze/event data for TobiiStudio", {
  data <- data.frame(
    eyeTrackerTimestamp = c(0, 17, -9999, NA),
    stringsAsFactors = FALSE
  )

  result <- expect_error(extractEventRows(data, software = "TobiiStudio"), NA)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_equal(nrow(result[[1]]), 2)
  expect_equal(nrow(result[[2]]), 2)
})
