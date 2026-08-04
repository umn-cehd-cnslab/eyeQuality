# eyeSelection() combines left/right eye gaze, pupil, and distance streams
# into a single "eyeSelect" stream according to one of four strategies. All
# expected values below were hand-derived from the function's documented
# per-method logic and confirmed by direct execution.
#
# Shared 3-row fixture used across all four methods:
#   row 1: both eyes present (left != right, to catch an accidental
#           left/right swap)
#   row 2: left eye missing, right eye present
#   row 3: both eyes missing

testData <- function() {
  data.frame(
    gazeLeftX.int = c(10, NA, NA),
    gazeLeftY.int = c(20, NA, NA),
    gazeRightX.int = c(12, 15, NA),
    gazeRightY.int = c(22, 25, NA),
    pupilLeft.int = c(3, NA, NA),
    pupilRight.int = c(3.4, 3.2, NA),
    distanceLeftZ.int = c(600, NA, NA),
    distanceRightZ.int = c(602, 605, NA)
  )
}

test_that("eyeSelection Maximize averages both eyes when present and falls back to the available eye otherwise", {
  result <- eyeSelection(testData(), eyeSelection_method = "Maximize")

  expect_equal(result$gazeX.eyeSelect, c(11, 15, NaN))
  expect_equal(result$gazeY.eyeSelect, c(21, 25, NaN))
  expect_equal(result$pupil.eyeSelect, c(3.2, 3.2, NaN))
  expect_equal(result$distanceZ.eyeSelect, c(601, 605, NaN))
  expect_equal(
    result$gazeX.es.selection,
    c("mean_maximized", "right_only", "both_na")
  )
})

test_that("eyeSelection Strict returns NA for a row wherever either eye is missing", {
  result <- eyeSelection(testData(), eyeSelection_method = "Strict")

  expect_equal(result$gazeX.eyeSelect, c(11, NA, NA))
  expect_equal(result$gazeY.eyeSelect, c(21, NA, NA))
  expect_equal(result$pupil.eyeSelect, c(3.2, NA, NA))
  expect_equal(result$distanceZ.eyeSelect, c(601, NA, NA))
  expect_equal(
    result$gazeX.es.selection,
    c("mean_strict", "left_na", "both_na")
  )
})

test_that("eyeSelection Left always uses the left eye stream, regardless of the right eye's data", {
  result <- eyeSelection(testData(), eyeSelection_method = "Left")

  expect_equal(result$gazeX.eyeSelect, c(10, NA, NA))
  expect_equal(result$gazeY.eyeSelect, c(20, NA, NA))
  expect_equal(result$pupil.eyeSelect, c(3, NA, NA))
  expect_equal(result$distanceZ.eyeSelect, c(600, NA, NA))
  expect_equal(
    result$gazeX.es.selection,
    c("left_only", "left_na", "left_na")
  )
})

test_that("eyeSelection Right always uses the right eye stream, regardless of the left eye's data", {
  result <- eyeSelection(testData(), eyeSelection_method = "Right")

  expect_equal(result$gazeX.eyeSelect, c(12, 15, NA))
  expect_equal(result$gazeY.eyeSelect, c(22, 25, NA))
  expect_equal(result$pupil.eyeSelect, c(3.4, 3.2, NA))
  expect_equal(result$distanceZ.eyeSelect, c(602, 605, NA))
  expect_equal(
    result$gazeX.es.selection,
    c("right_only", "right_only", "right_na")
  )
})
