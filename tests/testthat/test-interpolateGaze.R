# interpolateGaze() gap-fills NA runs up to maxGapPoints (derived from
# maxGapLength_ms / sampling interval) via zoo::na.approx(), and derives the
# output column name by stripping a trailing ".valid" suffix from the input
# column name before appending ".int".

test_that("interpolateGaze linearly interpolates a gap within maxGapLength_ms and leaves a longer gap untouched", {
  # 100 Hz -> 10ms/sample. maxGapLength_ms = 50 (default) -> maxGapPoints = floor(50/10) = 5.
  data <- data.frame(
    gazeLeftX.valid = c(1, NA, NA, 4, 5, NA, NA, NA, NA, NA, NA, 12)
  )
  # gap at indices 2:3 is 2 NAs (<= 5) -> linearly interpolated between 1 and 4
  #   index2 = 1 + (4-1) * 1/3 = 2, index3 = 1 + (4-1) * 2/3 = 3
  # gap at indices 6:11 is 6 NAs (> 5) -> left as NA

  result <- interpolateGaze(
    data,
    recordingFrequency_hz = 100,
    columnsToInterpolate = "gazeLeftX.valid"
  )

  expect_equal(
    result$gazeLeftX.int,
    c(1, 2, 3, 4, 5, NA, NA, NA, NA, NA, NA, 12)
  )
})

test_that("interpolateGaze strips a trailing .valid suffix instead of appending .int after it", {
  data <- data.frame(gazeLeftX.valid = c(1, NA, 3))

  result <- interpolateGaze(
    data,
    recordingFrequency_hz = 100,
    columnsToInterpolate = "gazeLeftX.valid"
  )

  expect_true("gazeLeftX.int" %in% names(result))
  expect_false("gazeLeftX.valid.int" %in% names(result))
})

test_that("interpolateGaze respects a custom maxGapLength_ms threshold", {
  # 100 Hz -> 10ms/sample. maxGapLength_ms = 20 -> maxGapPoints = floor(20/10) = 2.
  data <- data.frame(pupilLeft.int = c(1, NA, NA, NA, 5))
  # gap of 3 NAs exceeds maxGapPoints (2), so it should remain NA

  result <- interpolateGaze(
    data,
    recordingFrequency_hz = 100,
    columnsToInterpolate = "pupilLeft.int",
    maxGapLength_ms = 20
  )

  expect_equal(result$pupilLeft.int.int, c(1, NA, NA, NA, 5))
})
