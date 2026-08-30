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

# --- P3-10 verbose diagnostics ----------------------------------------------

test_that("interpolateGaze(verbose = TRUE) reports how many missing samples were filled and how many remain NA", {
  # same fixture/thresholds as the first test above: 2 of 8 total missing
  # samples get filled (the 2-NA gap), the other 6 (the 6-NA gap) remain NA.
  data <- data.frame(
    gazeLeftX.valid = c(1, NA, NA, 4, 5, NA, NA, NA, NA, NA, NA, 12)
  )

  out <- capture.output(
    interpolateGaze(
      data,
      recordingFrequency_hz = 100,
      columnsToInterpolate = "gazeLeftX.valid",
      verbose = TRUE
    )
  )

  expect_true(any(grepl(
    "interpolateGaze: 'gazeLeftX.valid' -- filled 2 of 8 missing sample\\(s\\); 6 remain NA",
    out
  )))
})

test_that("interpolateGaze(verbose = TRUE) emits no fill-report line for a column with no missing samples", {
  data <- data.frame(gazeLeftX.valid = c(1, 2, 3))

  out <- capture.output(
    interpolateGaze(
      data,
      recordingFrequency_hz = 100,
      columnsToInterpolate = "gazeLeftX.valid",
      verbose = TRUE
    )
  )

  expect_false(any(grepl("interpolateGaze:", out)))
})

test_that("interpolateGaze(verbose = FALSE) (default) emits no [verbose]-tagged diagnostic lines", {
  # interpolateGaze() has a pre-existing, non-verbose-gated progress print()
  # call ("Filling in gaps of ...ms..."), so this checks specifically for the
  # absence of the P3-10 "[verbose]" tag rather than full silence.
  data <- data.frame(
    gazeLeftX.valid = c(1, NA, NA, 4, 5, NA, NA, NA, NA, NA, NA, 12)
  )

  out <- capture.output(
    interpolateGaze(
      data,
      recordingFrequency_hz = 100,
      columnsToInterpolate = "gazeLeftX.valid"
    )
  )

  expect_false(any(grepl("\\[verbose\\]", out)))
})
