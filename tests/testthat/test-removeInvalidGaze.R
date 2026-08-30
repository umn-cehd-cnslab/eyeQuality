# Regression test for P1-04: removeInvalidGaze() previously fell through its
# if/else if chain with no final else, silently returning `data` unmodified
# for any unrecognized `software` value -- no invalid points removed, no
# error. That let bad data pass QC undetected, which is worse than a crash.
# A final `else stop(...)` now closes that gap.

test_that("removeInvalidGaze errors on an unrecognized software value instead of returning data unchanged", {
  data <- data.frame(
    gazeLeftX = c(1, 2, 3),
    gazeLeftY = c(1, 2, 3),
    validityLeft = c("Valid", "Invalid", "Valid"),
    stringsAsFactors = FALSE
  )

  expect_error(
    removeInvalidGaze(data, "left", "bogus"),
    regexp = "bogus"
  )
})

test_that("removeInvalidGaze creates .valid columns for invalid TobiiPro gaze points, leaving raw columns untouched", {
  data <- data.frame(
    gazeLeftX = c(1, 2, 3, 4),
    gazeLeftY = c(1, 2, 3, 4),
    validityLeft = c("Valid", "Invalid", "Valid", NA),
    stringsAsFactors = FALSE
  )

  result <- expect_error(removeInvalidGaze(data, "left", "TobiiPro"), NA)

  expect_equal(result$gazeLeftX.valid, c(1, NA, 3, 4))
  expect_equal(result$gazeLeftY.valid, c(1, NA, 3, 4))
  # raw columns must be left untouched
  expect_equal(result$gazeLeftX, c(1, 2, 3, 4))
  expect_equal(result$gazeLeftY, c(1, 2, 3, 4))
})

test_that("removeInvalidGaze creates .valid columns for out-of-threshold TobiiStudio gaze points, leaving raw columns untouched", {
  data <- data.frame(
    gazeLeftX = c(1, 2, 3, -9999),
    gazeLeftY = c(1, 2, 3, 4),
    validityLeft = c(0, 3, 1, 0),
    stringsAsFactors = FALSE
  )

  result <- expect_error(removeInvalidGaze(data, "left", "TobiiStudio"), NA)

  expect_equal(result$gazeLeftX.valid, c(1, NA, 3, NA))
  expect_equal(result$gazeLeftY.valid, c(1, NA, 3, 4))
  # raw columns must be left untouched, including the -9999 sentinel value
  expect_equal(result$gazeLeftX, c(1, 2, 3, -9999))
  expect_equal(result$gazeLeftY, c(1, 2, 3, 4))
})
