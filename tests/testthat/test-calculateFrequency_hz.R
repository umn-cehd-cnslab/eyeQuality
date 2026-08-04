# calculateFrequency_hz() derives the recording frequency (Hz) from the mean
# spacing between consecutive recordingTimestamp_ms values. These cases are
# the function's own roxygen @examples, converted into formal assertions
# (P2-05), plus one uneven-spacing case to exercise the averaging behavior.

test_that("calculateFrequency_hz computes 500 Hz for a constant 2ms sampling interval", {
  # roxygen @examples case 1
  exampledata <- data.frame("recordingTimestamp_ms" = seq(1, 1000, 2))

  expect_equal(calculateFrequency_hz(exampledata), 500)
})

test_that("calculateFrequency_hz gives the same result when excluding the first timepoint", {
  # roxygen @examples case 2 -- spacing is still a constant 2ms, so excluding
  # the first timepoint should not change the computed frequency
  exampledata <- data.frame("recordingTimestamp_ms" = seq(1, 1000, 2))

  expect_equal(
    calculateFrequency_hz(exampledata, 2:nrow(exampledata)),
    500
  )
})

test_that("calculateFrequency_hz averages uneven sample spacing before converting to Hz", {
  # diffs: 8, 8, 9, 8 -> mean = 8.25 -> rounded to 1 decimal = 8.2
  # (R's round-half-to-even and floating point representation of 8.25 yield
  # exactly 8.2 here, confirmed by direct execution)
  data <- data.frame(recordingTimestamp_ms = c(0, 8, 16, 25, 33))

  expect_equal(round(calculateFrequency_hz(data), 4), round(1000 / 8.2, 4))
})
