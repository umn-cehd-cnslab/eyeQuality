# calculateVelocity_va_ms() computes per-axis velocity (visual angle per
# second) as a 2-sample central difference:
#   velocityX[i] = (x[i] - x[i-2]) / (t[i] - t[i-2]) * 1000
# with the first two and last samples left NA (no 2-sample-back neighbor, and
# the trailing value is dropped/re-padded as NA by construction), and
# Euclidean velocity = sqrt(velocityX^2 + velocityY^2).

test_that("calculateVelocity_va_ms matches a hand-computed 2-sample central difference", {
  data <- data.frame(
    gazeX_va = c(0, 1, 3, 6, 10),
    gazeY_va = c(0, 0, 0, 0, 0),
    recordingTimestamp_ms = c(0, 10, 20, 30, 40)
  )
  # velocityX[3] = (3 - 0) / (20 - 0) * 1000 = 150
  # velocityX[4] = (6 - 1) / (30 - 10) * 1000 = 250
  # velocityX[5] = (10 - 3) / (40 - 20) * 1000 = 350
  # (row 5's raw computed value is dropped by the trailing shift/NA-pad, so
  # both row 1, row 2, and row 5 end up NA)

  result <- calculateVelocity_va_ms(
    data,
    gazeX_va = "gazeX_va",
    gazeY_va = "gazeY_va",
    timestamp = "recordingTimestamp_ms"
  )

  expect_equal(result$velocityX_va_ms, c(NA, 150, 250, 350, NA))
  expect_equal(result$velocityY_va_ms, c(NA, 0, 0, 0, NA))
  expect_equal(result$velocityEuclidean_va_ms, c(NA, 150, 250, 350, NA))
})
