# assignFinalColumnNames() copies the smoothed (smoothGaze_boolean = TRUE) or
# eye-selected (smoothGaze_boolean = FALSE) working columns onto the final
# ".preprocessed" column names. Regression test for a real bug: the
# smoothGaze_boolean = FALSE branch referenced an undefined variable
# (`noise_reduction`, a stale leftover from before that parameter was renamed
# to `smoothGaze_boolean`), which meant eyeQuality(smoothGaze_boolean = FALSE)
# crashed with "object 'noise_reduction' not found" -- a path no existing
# test ever exercised, discovered incidentally via an R CMD check WARNING for
# a stale roxygen @param tag pointing at the same rename.

make_afcn_fixture <- function() {
  data.frame(
    gazeX.smooth = c(1, 2), gazeY.smooth = c(3, 4),
    distanceZ.smooth = c(5, 6), pupil.smooth = c(7, 8),
    gazeX.eyeSelect = c(11, 12), gazeY.eyeSelect = c(13, 14),
    distanceZ.eyeSelect = c(15, 16), pupil.eyeSelect = c(17, 18),
    gazeX_va = c(21, 22), gazeY_va = c(23, 24),
    velocityX.smooth_va_ms = c(31, 32), velocityY.smooth_va_ms = c(33, 34),
    velocityEuclidean.smooth_va_ms = c(35, 36)
  )
}

test_that("assignFinalColumnNames(smoothGaze_boolean = TRUE) uses the smoothed columns", {
  result <- assignFinalColumnNames(make_afcn_fixture(), smoothGaze_boolean = TRUE)

  expect_equal(result$gazeX.preprocessed_px, c(1, 2))
  expect_equal(result$gazeY.preprocessed_px, c(3, 4))
  expect_equal(result$distanceZ.preprocessed_mm, c(5, 6))
  expect_equal(result$pupil.preprocessed, c(7, 8))
})

test_that("assignFinalColumnNames(smoothGaze_boolean = FALSE) uses the eye-selected columns without erroring", {
  result <- assignFinalColumnNames(make_afcn_fixture(), smoothGaze_boolean = FALSE)

  expect_equal(result$gazeX.preprocessed_px, c(11, 12))
  expect_equal(result$gazeY.preprocessed_px, c(13, 14))
  expect_equal(result$distanceZ.preprocessed_mm, c(15, 16))
  expect_equal(result$pupil.preprocessed, c(17, 18))
})

test_that("assignFinalColumnNames() always assigns the visual-angle and velocity columns regardless of smoothGaze_boolean", {
  fixture <- make_afcn_fixture()

  result_smooth <- assignFinalColumnNames(fixture, smoothGaze_boolean = TRUE)
  result_eyeselect <- assignFinalColumnNames(fixture, smoothGaze_boolean = FALSE)

  for (result in list(result_smooth, result_eyeselect)) {
    expect_equal(result$gazeX.preprocessed_va, c(21, 22))
    expect_equal(result$gazeY.preprocessed_va, c(23, 24))
    expect_equal(result$velocityX.preprocessed_va_ms, c(31, 32))
    expect_equal(result$velocityY.preprocessed_va_ms, c(33, 34))
    expect_equal(result$velocityEuclidean.preprocessed_va_ms, c(35, 36))
  }
})
