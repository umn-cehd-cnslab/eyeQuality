# P2-03: Tobii Pro fixtures. These are static files at
# tests/testthat/fixtures/tobii_pro_sample.tsv (binocular) and
# tobii_pro_monocular.tsv (right eye Validity right = "Invalid" for the whole
# recording), with column headers matching a real Tobii Pro export per
# R/renameColumns.R's "TobiiPro" branch (e.g. "Recording software version",
# "Gaze point left X") and R/extractEventRows.R's "Sensor" column check. They
# exist so later Phase 2/3 tests have a real-schema TobiiPro input to
# exercise without hand-building one inline each time, and so any future
# accidental drift in the TobiiPro column mapping (renameColumns.R,
# detectImportSourceType.R, extractEventRows.R) shows up as a fixture test
# failure here rather than silently downstream.

test_that("detectImportSourceType identifies the Tobii Pro sample fixture as TobiiPro", {
  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")
  data <- importData(fp)

  expect_equal(detectImportSourceType(data), "TobiiPro")
})

test_that("detectImportSourceType identifies the Tobii Pro monocular fixture as TobiiPro", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)

  expect_equal(detectImportSourceType(data), "TobiiPro")
})

test_that("eyeQuality runs end-to-end on the Tobii Pro sample fixture without error", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")

  result <- expect_error(
    eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE
    ),
    NA
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  # gaze is held constant at one off-center pixel location for the whole
  # recording, so the final .preprocessed_va column should be a single
  # non-zero, non-NA value -- this catches a broken column mapping (e.g. a
  # typo'd source column name in renameColumns.R's TobiiPro branch, or a
  # "Sensor" filter regression in extractEventRows.R) producing all-NA
  # output that would still pass a bare "doesn't error" check.
  expect_false(any(is.na(result$gazeX.preprocessed_va)))
  # 12.78 (not 12.77): calculateVisualAngle() previously used a
  # (displayResolution_px + 1) * 0.5 screen-center pixel that didn't match
  # convertVisualAngToPixels()'s displayResolution_px / 2 center; fixed so
  # both functions use displayResolution_px / 2, the convention confirmed
  # correct for this package's Tobii ADCS-derived pixel coordinates -- see
  # R/calculateVisualAngle.R's "Screen-center pixel convention" section.
  # 12.77 was this fixture's value under the old, half-pixel-biased center.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.78, tolerance = 1e-6)
})

test_that("eyeQuality runs end-to-end on the Tobii Pro monocular fixture without error", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")

  result <- expect_error(
    eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE
    ),
    NA
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  # right eye is invalid (Validity right = "Invalid") for the whole
  # recording, so eye selection ("Maximize", the default) should fall back
  # to the left eye's constant gazepoint everywhere, with no NAs surviving
  # to the final output. This also exercises the TobiiPro string-based
  # ("Invalid") branch of removeInvalidGaze.R, as distinct from
  # TobiiStudio's numeric threshold branch.
  expect_false(any(is.na(result$gazeX.preprocessed_va)))
  # 12.78 (not 12.77): calculateVisualAngle() previously used a
  # (displayResolution_px + 1) * 0.5 screen-center pixel that didn't match
  # convertVisualAngToPixels()'s displayResolution_px / 2 center; fixed so
  # both functions use displayResolution_px / 2, the convention confirmed
  # correct for this package's Tobii ADCS-derived pixel coordinates -- see
  # R/calculateVisualAngle.R's "Screen-center pixel convention" section.
  # 12.77 was this fixture's value under the old, half-pixel-biased center.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.78, tolerance = 1e-6)
})
