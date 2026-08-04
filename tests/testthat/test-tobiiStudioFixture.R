# P2-02: Tobii Studio fixtures. These are static files at
# tests/testthat/fixtures/tobii_studio_sample.tsv (binocular) and
# tobii_studio_monocular.tsv (right eye ValidityRight = 4 / invalid for the
# whole recording), with column headers matching a real Tobii Studio export
# per R/renameColumns.R's "TobiiStudio" branch (e.g. "StudioVersionRec",
# "GazePointLeftX (ADCSpx)"). They exist so later Phase 2/3 tests have a
# real-schema TobiiStudio input to exercise without hand-building one inline
# each time, and so any future accidental drift in the TobiiStudio column
# mapping (renameColumns.R, detectImportSourceType.R) shows up as a fixture
# test failure here rather than silently downstream.

test_that("detectImportSourceType identifies the Tobii Studio sample fixture as TobiiStudio", {
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  data <- importData(fp)

  expect_equal(detectImportSourceType(data), "TobiiStudio")
})

test_that("detectImportSourceType identifies the Tobii Studio monocular fixture as TobiiStudio", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)

  expect_equal(detectImportSourceType(data), "TobiiStudio")
})

test_that("eyeQuality runs end-to-end on the Tobii Studio sample fixture without error", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")

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
  # typo'd source column name in renameColumns.R's TobiiStudio branch)
  # producing all-NA output that would still pass a bare "doesn't error"
  # check.
  expect_false(any(is.na(result$gazeX.preprocessed_va)))
  expect_equal(unique(result$gazeX.preprocessed_va), 12.77, tolerance = 1e-6)
})

test_that("eyeQuality runs end-to-end on the Tobii Studio monocular fixture without error", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")

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
  # right eye is invalid (ValidityRight = 4) for the whole recording, so
  # eye selection ("Maximize", the default) should fall back to the left
  # eye's constant gazepoint everywhere, with no NAs surviving to the final
  # output.
  expect_false(any(is.na(result$gazeX.preprocessed_va)))
  expect_equal(unique(result$gazeX.preprocessed_va), 12.77, tolerance = 1e-6)
})
