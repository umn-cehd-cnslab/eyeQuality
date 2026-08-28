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
  # 12.78 (not 12.77): calculateVisualAngle() previously used a
  # (displayResolution_px + 1) * 0.5 screen-center pixel that didn't match
  # convertVisualAngToPixels()'s displayResolution_px / 2 center; fixed so
  # both functions use displayResolution_px / 2, the convention confirmed
  # correct for this package's Tobii ADCS-derived pixel coordinates -- see
  # R/calculateVisualAngle.R's "Screen-center pixel convention" section.
  # 12.77 was this fixture's value under the old, half-pixel-biased center.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.78, tolerance = 1e-6)
})

test_that("inst/extdata's vignette copy of the sample fixture stays byte-identical to the test fixture", {
  # P6-02 added inst/extdata/tobii_studio_sample.tsv as a duplicate of this
  # fixture so getting-started.Rmd can reach it via system.file() after
  # install (vignettes can't read tests/testthat/fixtures/). The vignette's
  # prose hardcodes values derived from this file's exact contents (e.g. a
  # constant gazeX.preprocessed_va of 12.77, "200/200 raw samples valid").
  # If the two copies ever desync -- someone edits one fixture to add an
  # edge case without updating the other -- the vignette would silently
  # build against stale data and its narrated numbers would stop matching
  # its own output. Comparing the two copies directly catches that before
  # it ships.
  test_fixture <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  vignette_fixture <- system.file("extdata", "tobii_studio_sample.tsv", package = "eyeQuality")

  skip_if(!nzchar(vignette_fixture), "inst/extdata/tobii_studio_sample.tsv not found via system.file()")

  expect_identical(
    readBin(test_fixture, "raw", n = file.info(test_fixture)$size),
    readBin(vignette_fixture, "raw", n = file.info(vignette_fixture)$size)
  )
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
  # 12.78 (not 12.77): calculateVisualAngle() previously used a
  # (displayResolution_px + 1) * 0.5 screen-center pixel that didn't match
  # convertVisualAngToPixels()'s displayResolution_px / 2 center; fixed so
  # both functions use displayResolution_px / 2, the convention confirmed
  # correct for this package's Tobii ADCS-derived pixel coordinates -- see
  # R/calculateVisualAngle.R's "Screen-center pixel convention" section.
  # 12.77 was this fixture's value under the old, half-pixel-biased center.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.78, tolerance = 1e-6)
})
