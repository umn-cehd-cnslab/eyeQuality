# P2-08: Integration tests. A systematic sweep of full eyeQuality() end-to-end
# runs across every VALID Phase 2 fixture in one place, complementary to the
# single hand-derived value checks already living in
# test-tobiiStudioFixture.R / test-tobiiProFixture.R / test-edgeCaseFixtures.R.
# Where those files check one column value each, this file checks a broader
# slice of the output shape per fixture -- several key output columns via
# explicit expect_equal() (all four fixtures hold gaze constant at one
# off-center pixel location, so their expected values are known exactly) plus
# the full calculateOutputMetrics() summary row via expect_snapshot(), since
# that table (32 metrics x 12 stat columns) is large structured output that
# isn't practical to enumerate by hand. The goal is that Phase 3's adapter
# rewrite (explicitly required to produce zero behavior change) has a single
# place to diff pre- and post-refactor runs against.
#
# Fixtures covered: tobii_studio_sample.tsv, tobii_studio_monocular.tsv,
# tobii_pro_sample.tsv, tobii_pro_monocular.tsv (all from P2-02/P2-03).
#
# Fixtures deliberately NOT covered here:
#   - blink_boundary_sample.csv (P2-04a): this is pre-interpolated
#     pupilLeft.int/pupilRight.int data, the exact shape classifyBlinks()
#     expects as *input* -- it has no raw device columns and cannot be run
#     through eyeQuality() at all, let alone end-to-end.
#   - tobii_studio_out_of_order_timestamps.tsv, tobii_studio_all_na_gaze.tsv,
#     unrecognized_schema.csv (P2-04b/c/e): these are malformed-input edge
#     cases that are *supposed* to make eyeQuality() abort via stop() --
#     already covered by dedicated abort-message assertions in
#     test-edgeCaseFixtures.R. Running them through this file's "does it
#     complete end-to-end" sweep would be a contradiction, not a regression
#     test.
#
# eyeQuality() itself only returns the final preprocessed data.frame (not the
# calculateOutputMetrics() summary row) when saveData = FALSE, and strips the
# intermediate .valid/.es.selection columns that calculateOutputMetrics()
# needs unless includeIntermediates = TRUE. So every run below passes
# includeIntermediates = TRUE and calls calculateOutputMetrics() directly on
# the returned data, exactly as eyeQuality() does internally before it would
# otherwise strip those columns for a saveData = TRUE run.

test_that("eyeQuality end-to-end output shape and metrics match expectations for tobii_studio_sample.tsv", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  # gaze is held constant at one off-center pixel location (1400, 800) for
  # the whole 200-row recording, with constant pupil (3.5) and distance
  # (600mm) -- so every key output column should reduce to a single known
  # value, and any drift here (e.g. a broken column mapping, a mis-scaled
  # visual angle conversion) shows up as an exact-value mismatch.
  expect_equal(nrow(result), 200)
  expect_equal(unique(result$gazeX.preprocessed_px), 1400)
  expect_equal(unique(result$gazeY.preprocessed_px), 800)
  expect_equal(unique(result$distanceZ.preprocessed_mm), 600)
  expect_equal(unique(result$pupil.preprocessed), 3.5)
  # 12.78/7.86 (not 12.77/7.84): calculateVisualAngle() previously used a
  # (displayResolution_px + 1) * 0.5 screen-center pixel that didn't match
  # convertVisualAngToPixels()'s displayResolution_px / 2 center -- see
  # R/calculateVisualAngle.R's "Screen-center pixel convention" section.
  # The fix aligns both on displayResolution_px / 2, the correct center for
  # this package's ADCS-derived pixel coordinates.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.78, tolerance = 1e-6)
  expect_equal(unique(result$gazeY.preprocessed_va), 7.86, tolerance = 1e-6)
  expect_equal(unique(result$blink.classification), 0)
  expect_equal(unique(result$offscreen.classification), "onscreen")
  # velocity is undefined for the very first sample and IVT can't classify a
  # gaze point that has no preceding point to compare against -- rows 1 and
  # 200 are "unclassified", everything strictly between them is "fixation"
  # (gaze never moves, so no saccades).
  expect_equal(
    as.character(result$IVT.classification[c(1, 200)]),
    c("unclassified", "unclassified")
  )
  expect_true(all(result$IVT.classification[2:199] == "fixation"))

  metrics <- calculateOutputMetrics(result)
  expect_snapshot(metrics)
})

test_that("eyeQuality end-to-end output shape and metrics match expectations for tobii_studio_monocular.tsv", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  # right eye is invalid (ValidityRight = 4) for the whole recording, so
  # "Maximize" eye selection falls back to the left eye's constant gazepoint
  # everywhere -- final key columns should be identical to the binocular
  # fixture's, with no NAs surviving to the final output.
  expect_equal(nrow(result), 200)
  expect_equal(unique(result$gazeX.preprocessed_px), 1400)
  expect_equal(unique(result$gazeY.preprocessed_px), 800)
  expect_equal(unique(result$distanceZ.preprocessed_mm), 600)
  expect_equal(unique(result$pupil.preprocessed), 3.5)
  # 12.78/7.86 (not 12.77/7.84): calculateVisualAngle() previously used a
  # (displayResolution_px + 1) * 0.5 screen-center pixel that didn't match
  # convertVisualAngToPixels()'s displayResolution_px / 2 center -- see
  # R/calculateVisualAngle.R's "Screen-center pixel convention" section.
  # The fix aligns both on displayResolution_px / 2, the correct center for
  # this package's ADCS-derived pixel coordinates.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.78, tolerance = 1e-6)
  expect_equal(unique(result$gazeY.preprocessed_va), 7.86, tolerance = 1e-6)
  expect_equal(unique(result$blink.classification), 0)
  expect_equal(unique(result$offscreen.classification), "onscreen")
  expect_equal(
    as.character(result$IVT.classification[c(1, 200)]),
    c("unclassified", "unclassified")
  )
  expect_true(all(result$IVT.classification[2:199] == "fixation"))

  metrics <- calculateOutputMetrics(result)
  # this fixture's metrics table is the one place this sweep distinguishes
  # itself from the binocular fixture -- missing_raw_data_RightEye and
  # eye_select_LeftOnly should both read as "all 200 rows", confirming the
  # monocular fallback path was actually exercised rather than just
  # happening to produce the same final gaze values by coincidence.
  expect_equal(metrics["missing_raw_data_RightEye", "n"], 200)
  expect_equal(metrics["eye_select_LeftOnly", "n"], 200)
  expect_equal(metrics["eye_select_mean", "n"], 0)
  expect_snapshot(metrics)
})

test_that("eyeQuality end-to-end output shape and metrics match expectations for tobii_pro_sample.tsv", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")
  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  expect_equal(nrow(result), 200)
  expect_equal(unique(result$gazeX.preprocessed_px), 1400)
  expect_equal(unique(result$gazeY.preprocessed_px), 800)
  expect_equal(unique(result$distanceZ.preprocessed_mm), 600)
  expect_equal(unique(result$pupil.preprocessed), 3.5)
  # 12.78/7.86 (not 12.77/7.84): calculateVisualAngle() previously used a
  # (displayResolution_px + 1) * 0.5 screen-center pixel that didn't match
  # convertVisualAngToPixels()'s displayResolution_px / 2 center -- see
  # R/calculateVisualAngle.R's "Screen-center pixel convention" section.
  # The fix aligns both on displayResolution_px / 2, the correct center for
  # this package's ADCS-derived pixel coordinates.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.78, tolerance = 1e-6)
  expect_equal(unique(result$gazeY.preprocessed_va), 7.86, tolerance = 1e-6)
  expect_equal(unique(result$blink.classification), 0)
  expect_equal(unique(result$offscreen.classification), "onscreen")
  expect_equal(
    as.character(result$IVT.classification[c(1, 200)]),
    c("unclassified", "unclassified")
  )
  expect_true(all(result$IVT.classification[2:199] == "fixation"))

  metrics <- calculateOutputMetrics(result)
  expect_snapshot(metrics)
})

test_that("eyeQuality end-to-end output shape and metrics match expectations for tobii_pro_monocular.tsv", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  # right eye is invalid (Validity right = "Invalid") for the whole
  # recording -- this exercises the TobiiPro string-based validity branch of
  # removeInvalidGaze.R, as distinct from the monocular Studio fixture's
  # numeric threshold branch, while landing on the same final gaze values.
  expect_equal(nrow(result), 200)
  expect_equal(unique(result$gazeX.preprocessed_px), 1400)
  expect_equal(unique(result$gazeY.preprocessed_px), 800)
  expect_equal(unique(result$distanceZ.preprocessed_mm), 600)
  expect_equal(unique(result$pupil.preprocessed), 3.5)
  # 12.78/7.86 (not 12.77/7.84): calculateVisualAngle() previously used a
  # (displayResolution_px + 1) * 0.5 screen-center pixel that didn't match
  # convertVisualAngToPixels()'s displayResolution_px / 2 center -- see
  # R/calculateVisualAngle.R's "Screen-center pixel convention" section.
  # The fix aligns both on displayResolution_px / 2, the correct center for
  # this package's ADCS-derived pixel coordinates.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.78, tolerance = 1e-6)
  expect_equal(unique(result$gazeY.preprocessed_va), 7.86, tolerance = 1e-6)
  expect_equal(unique(result$blink.classification), 0)
  expect_equal(unique(result$offscreen.classification), "onscreen")
  expect_equal(
    as.character(result$IVT.classification[c(1, 200)]),
    c("unclassified", "unclassified")
  )
  expect_true(all(result$IVT.classification[2:199] == "fixation"))

  metrics <- calculateOutputMetrics(result)
  expect_equal(metrics["missing_raw_data_RightEye", "n"], 200)
  expect_equal(metrics["eye_select_LeftOnly", "n"], 200)
  expect_equal(metrics["eye_select_mean", "n"], 0)
  expect_snapshot(metrics)
})
