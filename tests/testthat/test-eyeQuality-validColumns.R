# Regression tests for P1-05: removeInvalidGaze() now emits `.valid`-suffixed
# columns instead of overwriting raw gaze/pupil/distance columns in place.
# These tests exercise the full eyeQuality() pipeline (not just
# removeInvalidGaze() in isolation) to confirm:
#   (a) the includeIntermediates toggle correctly adds/strips the new `.valid`
#       columns from the final output, and
#   (b) the downstream `.preprocessed*` columns still compute sensible values
#       when some points are masked as invalid and then interpolated, i.e.
#       the new column-naming plumbing (removeOffscreenGaze operating on
#       `.valid` columns, interpolateGaze() stripping the trailing `.valid`
#       before appending `.int`, etc.) didn't break the pipeline.

# Same TobiiPro-format fixture shape as write_p1_01_fixture() in
# test-eyeQualityBatch.R, but with two consecutive rows marked "Invalid" for
# both eyes so removeInvalidGaze() has something to mask. Gaze position is
# held constant (off-center, as in the P1-01 fixture) everywhere else, so
# interpolating across the 2-row gap should recover the exact same constant
# value -- letting us hand-verify the final `.preprocessed*` output without
# needing a full byte-for-byte pre-change snapshot.
write_p105_fixture <- function(dir, filename = "sub-01_task-test_recording-eyetracking_physio.tsv") {
  n <- 200
  dt_ms <- 17 # ~58.8 Hz, arbitrary but realistic
  ts <- seq(0, by = dt_ms, length.out = n)

  validityLeft <- rep("Valid", n)
  validityRight <- rep("Valid", n)
  # 2-row gap: within interpolateGaze()'s default maxGapLength_ms = 50ms
  # (floor(50 / 17) = 2 points), so it should get interpolated rather than
  # left NA.
  invalidRows <- c(100, 101)
  validityLeft[invalidRows] <- "Invalid"
  validityRight[invalidRows] <- "Invalid"

  d <- data.frame(
    "Recording software version" = rep("1.90.0", n),
    "Sensor" = rep("Eye Tracker", n),
    "Event" = rep(NA_character_, n),
    "Event value" = rep(NA_character_, n),
    "Recording duration" = rep(NA_real_, n),
    "Recording resolution height" = rep(1080, n),
    "Recording resolution width" = rep(1920, n),
    "Eyetracker timestamp" = ts,
    "Recording timestamp" = ts,
    # off-center on purpose, matching write_p1_01_fixture()'s rationale: a
    # gazepoint at the exact pixel center would produce a 0 VA angle
    # regardless of displayDimension_mm.
    "Gaze point left X" = rep(1400, n),
    "Gaze point left Y" = rep(800, n),
    "Gaze point right X" = rep(1400, n),
    "Gaze point right Y" = rep(800, n),
    "Eye position left Z (DACSmm)" = rep(600, n),
    "Eye position right Z (DACSmm)" = rep(600, n),
    "Pupil diameter left" = rep(3.5, n),
    "Pupil diameter right" = rep(3.5, n),
    "Validity left" = validityLeft,
    "Validity right" = validityRight,
    check.names = FALSE
  )

  filepath <- file.path(dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

test_that("eyeQuality() with includeIntermediates = FALSE (default) drops .valid columns from the final output", {
  skip_on_cran()

  dir <- tempfile("p105_default_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p105_fixture(dir)

  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE
  )

  validCols <- colnames(result)[grepl("\\.valid$", colnames(result), ignore.case = TRUE)]
  expect_length(validCols, 0)
})

test_that("eyeQuality() with includeIntermediates = TRUE retains .valid columns in the final output", {
  skip_on_cran()

  dir <- tempfile("p105_intermediates_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p105_fixture(dir)

  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  validCols <- colnames(result)[grepl("\\.valid$", colnames(result), ignore.case = TRUE)]
  expect_true(length(validCols) > 0)
  expect_true("gazeLeftX.valid" %in% validCols)
  expect_true("gazeRightX.valid" %in% validCols)

  # the two rows marked "Invalid" in the raw fixture should be NA in the
  # .valid column, since removeInvalidGaze() masks them there...
  expect_true(all(is.na(result$gazeLeftX.valid[c(100, 101)])))
  # ...while the raw column stays exactly as supplied (untouched), which is
  # the core P1-05 guarantee this pipeline-level test exists to protect
  # end-to-end, not just at the removeInvalidGaze() unit level.
  expect_equal(result$gazeLeftX[c(100, 101)], c(1400, 1400))
  expect_equal(result$gazeLeftY[c(100, 101)], c(800, 800))
})

test_that("eyeQuality() end-to-end pipeline recovers constant gaze position through masking and interpolation, producing the expected .preprocessed_va value", {
  skip_on_cran()

  dir <- tempfile("p105_pipeline_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p105_fixture(dir)

  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE
  )

  # raw columns are untouched everywhere, including the two rows marked
  # invalid in the fixture -- this is the byte-identical-raw-columns
  # guarantee from P1-05's acceptance criteria, checked here against a full
  # pipeline run rather than just removeInvalidGaze() directly.
  expect_equal(unique(result$gazeLeftX), 1400)
  expect_equal(unique(result$gazeLeftY), 800)
  expect_equal(unique(result$gazeRightX), 1400)
  expect_equal(unique(result$gazeRightY), 800)

  # gaze position is constant (1400, 800) everywhere in the fixture except
  # for the 2-row masked gap, which interpolateGaze() should fill back in to
  # the same constant value (interpolating a flat signal across a 2-point
  # gap yields the same flat value) -- so the final smoothed/eye-selected
  # .preprocessed_px output should show no NAs and no deviation from the
  # constant, at every row including the previously-masked ones.
  expect_false(any(is.na(result$gazeX.preprocessed_px)))
  expect_false(any(is.na(result$gazeY.preprocessed_px)))
  expect_equal(result$gazeX.preprocessed_px[c(100, 101)], c(1400, 1400))
  expect_equal(result$gazeY.preprocessed_px[c(100, 101)], c(800, 800))

  # since gaze position is constant across the whole recording (after
  # masking/interpolation recovers the 2 previously-invalid rows),
  # gazeX.preprocessed_va should be constant too, and match
  # calculateVisualAngle(1400, 600, 1920, 594) rounded to 2 decimal places --
  # the same hand-derived value used by the P1-01 fixture test, giving a
  # concrete pre-change-equivalent value to check against even without an
  # actual pre-change snapshot to diff.
  expect_equal(unique(result$gazeX.preprocessed_va), 12.77, tolerance = 1e-6)
})
