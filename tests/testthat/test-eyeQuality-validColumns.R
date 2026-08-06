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

# Regression tests for P3-07: eyeQuality() was rewired to call
# adapter$normalize_validity() (P3-06) instead of two per-eye
# removeInvalidGaze() calls. normalize_validity() additionally computes new
# confidenceLeft/confidenceRight columns (?eyeQuality-schema) that the old
# removeInvalidGaze() path never produced, and removeIntermediateCols()
# selects final output columns by *position* -- so without an explicit strip,
# these new columns would silently leak into eyeQuality()'s default output
# and shift every column after them. These columns get the same
# includeIntermediates-gated treatment as the .valid columns above: stripped
# by default (keeping default output byte-identical to pre-P3-07), retained
# when includeIntermediates = TRUE so users who ask for pipeline-internal
# state can actually see the new confidence values.

test_that("eyeQuality() excludes confidenceLeft/confidenceRight from output with includeIntermediates = FALSE (default)", {
  skip_on_cran()

  dir <- tempfile("p307_confidence_default_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p105_fixture(dir)

  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE
  )

  expect_false("confidenceLeft" %in% colnames(result))
  expect_false("confidenceRight" %in% colnames(result))
})

test_that("eyeQuality() retains confidenceLeft/confidenceRight in output with includeIntermediates = TRUE", {
  skip_on_cran()

  dir <- tempfile("p307_confidence_intermediates_")
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

  expect_true("confidenceLeft" %in% colnames(result))
  expect_true("confidenceRight" %in% colnames(result))
  # fixture marks rows 100-101 "Invalid" on both eyes; confidence should be
  # low (0) there and high (1) everywhere else, per P3-06's Tobii Pro mapping.
  expect_equal(result$confidenceLeft[c(100, 101)], c(0, 0))
  expect_equal(result$confidenceRight[c(100, 101)], c(0, 0))
  expect_true(all(result$confidenceLeft[-c(100, 101)] == 1))
  expect_true(all(result$confidenceRight[-c(100, 101)] == 1))
})

# Regression tests for P3-08: eyeQuality()'s old maxValidityThreshold
# argument (Tobii-Studio-native 0-4 scale, default 2) was replaced with
# validityThreshold on the generic confidence 0-1 scale (1 = highest
# confidence), NULL by default so each adapter falls back to its own
# default_thresholds. When supplied and the detected adapter is TobiiStudio,
# eyeQuality() converts it onto that adapter's native scale via
# nativeThreshold <- (1 - validityThreshold) * 4 before calling
# adapter$normalize_validity(). These tests exercise that conversion
# end-to-end through eyeQuality() rather than just unit-testing the adapter,
# since the conversion itself lives in R/eyeQuality.R, not in the adapter.
#
# write_p105_fixture() above holds validity uniformly "Valid"/0 everywhere,
# which can't distinguish between threshold levels at all (every threshold
# masks the same rows). This fixture instead cycles Tobii Studio's native
# 0-4 validity codes evenly across the recording (40 rows at each code), so
# different validityThreshold values actually mask different numbers of
# points.
write_p308_studio_graded_fixture <- function(dir, filename = "sub-01_task-test_recording-eyetracking_physio.tsv") {
  n <- 200
  dt_ms <- 17 # ~58.8 Hz, arbitrary but realistic, matches write_p105_fixture()
  ts <- seq(0, by = dt_ms, length.out = n)

  # cycles 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, ... -- exactly 40 rows at each code
  # over the 200-row fixture.
  validityCodes <- rep(0:4, length.out = n)

  d <- data.frame(
    StudioVersionRec = rep("3.4.8", n),
    StudioEvent = rep(NA_character_, n),
    StudioEventData = rep(NA_character_, n),
    RecordingDuration = rep(NA_real_, n),
    RecordingResolution = rep("1920 x 1080", n),
    EyeTrackerTimestamp = ts,
    RecordingTimestamp = ts,
    # off-center on purpose, matching write_p105_fixture()'s rationale.
    `GazePointLeftX (ADCSpx)` = rep(1400, n),
    `GazePointLeftY (ADCSpx)` = rep(800, n),
    `GazePointRightX (ADCSpx)` = rep(1400, n),
    `GazePointRightY (ADCSpx)` = rep(800, n),
    `EyePosLeftZ (ADCSmm)` = rep(600, n),
    `EyePosRightZ (ADCSmm)` = rep(600, n),
    PupilLeft = rep(3.5, n),
    PupilRight = rep(3.5, n),
    ValidityLeft = validityCodes,
    ValidityRight = validityCodes,
    check.names = FALSE
  )

  filepath <- file.path(dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

test_that("eyeQuality() validityThreshold = 0.5 (Tobii Studio) produces output identical to the default NULL threshold", {
  skip_on_cran()

  dir <- tempfile("p308_equivalence_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p308_studio_graded_fixture(dir)

  resultDefault <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )
  resultExplicit <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE,
    validityThreshold = 0.5
  )

  # 0.5 confidence converts to native threshold (1 - 0.5) * 4 = 2, exactly
  # Tobii Studio's default_thresholds$validityThreshold (2) and the
  # pre-P3-08 hardcoded maxValidityThreshold default -- so these two calls
  # should produce byte-identical output end to end (masking, confidence,
  # every downstream column), not just an identical count of masked points.
  expect_equal(resultDefault, resultExplicit)
})

test_that("eyeQuality() validityThreshold: a stricter confidence cutoff masks at least as many points as a looser one (Tobii Studio, graded validity)", {
  skip_on_cran()

  dir <- tempfile("p308_monotonic_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p308_studio_graded_fixture(dir)

  resultStrict <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE,
    validityThreshold = 0.9
  )
  resultLoose <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE,
    validityThreshold = 0.1
  )

  maskedStrict <- sum(is.na(resultStrict$gazeLeftX.valid))
  maskedLoose <- sum(is.na(resultLoose$gazeLeftX.valid))

  # validityThreshold = 0.9 (near-highest confidence required) converts to
  # native threshold (1 - 0.9) * 4 = 0.4, masking every validityLeft > 0.4,
  # i.e. codes 1-4 -- 160 of the fixture's 200 rows; only the 40 code-0 rows
  # survive.
  expect_equal(maskedStrict, 160)
  # validityThreshold = 0.1 (looser) converts to native threshold
  # (1 - 0.1) * 4 = 3.6, masking only validityLeft > 3.6, i.e. code 4 alone --
  # 40 of the 200 rows.
  expect_equal(maskedLoose, 40)
  # the general monotonicity property this test protects: reversing the
  # (1 - validityThreshold) * 4 conversion (or breaking the pass-through
  # entirely) would make a stricter confidence cutoff mask fewer or the same
  # number of points as a looser one, rather than strictly more.
  expect_gte(maskedStrict, maskedLoose)
})

test_that("eyeQuality() accepts validityThreshold without erroring for Tobii Pro (documented no-op threshold)", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")

  # Tobii Pro's normalize_validity() ignores `threshold` entirely (binary
  # Valid/Invalid validity has no threshold concept) -- this only confirms
  # passing validityThreshold through eyeQuality() for a TobiiPro-detected
  # file doesn't error, not that it changes masking (it shouldn't and isn't
  # asserted to).
  expect_no_error(
    eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE,
      validityThreshold = 0.5
    )
  )
})
