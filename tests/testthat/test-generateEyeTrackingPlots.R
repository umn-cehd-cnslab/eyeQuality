# Regression tests for P1-05's generateEyeTrackingPlots() fix: the function
# used to hardcode references to `.valid`-suffixed columns (e.g.
# "gazeLeftX.valid"), which only exist in eyeQuality() output when
# includeIntermediates = TRUE. Under the default includeIntermediates = FALSE
# (P1-05's new default behavior), those columns are stripped before
# eyeQuality() returns, so generateEyeTrackingPlots() errored on ordinary
# default-mode output. bug-fixer's resolveValidCol() helper falls back to the
# raw column name when the `.valid` column isn't present. These tests exercise
# both modes end-to-end against a real eyeQuality() pipeline run rather than
# hand-built column names, so they'd fail again if resolveValidCol() were
# removed or the hardcoded ".valid" references crept back in.

# Same minimal TobiiPro-format fixture pattern as write_p1_01_fixture() in
# test-eyeQualityBatch.R: a single simulated participant fixating one
# off-center screen location for the whole recording, so every pipeline stage
# runs cleanly without hitting missing-data edge cases -- all this fixture
# needs to do is produce a valid eyeQuality() output data frame to hand to
# generateEyeTrackingPlots().
write_plots_fixture <- function(dir, filename = "sub-01_task-test_recording-eyetracking_physio.tsv") {
  n <- 200
  dt_ms <- 17 # ~58.8 Hz, arbitrary but realistic
  ts <- seq(0, by = dt_ms, length.out = n)

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
    "Gaze point left X" = rep(1400, n),
    "Gaze point left Y" = rep(800, n),
    "Gaze point right X" = rep(1400, n),
    "Gaze point right Y" = rep(800, n),
    "Eye position left Z (DACSmm)" = rep(600, n),
    "Eye position right Z (DACSmm)" = rep(600, n),
    "Pupil diameter left" = rep(3.5, n),
    "Pupil diameter right" = rep(3.5, n),
    "Validity left" = rep("Valid", n),
    "Validity right" = rep("Valid", n),
    check.names = FALSE
  )

  filepath <- file.path(dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

test_that("generateEyeTrackingPlots() does not error on default eyeQuality() output (includeIntermediates = FALSE, no .valid columns)", {
  skip_on_cran()

  dir <- tempfile("plots_default_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_plots_fixture(dir)

  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE
  )

  # confirm the fixture actually exercises the fallback path this test is
  # meant to protect: no .valid columns present in the input to
  # generateEyeTrackingPlots()
  expect_length(colnames(result)[grepl("\\.valid$", colnames(result))], 0)

  plots <- NULL
  expect_error(
    plots <- generateEyeTrackingPlots(result),
    NA
  )

  expect_type(plots, "list")
  expect_length(plots, 3)
})

test_that("generateEyeTrackingPlots() does not error on eyeQuality() output with .valid columns present (includeIntermediates = TRUE)", {
  skip_on_cran()

  dir <- tempfile("plots_intermediates_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_plots_fixture(dir)

  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  # confirm the fixture actually exercises the .valid-column path this test
  # is meant to protect
  validCols <- colnames(result)[grepl("\\.valid$", colnames(result))]
  expect_true("gazeLeftX.valid" %in% validCols)

  plots <- NULL
  expect_error(
    plots <- generateEyeTrackingPlots(result),
    NA
  )

  expect_type(plots, "list")
  expect_length(plots, 3)
})
