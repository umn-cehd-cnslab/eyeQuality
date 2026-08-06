# P3-10: eyeQuality() gained an opt-in verbose = FALSE argument, forwarded to
# the detected adapter's standardize()/extract_events()/normalize_validity()
# and to classifyBlinks()/eyeSelection()/removeOffscreenGaze()/
# interpolateGaze(). The two acceptance criteria this file exists to protect
# most directly:
#   (a) verbose is purely additive console/log output -- the returned data
#       and every other side effect must be identical regardless of this
#       argument.
#   (b) verbose = TRUE actually surfaces a data-quality-relevant diagnostic a
#       user wouldn't otherwise see, for a fixture with a known anomaly.
#
# Diagnostics are emitted via print() (not message()), so expect_output()/
# capture.output() is the right tool here, not expect_message().
#
# tests/testthat/fixtures/tobii_studio_monocular.tsv is reused as the "known
# anomaly" fixture: ValidityRight = 4 (worst, above the default threshold of
# 2) for the entire 200-row recording -- a textbook case for
# .diagnose_consecutive_runs()'s long-run report. Exact diagnostic text below
# was confirmed by direct execution against this fixture.

test_that("eyeQuality(verbose = FALSE) (default) emits no [verbose]-tagged diagnostic lines, even on a fixture with a known long anomaly", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")

  out <- testthat::capture_output({
    eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE
    )
  })

  expect_false(grepl("\\[verbose\\]", out))
})

test_that("eyeQuality(verbose = TRUE) emits diagnostic lines referencing the known long run of below-threshold right-eye validity", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")

  out <- testthat::capture_output({
    eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE,
      verbose = TRUE
    )
  })

  expect_true(grepl(
    "200 sample\\(s\\) flagged for: right eye validity below threshold \\(2\\)", out
  ))
  expect_true(grepl(
    "rows 1-200: 200 consecutive samples flagged for: right eye validity below threshold \\(2\\)", out
  ))
})

test_that("eyeQuality(verbose = TRUE) returns byte-identical data to eyeQuality(verbose = FALSE)/the default on the same fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")

  result_default <- testthat::capture_output({
    out_default <- eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE,
      includeIntermediates = TRUE
    )
  })
  result_explicit_false <- testthat::capture_output({
    out_explicit_false <- eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE,
      includeIntermediates = TRUE,
      verbose = FALSE
    )
  })
  result_verbose <- testthat::capture_output({
    out_verbose <- eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE,
      includeIntermediates = TRUE,
      verbose = TRUE
    )
  })

  # verbose = FALSE explicitly supplied is indistinguishable from the
  # verbose argument being omitted entirely
  expect_identical(out_default, out_explicit_false)
  # the core P3-10 non-negotiable: verbose is output-only. The returned
  # data.frame must be identical whether or not diagnostics were emitted.
  expect_identical(out_default, out_verbose)
})

test_that("eyeQuality(verbose = TRUE) reports only a total-count diagnostic (no long-run line) for a short below-threshold run", {
  # A small, self-contained TobiiPro-format fixture with exactly 2 consecutive
  # "Invalid" rows -- below .diagnose_consecutive_runs()'s default
  # min_run_length (5), so this should emit the total-count summary line but
  # NOT a "rows X-Y: N consecutive samples" line, confirming the
  # min_run_length threshold is actually respected end-to-end through
  # eyeQuality(), not just at the adapter-unit level.
  n <- 20
  dt_ms <- 17
  ts <- seq(0, by = dt_ms, length.out = n)
  validityLeft <- rep("Valid", n)
  validityRight <- rep("Valid", n)
  validityLeft[c(10, 11)] <- "Invalid"
  validityRight[c(10, 11)] <- "Invalid"

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
    "Validity left" = validityLeft,
    "Validity right" = validityRight,
    check.names = FALSE
  )

  dir <- tempfile("p310_shortrun_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- file.path(dir, "sub-01_task-test_recording-eyetracking_physio.tsv")
  readr::write_tsv(d, fp)

  out <- testthat::capture_output({
    eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = FALSE,
      verbose = TRUE
    )
  })

  # (see test-adapterNormalizeValidity.R's note on why the quoted "Invalid"
  # word itself is intentionally excluded from this pattern -- print()'s own
  # display quoting escapes it with a literal backslash in captured output)
  expect_true(grepl("2 sample\\(s\\) flagged for: left eye validity ==", out))
  expect_false(grepl("consecutive samples flagged for: left eye validity", out))
})
