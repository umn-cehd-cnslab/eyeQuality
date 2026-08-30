# Regression tests for P1-09: eyeQuality()'s "command run: ..." arg-logging
# line used to build its message by gluing raw argument values directly
# (`stringr::str_glue("{names(args)} = {args}")`), which is fine for atomic
# scalars but produces a single giant garbled line (every row of every
# gazepoint column pasted inline as `c(1400, 1400, 1400, ...)`) when an
# argument is a data.frame or list -- exactly what happens when a caller
# passes `data = someDataFrame` directly instead of `filepath = `, which is
# the package's own documented alternate calling convention (see
# `?eyeQuality`'s `data` parameter). The fix pre-summarizes non-atomic args
# (data.frame -> "<data.frame: NxM>", list -> "<list: length N>") before
# gluing. These tests exercise the real `eyeQuality()` call path (not just
# the summarization logic in isolation) to confirm the fix actually reaches
# the print line without erroring and without dumping raw values.

# Minimal synthetic TobiiPro-format data.frame, matching the raw column shape
# `write_p1_01_fixture()` (test-eyeQualityBatch.R) writes to a .tsv file --
# but built directly as a data.frame and passed via `data = ` rather than
# written to disk, since exercising that exact `data = ` calling convention
# (which skips `importData()` entirely) is the whole point of this bug: a
# small, single, constant, off-center fixation with every eye always valid,
# so the full pipeline completes without hitting any interpolation/blink/
# smoothing edge cases we don't care about here.
build_arglogging_fixture_df <- function(n = 30) {
  dt_ms <- 17 # ~58.8 Hz, arbitrary but realistic
  ts <- seq(0, by = dt_ms, length.out = n)

  data.frame(
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
}

test_that("eyeQuality(data = someDataFrame, ...) logs a summarized <data.frame: NxM> line instead of erroring or gluing raw row values", {
  skip_on_cran()

  fixtureDf <- build_arglogging_fixture_df(n = 30)
  expectedDims <- paste0(nrow(fixtureDf), "x", ncol(fixtureDf))

  result <- NULL
  out <- testthat::capture_output({
    result <- eyeQuality(
      filepath = "unused-because-data-is-supplied.tsv",
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      data = fixtureDf,
      saveData = FALSE
    )
  })

  # ran to completion (i.e. reached the argList construction/print line
  # without erroring, and continued on through the rest of the pipeline)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)

  # the "command run: ..." block contains a summarized entry for the `data`
  # argument, in the "<data.frame: NxM>" form documented by P1-09's target
  # state, with dimensions matching the actual data.frame supplied
  expect_true(grepl(
    paste0("data = <data.frame: ", expectedDims, ">"),
    out,
    fixed = TRUE
  ))

  # the pre-fix bug: gluing `args` (which included the raw ~30-row
  # data.frame) directly produced a single line dumping every gazepoint
  # value inline, e.g. "data = list(... c(1400, 1400, 1400, ...) ...)".
  # None of that raw per-row content should appear anywhere in the printed
  # "command run" output.
  expect_false(grepl("c(1400, 1400", out, fixed = TRUE))
})

test_that("eyeQuality() logs a summarized <list: length N> line for a list-valued argument instead of gluing raw list contents", {
  skip_on_cran()

  fixtureDf <- build_arglogging_fixture_df(n = 30)

  # studioEvents is documented as accepting a length-2 sequence of event
  # labels; passing an actual list() (rather than c()) exercises the
  # is.list() branch of the summarization fix. proEvents is left at its
  # NULL default, so `!isempty(studioEvents) && !isempty(proEvents)` is
  # FALSE and the event-based time-range branch is never entered -- this
  # argument is only present to be logged, not acted on.
  out <- testthat::capture_output({
    eyeQuality(
      filepath = "unused-because-data-is-supplied.tsv",
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      data = fixtureDf,
      studioEvents = list("TaskStart", "TaskEnd"),
      saveData = FALSE
    )
  })

  expect_true(grepl("studioEvents = <list: length 2>", out, fixed = TRUE))

  # the raw list contents ("TaskStart"/"TaskEnd") should not appear glued
  # directly into the command-run line
  expect_false(grepl("studioEvents = list(", out, fixed = TRUE))
  expect_false(grepl("TaskStart", out, fixed = TRUE))
})
