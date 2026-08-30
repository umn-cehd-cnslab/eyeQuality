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

# ---------------------------------------------------------------------------
# Y-axis reversal regression tests (see plotGazeHeatmap.R and
# analyze_helpers.R for the same fix in the heatmap/Gaze Explorer tab):
# Tobii (and screen-based eye trackers generally) report gaze position with
# (0, 0) at the screen's top-left corner and Y increasing downward, so
# rawGaze_leftY/rawGaze_rightY (raw-data panel) and gaze_Y (the final
# smoothed-gaze panel) each gained a `+ ggplot2::scale_y_reverse()` --
# applied to the Y-position channels only, NOT X, pupil, or Z-distance.
#
# generateEyeTrackingPlots() returns ggpubr::ggarrange() composites, not the
# individual per-channel ggplot objects, so a plain layer_scales()/
# ggplot_build() check on the returned list elements doesn't directly work.
# Two different (version-robust, geometry-based) extraction approaches are
# used below, one per composite, matched to how each one is actually put
# together internally:
#
#  - rawGazePlot (8 channels, ggarrange(common.legend = TRUE, ncol = 2,
#    nrow = 4)): each channel survives as its own already-built
#    ggplotGrob, directly reachable via each GeomCustomAnn layer's
#    `geom_params$grob` on the returned ggarrange object itself (no need to
#    print()/render it first).
#  - gazePlot (2 channels, ggarrange(common.legend = TRUE, ncol = 2,
#    nrow = 1)): under this specific ggpubr/ggplot2 combination, the
#    returned ggarrange object's own $layers only exposes ONE combined
#    grob (missing the second channel) until it's actually rendered -- so
#    it's captured via print() + grid::grid.grab() first, then the two
#    panels are found by full recursive traversal (deduplicated by content,
#    since the same nested gtable is reachable via more than one path in
#    the captured grob tree).
#
# In both cases, the check itself is the same idea as plotGazeHeatmap's own
# regression test: pull each panel's geom_point layer, convert its y
# grob to "native" (post-scale-transform) units, and confirm the sign of its
# correlation with the raw plotted column -- negative for a reversed scale,
# positive for a normal one.

# Recursively finds the first "points"-class grob under `g` (geom_point's
# rendered grob), searching both $children (gTree) and $grobs (gtable).
find_points_grob <- function(g) {
  if (inherits(g, "points")) {
    return(g)
  }
  if (!is.null(g$children)) {
    for (nm in names(g$children)) {
      res <- find_points_grob(g$children[[nm]])
      if (!is.null(res)) {
        return(res)
      }
    }
  }
  if (!is.null(g$grobs)) {
    for (child in g$grobs) {
      res <- find_points_grob(child)
      if (!is.null(res)) {
        return(res)
      }
    }
  }
  NULL
}

# Every "panel"-named cell's points-grob y, converted to native
# (post-scale-transform) units, found anywhere under `top_grob` -- one
# vector per panel encountered, in traversal order (may include duplicates
# when the same nested gtable is reachable via more than one path; callers
# dedupe by content when that matters).
panel_points_native_y <- function(top_grob) {
  out <- list()
  visit <- function(g) {
    if (inherits(g, "gtable")) {
      panel_idx <- which(grepl("^panel", g$layout$name))
      if (length(panel_idx) > 0) {
        ord <- order(g$layout$t[panel_idx], g$layout$l[panel_idx])
        for (idx in panel_idx[ord]) {
          pg <- find_points_grob(g$grobs[[idx]])
          if (!is.null(pg)) {
            out[[length(out) + 1]] <<- grid::convertY(pg$y, "native", valueOnly = TRUE)
          }
        }
      }
    }
    if (!is.null(g$grobs)) {
      for (child in g$grobs) visit(child)
    } else if (!is.null(g$children)) {
      for (nm in names(g$children)) visit(g$children[[nm]])
    }
  }
  visit(top_grob)
  out
}

# Small, varying-value fixture (constant values everywhere give 0 variance,
# making a correlation check meaningless) run through the real eyeQuality()
# pipeline, matching write_plots_fixture()'s TobiiPro-format shape above.
write_varying_plots_fixture <- function(dir, filename = "sub-01_task-test_recording-eyetracking_physio.tsv") {
  n <- 100
  dt_ms <- 17
  ts <- seq(0, by = dt_ms, length.out = n)
  xvals <- seq(400, 1500, length.out = n)
  yvals <- seq(200, 900, length.out = n)

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
    "Gaze point left X" = xvals,
    "Gaze point left Y" = yvals,
    "Gaze point right X" = xvals + 5,
    "Gaze point right Y" = yvals + 5,
    "Eye position left Z (DACSmm)" = seq(590, 610, length.out = n),
    "Eye position right Z (DACSmm)" = seq(590, 610, length.out = n),
    "Pupil diameter left" = seq(3, 4, length.out = n),
    "Pupil diameter right" = seq(3, 4, length.out = n),
    "Validity left" = rep("Valid", n),
    "Validity right" = rep("Valid", n),
    check.names = FALSE
  )

  filepath <- file.path(dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

test_that("generateEyeTrackingPlots() reverses the Y axis for rawGaze_leftY/rawGaze_rightY only, not X/Z/pupil", {
  skip_on_cran()

  dir <- tempfile("plots_yreverse_raw_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_varying_plots_fixture(dir)

  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )
  rawGazePlot <- generateEyeTrackingPlots(result)[[1]]

  content_layers <- Filter(function(L) !is.null(L$geom_params$grob), rawGazePlot$layers)
  # ggarrange() call order: leftX, rightX, leftY, rightY, leftZ, rightZ,
  # leftPupil, rightPupil (matches generateEyeTrackingPlots.R's own
  # ggpubr::ggarrange(rawGaze_leftX, rawGaze_rightX, rawGaze_leftY, ...) call)
  expect_length(content_layers, 8)

  raw_cols <- c(
    "gazeLeftX.valid", "gazeRightX.valid", "gazeLeftY.valid", "gazeRightY.valid",
    "distanceLeftZ.valid", "distanceRightZ.valid", "pupilLeft.valid", "pupilRight.valid"
  )
  expected_sign <- c(1, 1, -1, -1, 1, 1, 1, 1) # only leftY/rightY are reversed

  for (i in seq_along(content_layers)) {
    native_y <- panel_points_native_y(content_layers[[i]]$geom_params$grob)[[1]]
    raw_col <- result[[raw_cols[i]]]
    co <- cor(raw_col, native_y, use = "complete.obs")
    if (expected_sign[i] < 0) {
      expect_lt(co, 0, label = raw_cols[i])
    } else {
      expect_gt(co, 0, label = raw_cols[i])
    }
  }
})

test_that("generateEyeTrackingPlots() reverses the Y axis for gaze_Y only, not gaze_X", {
  skip_on_cran()

  dir <- tempfile("plots_yreverse_gaze_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_varying_plots_fixture(dir)

  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )
  gazePlot <- generateEyeTrackingPlots(result)[[3]]

  grDevices::pdf(NULL)
  print(gazePlot)
  captured <- grid::grid.grab()
  grDevices::dev.off()

  panels_raw <- panel_points_native_y(captured)
  panels <- panels_raw[!duplicated(lapply(panels_raw, function(v) round(v, 6)))]
  # ggarrange() call order: gaze_X, gaze_Y
  expect_length(panels, 2)

  cor_x <- cor(result$gazeX.preprocessed_px, panels[[1]], use = "complete.obs")
  cor_y <- cor(result$gazeY.preprocessed_px, panels[[2]], use = "complete.obs")
  expect_gt(cor_x, 0)
  expect_lt(cor_y, 0)
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
