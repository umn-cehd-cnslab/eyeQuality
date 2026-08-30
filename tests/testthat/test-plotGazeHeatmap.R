# plotGazeHeatmap() draws gaze density using gazeX.preprocessed_px/
# gazeY.preprocessed_px. Tobii (and screen-based eye trackers generally)
# report gaze position with (0, 0) at the screen's TOP-LEFT corner and Y
# increasing DOWNWARD -- the opposite of ggplot2's default bottom-left-origin
# axis convention. This regression test protects the fix that changed
# scale_y_continuous() to scale_y_reverse() so the heatmap draws right-side
# up relative to the actual screen, rather than vertically flipped.
#
# Verification approach: build the plot with ggplot2::ggplot_build() and
# extract the point-layer's post-scale-transform y values, then confirm
# they're negatively correlated with the raw input gazeY.preprocessed_px
# values -- ggplot2 4.x's scale-introspection API (e.g. trans$name-style
# access) differs from older versions, so this built-layer-data approach is
# more version-robust than introspecting the scale object directly.

test_that("plotGazeHeatmap does not error and returns a ggplot object", {
  data <- data.frame(
    gazeX.preprocessed_px = c(100, 200, 300, 400, 500),
    gazeY.preprocessed_px = c(100, 200, 300, 400, 500)
  )
  plot <- NULL
  expect_error(plot <- plotGazeHeatmap(data), NA)
  expect_s3_class(plot, "ggplot")
})

test_that("plotGazeHeatmap reverses the Y axis (built layer y is negatively correlated with raw gazeY.preprocessed_px)", {
  raw_y <- c(50, 200, 400, 600, 900, 150, 700, 300, 1000, 20)
  data <- data.frame(
    gazeX.preprocessed_px = seq(100, 1000, length.out = length(raw_y)),
    gazeY.preprocessed_px = raw_y
  )
  plot <- plotGazeHeatmap(data)
  built <- ggplot2::ggplot_build(plot)

  # stat_density_2d's built layer carries its own y grid, not the raw
  # per-point values -- so instead we build the SAME plot's y scale directly
  # and confirm it maps raw values in reverse (larger raw y -> smaller
  # transformed/panel-position y), which is exactly what scale_y_reverse()
  # (vs. the old scale_y_continuous()) changes.
  y_scale <- built$plot$scales$get_scales("y")
  expect_false(is.null(y_scale))

  transformed <- y_scale$transform(raw_y)
  expect_lt(cor(raw_y, transformed), 0)
})

test_that("plotGazeHeatmap keeps the X axis in its default (non-reversed) orientation", {
  raw_x <- c(50, 200, 400, 600, 900, 150, 700, 300, 1000, 20)
  data <- data.frame(
    gazeX.preprocessed_px = raw_x,
    gazeY.preprocessed_px = seq(100, 1000, length.out = length(raw_x))
  )
  plot <- plotGazeHeatmap(data)
  built <- ggplot2::ggplot_build(plot)

  x_scale <- built$plot$scales$get_scales("x")
  expect_false(is.null(x_scale))
  transformed <- x_scale$transform(raw_x)
  expect_gt(cor(raw_x, transformed), 0)
})
