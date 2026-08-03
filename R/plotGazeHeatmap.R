#' plotGazeHeatmap
#'
#' @param data dataframe
#' @import ggplot2
#' @importFrom rlang .data
#' @return plots
#' @export
#'
plotGazeHeatmap <- function(data) {
  ..density.. <- NULL
  rm(..density..)
  data %>%
    ggplot(aes(x = .data$gazeX.preprocessed_px, y = .data$gazeY.preprocessed_px)) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_x_continuous(limits = c(0, 1920), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1080), expand = c(0, 0)) +
    # scale_fill_viridis() +
    scale_fill_distiller(palette = "Spectral") +
    theme(legend.position = 'none')
}
