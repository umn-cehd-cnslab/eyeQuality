#' plotGazeAndBlinks
#'
#' @param data dataframe
#' @param column name of data column you want plotted as points/lines
#' @param showFixations boolean TRUE/FALSE to indicate if boxes of color to show fixations should be plotted
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @return plots
#' @export
#'
plotGazeAndBlinks <- function(data, column, showFixations = FALSE) {
  ivt_ranges <- data %>% getSequenceGroupEndpoints("IVT.classification")
  if (showFixations == FALSE) {
    ivt_ranges <- ivt_ranges %>% filter(.data$group == "missing")
  }
  blink_ranges <- data %>%
    # getSequenceGroupEndpoints("PupilLeft_blink") %>%
    getSequenceGroupEndpoints("blink.classification") %>%
    filter(.data$group == 1)

  plot <- data %>%
    ggplot2::ggplot() +
    ggplot2::geom_rect(
      # data = data %>% getSequenceGroupEndpoints("IVT.classification"),
      data = ivt_ranges,
      ggplot2::aes(
        xmin = .data$start_recordingTimestamp_ms, xmax = .data$end_recordingTimestamp_ms,
        ymax = Inf,
        ymin = -Inf,
        fill = as.factor(.data$group)
      ),
      alpha = 0.4
    ) +
    ggplot2::geom_rect(
      data = blink_ranges,
      ggplot2::aes(
        xmin = .data$start_recordingTimestamp_ms, xmax = .data$end_recordingTimestamp_ms,
        ymax = Inf,
        ymin = -Inf,
        fill = as.factor(.data$group)
      ),
      alpha = 0.8
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$recordingTimestamp_ms, y = !!rlang::sym(column)), size = 0.5, alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(x = .data$recordingTimestamp_ms, y = !!rlang::sym(column)), alpha = 0.5) +
    ggplot2::scale_fill_manual(
      breaks = c("1", "missing", "unclassified", "fixation", "saccade", ""),
      labels = c("blink", "missing", "unclassified", "fixation", "saccade", ""),
      values = c("goldenrod3", "lightgrey", "gray25", "forestgreen", "blue", "red")
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      # title = "Gaze X (Smoothed)",
      # x = "Recording Timestamp (ms)",
      # y = "x position",
      fill = NULL
    )

  return(plot)
}
