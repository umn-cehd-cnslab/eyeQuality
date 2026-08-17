#' Get Sequence Group Endpoints
#'
#' @param data dataframe
#' @param group_column name of column to group data over the event
#'
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' @importFrom tibble tibble
#'
#' @return group_endpoints
#' @export
#'
getSequenceGroupEndpoints <- function(data, group_column) {
  group_endpoints <- data %>%
    dplyr::select("recordingTimestamp_ms", !!rlang::sym(group_column)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "start_flag" = !!rlang::sym(group_column) != dplyr::lag(!!rlang::sym(group_column), n = 1),
      "end_flag" = !!rlang::sym(group_column) != dplyr::lead(!!rlang::sym(group_column), n = 1),
    ) %>%
    dplyr::filter(.data$start_flag == TRUE |
      .data$end_flag == TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "group" = !!rlang::sym(group_column),
      "start_recordingTimestamp_ms" = ifelse(
        .data$start_flag,
        .data$recordingTimestamp_ms,
        dplyr::lag(.data$recordingTimestamp_ms)
      ),
      "end_recordingTimestamp_ms" = ifelse(
        .data$end_flag,
        .data$recordingTimestamp_ms,
        dplyr::lead(.data$recordingTimestamp_ms)
      )
    ) %>%
    dplyr::mutate("duration" = .data$end_recordingTimestamp_ms - .data$start_recordingTimestamp_ms) %>%
    dplyr::select(
      "group",
      "start_recordingTimestamp_ms",
      "end_recordingTimestamp_ms",
      "duration"
    ) %>%
    unique()

  rownames(group_endpoints) <- NULL

  group_endpoints <- group_endpoints %>%
    dplyr::group_by(.data$group) %>%
    dplyr::mutate(group_index = dplyr::row_number()) %>%
    dplyr::ungroup()

  return(group_endpoints)
}
