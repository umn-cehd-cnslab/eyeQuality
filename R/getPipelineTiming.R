#' getPipelineTiming - functions to calculation processing time for pipeline
#'
#' @param start timestamp for start time
#' @param stop timestamp for stop time
#'
#' @return time_diff a string stating the human readable elapsed time
#' @export
#'

getPipelineTiming <- function(start, stop) {
  diff <- as.numeric(stop - start, units = "secs")
  time_diff <- dplyr::case_when(
    diff < 0.05 ~ sprintf("%0.5f ms", diff * 1000),
    diff < 60 ~ sprintf("%0.5f sec", diff),
    diff > 60 ~ sprintf(
      "%d min %0.3f sec",
      floor(as.numeric(stop - start, units = "mins")),
      diff - 60 * floor(as.numeric(stop - start, units = "mins"))
    ),
    diff > 3600 ~ sprintf(
      "%d hr %d min %0.3f sec",
      floor(as.numeric(stop - start, units = "hours")),
      floor(as.numeric(stop - start, units = "mins")) - 60 * floor(as.numeric(stop - start,
        units =
          "hours"
      )),
      diff - 60 * floor(as.numeric(stop - start, units = "mins"))
    ),
    TRUE ~ sprintf("%0.5f sec", diff)
  )
  return(time_diff)
}
