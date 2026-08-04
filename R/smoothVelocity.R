#' Smooth Velocity
#'
#' @param data dataframe
#' @param recordingFrequency_hz recording Hz
#' @param velocity list of column names which contain velocity data to be smoothed. These columns should already be converted from pixels to VA
#' @param smoothVelocityWindow_ms An integer. Velocity smoothing window size in milliseconds. Default: 10ms (pg 121 Liz's dissertation)
#' @param ... additional passed parameters from parent function
#'
#' @importFrom zoo rollapply
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#'
#' @return data with smoothed velocity column
#' @export
#'
smoothVelocity <-
  function(data, recordingFrequency_hz, velocity, smoothVelocityWindow_ms = 10, ...) {
    sampling.dur <- 1000 / recordingFrequency_hz
    vel_window <- floor(smoothVelocityWindow_ms / sampling.dur)
    print(
      paste(
        "denoising using rolling average window of ",
        smoothVelocityWindow_ms,
        "ms. For your sampling rate, this is equivalent to ",
        vel_window,
        " gaze points",
        sep = ""
      )
    )

    if (as.integer(vel_window) %% 2 == 0) {
      vel_window <- vel_window + 1
    }

    # define new smoothed velocity columns
    # create list for smooth column names
    smoothCols <- velocity
    # add ".smooth" to column names, but insert before "_va_ms" if present
    for (sc in 1:length(smoothCols)) {
      i_col <- smoothCols[sc]
      if (stringr::str_detect(i_col, "_va_ms")) {
        smoothCols[sc] <- stringr::str_replace(i_col, "_va_ms", ".smooth_va_ms")
      } else {
        smoothCols[sc] <- paste0(i_col, ".smooth")
      }
    }

    # initiate smoothing for each input column
    for (v in 1:length(velocity)) {
      vCol <- velocity[v] # set interpolated column name
      sCol <- smoothCols[v] # set smoothed column name
      data[sCol] <-
        append(append(rep(NA, trunc(vel_window / 2)), round(
          zoo::rollapply(
            data[, vCol],
            width = vel_window,
            na.rm = TRUE,
            FUN = mean,
            align = "center"
          ),
          2
        )), rep(NA, trunc(vel_window / 2)))
      data[[sCol]][is.nan(data[[sCol]])] <- NA # set NaN to NA

      # from Liz's script - keeps from smoothing over missing data points?
      data[[sCol]][!is.na(data[[sCol]]) & is.na(data[[vCol]])] <- NA
    }

    return(data)
  }
