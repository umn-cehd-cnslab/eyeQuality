#' De-noise and smooth rolling averages
#'
#'
#' @param data dataframe
#' @param recordingFrequency_hz recording Hz
#' @param columnsToSmooth list of columns to smooth. Recommended to smooth gaze, pupil, and Z Dist columns
#' @param smoothingWindow_ms An integer, length of smoothing window in ms. Default 20 ms (Liz's dissertation pg 121)
#' @param smoothGaze_boolean Boolean
#' @param ... additional passed parameters from parent function
#'
#' @importFrom zoo rollapply
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#'
#' @return data with smoothed gazepoint, distance, and pupillometry
#' @export
#'
smoothGaze <-
  function(data,
           recordingFrequency_hz,
           columnsToSmooth,
           smoothingWindow_ms = 20,
           smoothGaze_boolean = TRUE,
           ...) {
    # update default smoothing window- not sure on value
    if (smoothGaze_boolean) {
      sampling_dur <- 1000 / recordingFrequency_hz
      smooth_window <- floor(smoothingWindow_ms / sampling_dur)
      print(
        paste(
          "denoising using rolling average window of ",
          smoothingWindow_ms,
          "ms. For your sampling rate, this is equivalent to ",
          smooth_window,
          " gaze points",
          sep = ""
        )
      )

      if (as.integer(smooth_window) %% 2 == 0) {
        smooth_window <- smooth_window + 1
      }

      # create list for smooth column names
      smooth_cols <- columnsToSmooth
      # add ".smooth" to column names, but remove ".eyeSelect" if present in column names
      for (sc in 1:length(smooth_cols)) {
        i_col <- smooth_cols[sc]
        if (stringr::str_detect(i_col, ".eyeSelect")) {
          smooth_cols[sc] <- stringr::str_replace(i_col, ".eyeSelect", ".smooth")
        } else {
          smooth_cols[sc] <- paste0(i_col, ".smooth")
        }
      }

      # run smoothing on window for gaze, va, dist, pupils
      for (sm in 1:length(columnsToSmooth)) {
        eCol <- columnsToSmooth[sm] # set interpolated column name
        sCol <- smooth_cols[sm] # set smoothed column name
        data[sCol] <-
          append(append(rep(NA, trunc(
            smooth_window / 2
          )), round(
            rollapply(
              data[, eCol],
              width = smooth_window,
              na.rm = TRUE,
              FUN = mean,
              align = "center"
            ),
            2
          )), rep(NA, trunc(smooth_window / 2)))
        data[[sCol]][is.nan(data[[sCol]])] <- NA # set NaN to NA
        data[[sCol]][!is.na(data[[sCol]]) & is.na(data[[eCol]])] <- NA
      }
      return(data)
    }
  }
