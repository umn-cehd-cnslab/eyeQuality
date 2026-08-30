# Guard against base R's min()/max() returning +/-Inf with a warning when
# every value in the group is NA (e.g. a recording with zero fixations,
# saccades, or blinks leaves the target column entirely NA).
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

# magrittr's `.` pronoun (used below in replace(is.na(.), 0) inside %>%
# chains) isn't a real exportable symbol the way rlang's `.data` is, so
# @importFrom can't resolve it -- R CMD check's static analysis flags it as
# an undefined global variable unless explicitly whitelisted this way, the
# standard idiom for this exact false positive.
utils::globalVariables(".")

# Precision metrics (SD + RMS sample-to-sample) -----------------------------
#
# Ports an existing lab script (previously run ad hoc, outside this
# package, against IBIS study data) into the pipeline proper, generalized
# to run for three fixation-selection variants -- "whole file" (no
# filtering), "longest fixation", and "median fixation" -- instead of the
# original's two, and to derive its rolling-window size from the
# recording's actual sampling rate (calculateFrequency_hz()) instead of a
# hardcoded ~300Hz assumption.

# Returns the rows of `data` belonging to the fixation(s) tied for the
# longest (or median) IVT.fixationDuration_ms. Mirrors the reference
# script's tie-handling: more than one IVT.fixationIndex can be tied for
# the target duration, and all of their rows are included together. Returns
# a zero-row data frame (not an error) when there are no classified
# fixations to select from.
.select_fixation_rows <- function(data, statistic = c("longest", "median")) {
  statistic <- match.arg(statistic)

  if (!all(c("IVT.fixationIndex", "IVT.fixationDuration_ms") %in% colnames(data)) ||
    all(is.na(data$IVT.fixationIndex))) {
    return(data[0, , drop = FALSE])
  }

  target_duration <- if (statistic == "longest") {
    safe_max(data$IVT.fixationDuration_ms)
  } else {
    stats::median(data$IVT.fixationDuration_ms, na.rm = TRUE)
  }

  if (is.na(target_duration)) {
    return(data[0, , drop = FALSE])
  }

  target_fix_ind <- unique(stats::na.omit(
    data$IVT.fixationIndex[data$IVT.fixationDuration_ms == target_duration]
  ))

  data[!is.na(data$IVT.fixationIndex) & data$IVT.fixationIndex %in% target_fix_ind, , drop = FALSE]
}

# Root-mean-square sample-to-sample deviation of one gaze stream, matching
# the reference script's rms.s2sdev(): the RMS of consecutive-sample
# differences, ignoring NA pairs.
.rms_s2s_deviation <- function(data_stream) {
  sqrt(sum((data_stream - dplyr::lag(data_stream))^2, na.rm = TRUE) / sum(!is.na(data_stream)))
}

# Computes the RMS-S2S rolling-window size, in samples, for a window of
# `window_ms` milliseconds, derived from `data`'s ACTUAL sampling rate
# (calculateFrequency_hz()) rather than a hardcoded frequency assumption.
# Returns NA if the sampling rate can't be determined (e.g. no
# recordingTimestamp_ms column, or a degenerate/non-finite frequency).
.precision_window_size_samples <- function(data, window_ms = 100) {
  if (!"recordingTimestamp_ms" %in% colnames(data)) {
    return(NA_real_)
  }
  recording_frequency_hz <- calculateFrequency_hz(data)
  if (is.na(recording_frequency_hz) || !is.finite(recording_frequency_hz) || recording_frequency_hz <= 0) {
    return(NA_real_)
  }
  floor(window_ms / (1000 / recording_frequency_hz)) + 1
}

# NaN can come out of mean()/median() (e.g. an all-NA rolling-window result)
# where NA is the value this file's other NA-safe helpers (and downstream
# consumers of qcsummary.tsv) expect -- collapse it the same way.
.nan_to_na <- function(x) if (is.nan(x)) NA_real_ else x

# Computes the 8 SD/RMS-S2S precision values for one fixation-selection
# variant ("all" = whole file, "longest", "median") of `data`.
# `window_size_samples` must be precomputed once, from the FULL,
# unsubsetted file's actual sampling rate (see calculateOutputMetrics()
# below) -- sampling rate is a property of the recording/device, not of an
# arbitrary row subset, so it should not be re-derived per fixation.
# Returns all-NA (not an error) if the visual-angle gaze columns are
# missing entirely, or if the requested fixation subset is empty.
.calculate_precision_metrics <- function(data,
                                          fixation_selection = c("all", "longest", "median"),
                                          window_size_samples) {
  fixation_selection <- match.arg(fixation_selection)

  na_result <- list(
    sdX = NA_real_,
    sdY = NA_real_,
    rmsX_mean = NA_real_,
    rmsX_median = NA_real_,
    rmsY_mean = NA_real_,
    rmsY_median = NA_real_,
    rmsEuc_mean = NA_real_,
    rmsEuc_median = NA_real_
  )

  if (!all(c("gazeX.preprocessed_va", "gazeY.preprocessed_va") %in% colnames(data))) {
    return(na_result)
  }

  if (length(window_size_samples) != 1 || is.na(window_size_samples) ||
    !is.finite(window_size_samples) || window_size_samples < 1) {
    return(na_result)
  }

  subset_data <- switch(fixation_selection,
    all = data,
    longest = .select_fixation_rows(data, "longest"),
    median = .select_fixation_rows(data, "median")
  )

  if (nrow(subset_data) == 0) {
    return(na_result)
  }

  gaze_x <- subset_data$gazeX.preprocessed_va
  gaze_y <- subset_data$gazeY.preprocessed_va

  rms_x <- runner::runner(gaze_x, lag = 1, k = window_size_samples, na_pad = TRUE, f = .rms_s2s_deviation)
  rms_y <- runner::runner(gaze_y, lag = 1, k = window_size_samples, na_pad = TRUE, f = .rms_s2s_deviation)
  rms_euc <- sqrt(rms_x^2 + rms_y^2)

  list(
    sdX = .nan_to_na(sd(gaze_x, na.rm = TRUE)),
    sdY = .nan_to_na(sd(gaze_y, na.rm = TRUE)),
    rmsX_mean = .nan_to_na(mean(rms_x, na.rm = TRUE)),
    rmsX_median = .nan_to_na(stats::median(rms_x, na.rm = TRUE)),
    rmsY_mean = .nan_to_na(mean(rms_y, na.rm = TRUE)),
    rmsY_median = .nan_to_na(stats::median(rms_y, na.rm = TRUE)),
    rmsEuc_mean = .nan_to_na(mean(rms_euc, na.rm = TRUE)),
    rmsEuc_median = .nan_to_na(stats::median(rms_euc, na.rm = TRUE))
  )
}

#' Calculate Output Metrics
#'
#' Also computes SD and RMS sample-to-sample (RMS-S2S) gaze precision --
#' how tightly clustered repeated gaze samples are, in visual-angle degrees
#' -- for three fixation-selection variants of the recording (the whole
#' file, its longest fixation, and its median-duration fixation), using a
#' rolling window sized from the recording's actual sampling rate.
#'
#' @param data dataframe
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom runner runner
#' @return summary_df dataframe describing each
#' @export
#'
calculateOutputMetrics <- function(data) {
  summarize_row_data <- data %>%
    dplyr::group_by() %>%
    dplyr::summarise(
      # denominators (number of rows)
      total_rows = dplyr::n(),
      total_final_rows = sum(ifelse(
        .data$gazeY.es.selection != "both_na", 1, 0
      )),
      # total rows of missing data, raw (validity-masked)
      total_rows_raw_na_Left = sum(ifelse(is.na(
        .data$gazeLeftX.valid
      ), 1, 0)),
      total_rows_raw_na_Right = sum(ifelse(is.na(
        .data$gazeRightX.valid
      ), 1, 0)),
      total_rows_raw_na_X = sum(ifelse(
        is.na(.data$gazeLeftX.valid) & is.na(.data$gazeRightX.valid),
        1,
        0
      )),
      total_rows_raw_na_Y = sum(ifelse(
        is.na(.data$gazeLeftY.valid) & is.na(.data$gazeRightY.valid),
        1,
        0
      )),
      total_final_rows_na = sum(ifelse(
        .data$gazeY.es.selection == "both_na", 1, 0
      )),
      # total number of blinks
      total_rows_blinks_Left = sum(ifelse(.data$pupilLeft.blink > 0, 1, 0)),
      total_rows_blinks_Right = sum(ifelse(.data$pupilRight.blink > 0, 1, 0)),
      total_rows_blinks_Both = sum(ifelse(.data$bothEyes.blink, 1, 0)),
      # COMMENTED out since there is no data where X and Y points exist but pupil data doesn't exist.
      # total_rows_PupilData_no_gaze_X = sum(ifelse(is.na(gazeLeftX) & !is.na(pupilLeft), 1, 0)),
      # total_rows_PupilData_no_gaze_Y = sum(ifelse(is.na(gazeRightX) & !is.na(pupilRight), 1, 0)),
      # total_rows_no_PupilData_gaze_X = sum(ifelse(!is.na(gazeLeftX) & is.na(pupilLeft), 1, 0)),
      # total_rows_no_PupilData_gaze_Y = sum(ifelse(!is.na(gazeRightX) & is.na(pupilRight), 1, 0)),
      # interpolated gaps filled
      total_rows_interpolated_LeftX = sum(
        ifelse(
          .data$gazeLeftX.valid %>% replace(is.na(.), 0) != .data$gazeLeftX.int %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_RightX = sum(
        ifelse(
          .data$gazeRightX.valid %>% replace(is.na(.), 0) != .data$gazeRightX.int %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_LeftY = sum(
        ifelse(
          .data$gazeLeftY.valid %>% replace(is.na(.), 0) != .data$gazeLeftY.int %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_RightY = sum(
        ifelse(
          .data$gazeRightY.valid %>% replace(is.na(.), 0) != .data$gazeRightY.int %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_LeftPupil = sum(
        ifelse(
          .data$pupilLeft.valid %>% replace(is.na(.), 0) != .data$pupilLeft.int %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_RightPupil = sum(
        ifelse(
          .data$pupilRight.valid %>% replace(is.na(.), 0) != .data$pupilRight.int %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_LeftDistZ = sum(
        ifelse(
          .data$distanceLeftZ.valid %>% replace(is.na(.), 0) != .data$distanceLeftZ.int %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_RightDistZ = sum(
        ifelse(
          .data$distanceRightZ.valid %>% replace(is.na(.), 0) != .data$distanceRightZ.int %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      # #COMMENTED OUT LH - created variables are not used and .temp columns break in other eyeselect protocols
      # eye select
      # total_rows_eye_select_LeftX = sum(
      #   ifelse(
      #     .data$gazeLeftX.int %>% replace(is.na(.), 0) != .data$gpLeft.X.temp %>% replace(is.na(.), 0),
      #     1,
      #     0
      #   )
      # ),
      # total_rows_eye_select_RightX = sum(
      #   ifelse(
      #     .data$gazeRightX.int %>% replace(is.na(.), 0) != .data$gpRight.X.temp %>% replace(is.na(.), 0),
      #     1,
      #     0
      #   )
      # ),
      # total_rows_eye_select_LeftY = sum(
      #   ifelse(
      #     .data$gazeLeftY.int %>% replace(is.na(.), 0) != .data$gpLeft.Y.temp %>% replace(is.na(.), 0),
      #     1,
      #     0
      #   )
      # ),
      # total_rows_eye_select_RightY = sum(
      #   ifelse(
      #     .data$gazeRightY.int %>% replace(is.na(.), 0) != .data$gpRight.Y.temp %>% replace(is.na(.), 0),
      #     1,
      #     0
      #   )
      # ),
      total_rows_eye_select_X_LeftOnly = sum(ifelse(
        .data$gazeX.es.selection == "left_only", 1, 0
      )),
      total_rows_eye_select_X_RightOnly = sum(ifelse(
        .data$gazeX.es.selection == "right_only", 1, 0
      )),
      total_rows_eye_select_Y_LeftOnly = sum(ifelse(
        .data$gazeY.es.selection == "left_only", 1, 0
      )),
      total_rows_eye_select_Y_RightOnly = sum(ifelse(
        .data$gazeY.es.selection == "right_only", 1, 0
      )),
      total_rows_eye_select_X_mean = sum(ifelse(
        str_detect(.data$gazeX.es.selection, "mean"), 1, 0
      )),
      total_rows_eye_select_Y_mean = sum(ifelse(
        str_detect(.data$gazeY.es.selection, "mean"), 1, 0
      )),
      mean_abs_diff_eye_select_X = mean(abs(
        ifelse(
          str_detect(.data$gazeX.es.selection, "mean"),
          .data$gazeLeftX.int,
          NA
        ) - ifelse(
          str_detect(.data$gazeX.es.selection, "mean"),
          .data$gazeX.eyeSelect,
          NA
        )
      ), na.rm = TRUE),
      mean_abs_diff_eye_select_Y = mean(abs(
        ifelse(
          str_detect(.data$gazeX.es.selection, "mean"),
          .data$gazeLeftY.int,
          NA
        ) - ifelse(
          str_detect(.data$gazeY.es.selection, "mean"),
          .data$gazeY.eyeSelect,
          NA
        )
      ), na.rm = TRUE),
      mean_abs_diff_eye_select_pupil = mean(abs(
        ifelse(
          str_detect(.data$pupil.es.selection, "mean"),
          .data$pupilLeft.int,
          NA
        ) - ifelse(
          str_detect(.data$pupil.es.selection, "mean"),
          .data$pupil.eyeSelect,
          NA
        )
      ), na.rm = TRUE),
      mean_abs_diff_eye_select_distZ = mean(abs(
        ifelse(
          str_detect(.data$distZ.es.selection, "mean"),
          .data$distanceLeftZ.int,
          NA
        ) - ifelse(
          str_detect(.data$distZ.es.selection, "mean"),
          .data$distanceZ.eyeSelect,
          NA
        )
      ), na.rm = TRUE),
      sd_abs_diff_eye_select_X = sd(abs(
        ifelse(
          str_detect(.data$gazeX.es.selection, "mean"),
          .data$gazeLeftX.int,
          NA
        ) - ifelse(
          str_detect(.data$gazeX.es.selection, "mean"),
          .data$gazeX.eyeSelect,
          NA
        )
      ), na.rm = TRUE),
      sd_abs_diff_eye_select_Y = sd(abs(
        ifelse(
          str_detect(.data$gazeX.es.selection, "mean"),
          .data$gazeLeftY.int,
          NA
        ) - ifelse(
          str_detect(.data$gazeY.es.selection, "mean"),
          .data$gazeY.eyeSelect,
          NA
        )
      ), na.rm = TRUE),
      sd_abs_diff_eye_select_pupil = sd(abs(
        ifelse(
          str_detect(.data$pupil.es.selection, "mean"),
          .data$pupilLeft.int,
          NA
        ) - ifelse(
          str_detect(.data$pupil.es.selection, "mean"),
          .data$pupil.eyeSelect,
          NA
        )
      ), na.rm = TRUE),
      sd_abs_diff_eye_select_distZ = sd(abs(
        ifelse(
          str_detect(.data$distZ.es.selection, "mean"),
          .data$distanceLeftZ.int,
          NA
        ) - ifelse(
          str_detect(.data$distZ.es.selection, "mean"),
          .data$distanceZ.eyeSelect,
          NA
        )
      ), na.rm = TRUE),
      median_eye_select_X = median(.data$gazeX.eyeSelect, na.rm = TRUE),
      median_eye_select_Y = median(.data$gazeY.eyeSelect, na.rm = TRUE),
      median_eye_select_pupil = median(.data$pupil.eyeSelect, na.rm = TRUE),
      median_eye_select_distZ = median(.data$distanceZ.eyeSelect, na.rm = TRUE),
      mean_eye_select_X = mean(.data$gazeX.eyeSelect, na.rm = TRUE),
      mean_eye_select_Y = mean(.data$gazeY.eyeSelect, na.rm = TRUE),
      mean_eye_select_pupil = mean(.data$pupil.eyeSelect, na.rm = TRUE),
      mean_eye_select_distZ = mean(.data$distanceZ.eyeSelect, na.rm = TRUE),
      min_eye_select_X = safe_min(.data$gazeX.eyeSelect),
      min_eye_select_Y = safe_min(.data$gazeY.eyeSelect),
      min_eye_select_pupil = safe_min(.data$pupil.eyeSelect),
      min_eye_select_distZ = safe_min(.data$distanceZ.eyeSelect),
      max_eye_select_X = safe_max(.data$gazeX.eyeSelect),
      max_eye_select_Y = safe_max(.data$gazeY.eyeSelect),
      max_eye_select_pupil = safe_max(.data$pupil.eyeSelect),
      max_eye_select_distZ = safe_max(.data$distanceZ.eyeSelect),
      sd_eye_select_X = sd(.data$gazeX.eyeSelect, na.rm = TRUE),
      sd_eye_select_Y = sd(.data$gazeY.eyeSelect, na.rm = TRUE),
      sd_eye_select_pupil = sd(.data$pupil.eyeSelect, na.rm = TRUE),
      sd_eye_select_distZ = sd(.data$distanceZ.eyeSelect, na.rm = TRUE),

      # smoothing
      total_rows_denoise_X = sum(
        ifelse(
          .data$gazeX.eyeSelect %>% replace(is.na(.), 0) != .data$gazeX.smooth %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_denoise_Y = sum(
        ifelse(
          .data$gazeY.eyeSelect %>% replace(is.na(.), 0) != .data$gazeY.smooth %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      mean_abs_diff_denoise_X = mean(abs(.data$gazeX.smooth - .data$gazeX.eyeSelect), na.rm = TRUE),
      mean_abs_diff_denoise_Y = mean(abs(.data$gazeY.smooth - .data$gazeY.eyeSelect), na.rm = TRUE),
      sd_abs_diff_denoise_X = sd(abs(.data$gazeX.smooth - .data$gazeX.eyeSelect), na.rm = TRUE),
      sd_abs_diff_denoise_Y = sd(abs(.data$gazeY.smooth - .data$gazeY.eyeSelect), na.rm = TRUE),
      total_rows_denoise_pupil = sum(
        ifelse(
          .data$pupil.eyeSelect %>% replace(is.na(.), 0) != .data$pupil.smooth %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_denoise_distZ = sum(
        ifelse(
          .data$distanceZ.eyeSelect %>% replace(is.na(.), 0) != .data$distanceZ.smooth %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      mean_abs_diff_denoise_pupil = mean(abs(.data$pupil.smooth - .data$pupil.eyeSelect), na.rm = TRUE),
      mean_abs_diff_denoise_distZ = mean(abs(.data$distanceZ.smooth - .data$distanceZ.eyeSelect), na.rm = TRUE),
      sd_abs_diff_denoise_pupil = sd(abs(.data$pupil.smooth - .data$pupil.eyeSelect), na.rm = TRUE),
      sd_abs_diff_denoise_distZ = sd(abs(.data$distanceZ.smooth - .data$distanceZ.eyeSelect), na.rm = TRUE),
      median_denoise_X = median(.data$gazeX.smooth, na.rm = TRUE),
      median_denoise_Y = median(.data$gazeY.smooth, na.rm = TRUE),
      median_denoise_pupil = median(.data$pupil.smooth, na.rm = TRUE),
      median_denoise_distZ = median(.data$distanceZ.smooth, na.rm = TRUE),
      mean_denoise_X = mean(.data$gazeX.smooth, na.rm = TRUE),
      mean_denoise_Y = mean(.data$gazeY.smooth, na.rm = TRUE),
      mean_denoise_pupil = mean(.data$pupil.smooth, na.rm = TRUE),
      mean_denoise_distZ = mean(.data$distanceZ.smooth, na.rm = TRUE),
      min_denoise_X = safe_min(.data$gazeX.smooth),
      min_denoise_Y = safe_min(.data$gazeY.smooth),
      min_denoise_pupil = safe_min(.data$pupil.smooth),
      min_denoise_distZ = safe_min(.data$distanceZ.smooth),
      max_denoise_X = safe_max(.data$gazeX.smooth),
      max_denoise_Y = safe_max(.data$gazeY.smooth),
      max_denoise_pupil = safe_max(.data$pupil.smooth),
      max_denoise_distZ = safe_max(.data$distanceZ.smooth),
      sd_denoise_X = sd(.data$gazeX.smooth, na.rm = TRUE),
      sd_denoise_Y = sd(.data$gazeY.smooth, na.rm = TRUE),
      sd_denoise_pupil = sd(.data$pupil.smooth, na.rm = TRUE),
      sd_denoise_distZ = sd(.data$distanceZ.smooth, na.rm = TRUE),

      # velocity smoothing
      total_rows_smoothVA_X = sum(
        ifelse(
          .data$velocityX_va_ms %>% replace(is.na(.), 0) != .data$velocityX.smooth_va_ms %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      total_rows_smoothVA_Y = sum(
        ifelse(
          .data$velocityY_va_ms %>% replace(is.na(.), 0) != .data$velocityY.smooth_va_ms %>% replace(is.na(.), 0),
          1,
          0
        )
      ),
      mean_abs_diff_smoothVA_X = mean(abs(
        .data$velocityX.smooth_va_ms - .data$velocityX_va_ms
      ), na.rm = TRUE),
      mean_abs_diff_smoothVA_Y = mean(abs(
        .data$velocityY.smooth_va_ms - .data$velocityY_va_ms
      ), na.rm = TRUE),
      sd_abs_diff_smoothVA_X = sd(abs(
        .data$velocityX.smooth_va_ms - .data$velocityX_va_ms
      ), na.rm = TRUE),
      sd_abs_diff_smoothVA_Y = sd(abs(
        .data$velocityY.smooth_va_ms - .data$velocityY_va_ms
      ), na.rm = TRUE),
      median_smoothVA_X = median(.data$velocityX.smooth_va_ms, na.rm = TRUE),
      median_smoothVA_Y = median(.data$velocityY.smooth_va_ms, na.rm = TRUE),
      mean_smoothVA_X = mean(.data$velocityX.smooth_va_ms, na.rm = TRUE),
      mean_smoothVA_Y = mean(.data$velocityY.smooth_va_ms, na.rm = TRUE),
      min_smoothVA_X = safe_min(.data$velocityX.smooth_va_ms),
      min_smoothVA_Y = safe_min(.data$velocityY.smooth_va_ms),
      max_smoothVA_X = safe_max(.data$velocityX.smooth_va_ms),
      max_smoothVA_Y = safe_max(.data$velocityY.smooth_va_ms),
      sd_smoothVA_X = sd(.data$velocityX.smooth_va_ms, na.rm = TRUE),
      sd_smoothVA_Y = sd(.data$velocityY.smooth_va_ms, na.rm = TRUE),

      # gaze points within fixation/saccade
      total_rows_fixation = sum(ifelse(
        .data$IVT.classification == "fixation", 1, 0
      )),
      total_rows_saccade = sum(ifelse(
        .data$IVT.classification == "saccade", 1, 0
      )),
      total_rows_unclassified = sum(ifelse(
        .data$IVT.classification == "unclassified", 1, 0
      )),
      total_rows_missing = sum(ifelse(
        .data$IVT.classification == "missing", 1, 0
      )),
      total_rows_blink = sum(ifelse(
        .data$IVT.classification == "blink", 1, 0
      )),
      # total_number_fixations = max(IVT.fixationIndex, na.rm = TRUE),
      # total_number_saccades = max(IVT.saccadeIndex, na.rm = TRUE),
    )

  # summary_output <- summarize_row_data %>%
  #   dplyr::summarize(
  #     #percentages out of all rows
  #     percent_missing_raw_data_LeftEye = sprintf("%0.2f%%, (%d/%d)", total_rows_raw_na_Left/total_rows * 100, total_rows_raw_na_Left, total_rows),
  #     percent_missing_raw_data_RightEye = sprintf("%0.2f%%, (%d/%d)", total_rows_raw_na_Right/total_rows * 100, total_rows_raw_na_Right, total_rows),
  #     percent_missing_raw_data_BothEyes = sprintf("%0.2f%%, (%d/%d)", total_rows_raw_na_X/total_rows * 100, total_rows_raw_na_X, total_rows),
  #     #COMMENTED OUT since X and Y data is identical, since no points with X data but not Y data.
  #     # percent_missing_raw_data_X = sprintf("%0.2f%%, (%d/%d)", total_rows_raw_na_X/total_rows * 100, total_rows_raw_na_X, total_rows),
  #     # percent_missing_raw_data_Y = sprintf("%0.2f%%, (%d/%d)", total_rows_raw_na_Y/total_rows * 100, total_rows_raw_na_Y, total_rows),
  #     percent_valid_raw_data = sprintf("%0.2f%%, (%d/%d)", (total_rows-total_rows_raw_na_X)/total_rows * 100, total_rows_raw_na_X, total_rows),
  #     percent_blinks_LeftEye = sprintf("%0.2f%%, (%d/%d)", total_rows_blinks_Left/total_rows * 100, total_rows_blinks_Left, total_rows),
  #     percent_blinks_RightEye = sprintf("%0.2f%%, (%d/%d)", total_rows_blinks_Right/total_rows * 100, total_rows_blinks_Right, total_rows),
  #     percent_blinks_BothEyes = sprintf("%0.2f%%, (%d/%d)", total_rows_blinks_Both/total_rows * 100, total_rows_blinks_Both, total_rows),
  #     #percentages out of valid data points
  #     percent_final_na = sprintf("%0.2f%%, (%d/%d)", total_final_rows_na/(total_final_rows_na+total_final_rows) * 100, total_final_rows_na, (total_final_rows_na+total_final_rows)),
  #     percent_final_valid = sprintf("%0.2f%%, (%d/%d)", total_final_rows/(total_final_rows_na+total_final_rows) * 100, total_final_rows, (total_final_rows_na+total_final_rows)),
  #     #how much of the data has been interpolated?
  #     percent_interpolated_LeftEye = sprintf("%0.2f%%, (%d/%d)", total_rows_interpolated_LeftX/total_final_rows * 100, total_rows_interpolated_LeftX, total_final_rows),
  #     percent_interpolated_RightEye = sprintf("%0.2f%%, (%d/%d)", total_rows_interpolated_RightX/total_final_rows * 100, total_rows_interpolated_RightX, total_final_rows),
  #     #COMMENTED OUT since no data with X and Y data, but not pupil and/or z distance
  #     # percent_interpolated_LeftPupil = sprintf("%0.2f%%, (%d/%d)", total_rows_interpolated_LeftPupil/total_final_rows * 100, total_rows_interpolated_LeftPupil, total_final_rows),
  #     # percent_interpolated_RightPupil = sprintf("%0.2f%%, (%d/%d)", total_rows_interpolated_RightPupil/total_final_rows * 100, total_rows_interpolated_RightPupil, total_final_rows),
  #     # percent_interpolated_LeftDistZ = sprintf("%0.2f%%, (%d/%d)", total_rows_interpolated_LeftDistZ/total_final_rows * 100, total_rows_interpolated_LeftDistZ, total_final_rows),
  #     # percent_interpolated_RightDistZ = sprintf("%0.2f%%, (%d/%d)", total_rows_interpolated_RightDistZ/total_final_rows * 100, total_rows_interpolated_RightDistZ, total_final_rows),
  #     #how much of the valid data is coming from a single eye, eye select
  #     percent_eye_selected_LeftOnly = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_X_LeftOnly/total_final_rows * 100, total_rows_eye_select_X_LeftOnly, total_final_rows),
  #     percent_eye_selected_RightOnly = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_X_RightOnly/total_final_rows * 100, total_rows_eye_select_X_RightOnly, total_final_rows),
  #     percent_eye_selected_mean_X = sprintf("%0.2f%%, (%d/%d), mean absolute difference: %0.3f, sd: %0.3f", total_rows_eye_select_X_mean/total_final_rows * 100, total_rows_eye_select_X_mean, total_final_rows, mean_abs_diff_eye_select_X, sd_abs_diff_eye_select_X),
  #     percent_eye_selected_mean_Y = sprintf("%0.2f%%, (%d/%d), mean absolute difference: %0.3f, sd: %0.3f", total_rows_eye_select_X_mean/total_final_rows * 100, total_rows_eye_select_X_mean, total_final_rows, mean_abs_diff_eye_select_Y, sd_abs_diff_eye_select_Y),
  #     percent_eye_selected_mean_pupil = sprintf("%0.2f%%, (%d/%d), mean absolute difference: %0.3f, sd: %0.3f", total_rows_eye_select_X_mean/total_final_rows * 100, total_rows_eye_select_X_mean, total_final_rows, mean_abs_diff_eye_select_pupil, sd_abs_diff_eye_select_pupil),
  #     percent_eye_selected_mean_distZ = sprintf("%0.2f%%, (%d/%d), mean absolute difference: %0.3f, sd: %0.3f", total_rows_eye_select_X_mean/total_final_rows * 100, total_rows_eye_select_X_mean, total_final_rows, mean_abs_diff_eye_select_distZ, sd_abs_diff_eye_select_distZ),
  #     #COMMENTED OUT since X replaced and Y replace are always the same, since no point has only X or only Y
  #     # percent_eye_select_na_Left_replaced = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_LeftX/total_final_rows * 100, total_rows_eye_select_LeftX, total_final_rows),
  #     # percent_eye_select_na_Right_replaced = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_RightX/total_final_rows * 100, total_rows_eye_select_RightX, total_final_rows),
  #     # percent_eye_select_na_LeftX_replaced = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_LeftX/total_final_rows * 100, total_rows_eye_select_LeftX, total_final_rows),
  #     # percent_eye_select_na_LeftY_replaced = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_LeftY/total_rows * 100, total_rows_eye_select_LeftY, total_rows),
  #     # percent_eye_select_na_RightX_replaced = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_RightX/total_final_rows * 100, total_rows_eye_select_RightX, total_final_rows),
  #     # percent_eye_select_na_RightY_replaced = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_RightY/total_rows * 100, total_rows_eye_select_RightY, total_rows),
  #     # percent_eye_selected_mean_X = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_X_mean/total_final_rows * 100, total_rows_eye_select_X_mean, total_final_rows),
  #     # percent_eye_selected_mean_Y = sprintf("%0.2f%%, (%d/%d)", total_rows_eye_select_Y_mean/total_final_rows * 100, total_rows_eye_select_Y_mean, total_final_rows),
  #     #how much has the final gaze data been smoothed?
  #     percent_smoothed_X = sprintf("%0.2f%%, (%d/%d), mean absolute difference: %0.3f, sd: %0.3f", total_rows_denoise_X/total_final_rows * 100, total_rows_denoise_X, total_final_rows, mean_abs_diff_denoise_X, sd_abs_diff_denoise_X),
  #     percent_smoothed_Y = sprintf("%0.2f%%, (%d/%d), mean absolute difference: %0.3f, sd: %0.3f", total_rows_denoise_Y/total_final_rows * 100, total_rows_denoise_Y, total_final_rows, mean_abs_diff_denoise_Y, sd_abs_diff_denoise_Y),
  #     percent_smoothed_velocity_X = sprintf("%0.2f%%, (%d/%d), mean absolute difference: %0.3f, sd: %0.3f", total_rows_smoothVA_X/total_final_rows * 100, total_rows_smoothVA_X, total_final_rows, mean_abs_diff_smoothVA_X, sd_abs_diff_smoothVA_X),
  #     percent_smoothed_velocity_Y = sprintf("%0.2f%%, (%d/%d), mean absolute difference: %0.3f, sd: %0.3f", total_rows_smoothVA_Y/total_final_rows * 100, total_rows_smoothVA_Y, total_final_rows, mean_abs_diff_smoothVA_Y, sd_abs_diff_smoothVA_Y),
  #     percent_gaze_points_part_of_fixation = sprintf("%0.2f%%, (%d/%d), n: %d", total_rows_fixation/total_final_rows * 100, total_rows_fixation, total_final_rows, total_number_fixations),
  #     percent_gaze_points_part_of_saccade = sprintf("%0.2f%%, (%d/%d), n: %d", total_rows_saccade/total_final_rows * 100, total_rows_saccade, total_final_rows, total_number_saccades),
  #     percent_gaze_points_part_of_unclassified = sprintf("%0.2f%%, (%d/%d)", total_rows_unclassified/total_final_rows * 100, total_rows_unclassified, total_final_rows),
  #     percent_gaze_points_part_of_missing = sprintf("%0.2f%%, (%d/%d)", total_rows_missing/total_rows * 100, total_rows_missing, total_rows),
  #     # percent_gaze_points_part_of_fixation = sprintf("%0.2f%%, (%d/%d), description: [n=%d|mean=%0.2f|max=%d|min=%d|sd=%0.2f]", total_rows_fixation/total_final_rows * 100, total_rows_fixation, total_final_rows, total_number_fixations, mean_duration_fixations, min_duration_fixations, max_duration_fixations, sd_duration_fixations),
  #   )
  # print(summary_output %>% t())

  # fixation_info <- data %>%
  #   dplyr::select(IVT.fixationIndex, IVT.fixationDuration_ms) %>%
  #   unique() %>%
  #   dplyr::summarise(
  #     label = "fixation duration",
  #     median = median(IVT.fixationDuration_ms, na.rm=TRUE),
  #     mean = mean(IVT.fixationDuration_ms, na.rm=TRUE),
  #     min = min(IVT.fixationDuration_ms, na.rm=TRUE),
  #     max = max(IVT.fixationDuration_ms, na.rm=TRUE),
  #     sd = sd(IVT.fixationDuration_ms, na.rm=TRUE))

  fixation_info <- data %>%
    getSequenceGroupEndpoints("IVT.classification") %>%
    filter(.data$group == "fixation") %>%
    summarize(
      label = "fixation duration",
      count = n(),
      median = median(.data$duration, na.rm = TRUE),
      mean = mean(.data$duration, na.rm = TRUE),
      min = safe_min(.data$duration),
      max = safe_max(.data$duration),
      sd = sd(.data$duration, na.rm = TRUE)
    )

  saccade_info <- data %>%
    getSequenceGroupEndpoints("IVT.classification") %>%
    filter(.data$group == "saccade") %>%
    summarize(
      label = "saccade duration",
      count = n(),
      median = median(.data$duration, na.rm = TRUE),
      mean = mean(.data$duration, na.rm = TRUE),
      min = safe_min(.data$duration),
      max = safe_max(.data$duration),
      sd = sd(.data$duration, na.rm = TRUE)
    )

  blink_info <- data %>%
    getSequenceGroupEndpoints("IVT.classification") %>%
    filter(.data$group == "blink") %>%
    summarize(
      label = "blink duration",
      count = n(),
      median = median(.data$duration, na.rm = TRUE),
      mean = mean(.data$duration, na.rm = TRUE),
      min = safe_min(.data$duration),
      max = safe_max(.data$duration),
      sd = sd(.data$duration, na.rm = TRUE)
    )

  missing_info <- data %>%
    getSequenceGroupEndpoints("IVT.classification") %>%
    filter(.data$group == "missing") %>%
    summarize(
      label = "missing duration",
      count = n(),
      median = median(.data$duration, na.rm = TRUE),
      mean = mean(.data$duration, na.rm = TRUE),
      min = safe_min(.data$duration),
      max = safe_max(.data$duration),
      sd = sd(.data$duration, na.rm = TRUE)
    )

  unclassified_info <- data %>%
    getSequenceGroupEndpoints("IVT.classification") %>%
    filter(.data$group == "unclassified") %>%
    summarize(
      label = "unclassified duration",
      count = n(),
      median = median(.data$duration, na.rm = TRUE),
      mean = mean(.data$duration, na.rm = TRUE),
      min = safe_min(.data$duration),
      max = safe_max(.data$duration),
      sd = sd(.data$duration, na.rm = TRUE)
    )

  # Precision (SD + RMS-S2S), computed for three fixation-selection
  # variants. The rolling window size is derived from this recording's
  # actual sampling rate (calculateFrequency_hz(), on the full,
  # unsubsetted file's timestamps) rather than a hardcoded frequency
  # assumption, computed once and reused across all three variants --
  # sampling rate is a property of the recording/device, not of an
  # arbitrary fixation subset.
  precision_window_size_samples <- .precision_window_size_samples(data, window_ms = 100)

  precision_wholeFile <- .calculate_precision_metrics(data, "all", precision_window_size_samples)
  precision_longestFixation <- .calculate_precision_metrics(data, "longest", precision_window_size_samples)
  precision_medianFixation <- .calculate_precision_metrics(data, "median", precision_window_size_samples)

  # setup output dataframe
  summary_df_rows <- c(
    "missing_raw_data_LeftEye",
    "missing_raw_data_RightEye",
    "missing_raw_data_BothEyes",
    "valid_raw_data",
    "blinks_LeftEye",
    "blinks_RightEye",
    "blinks_BothEyes",
    "final_na",
    "final_valid",
    "interpolated_LeftEye",
    "interpolated_RightEye",
    "eye_select_LeftOnly",
    "eye_select_RightOnly",
    "eye_select_mean",
    "eye_selected_mean_X",
    "eye_selected_mean_Y",
    "eye_selected_mean_pupil",
    "eye_selected_mean_distZ",
    "smoothed_X",
    "smoothed_Y",
    "smoothed_pupil",
    "smoothed_distZ",
    "smoothed_velocity_X",
    "smoothed_velocity_Y",
    "ivt_fixations",
    "ivt_saccades",
    "ivt_unclassified",
    "ivt_missing",
    "precision_sdX_wholeFile",
    "precision_sdY_wholeFile",
    "precision_rmsX_mean_wholeFile",
    "precision_rmsX_median_wholeFile",
    "precision_rmsY_mean_wholeFile",
    "precision_rmsY_median_wholeFile",
    "precision_rmsEuc_mean_wholeFile",
    "precision_rmsEuc_median_wholeFile",
    "precision_sdX_longestFixation",
    "precision_sdY_longestFixation",
    "precision_rmsX_mean_longestFixation",
    "precision_rmsX_median_longestFixation",
    "precision_rmsY_mean_longestFixation",
    "precision_rmsY_median_longestFixation",
    "precision_rmsEuc_mean_longestFixation",
    "precision_rmsEuc_median_longestFixation",
    "precision_sdX_medianFixation",
    "precision_sdY_medianFixation",
    "precision_rmsX_mean_medianFixation",
    "precision_rmsX_median_medianFixation",
    "precision_rmsY_mean_medianFixation",
    "precision_rmsY_median_medianFixation",
    "precision_rmsEuc_mean_medianFixation",
    "precision_rmsEuc_median_medianFixation"
  )
  summary_df_columns <- c(
    "n",
    "percent",
    "percent_numerator",
    "percent_denominator",
    "mean_absolute_difference",
    "sd_absolute_difference",
    "group_n",
    "group_median",
    "group_mean",
    "group_min",
    "group_max",
    "group_sd",
    "precision_value"
  )
  summary_df <-
    data.frame(matrix(
      nrow = length(summary_df_rows),
      ncol = length(summary_df_columns)
    ))
  rownames(summary_df) <- summary_df_rows
  colnames(summary_df) <- summary_df_columns

  # save data to output dataframe
  summary_df[["missing_raw_data_LeftEye", "n"]] <-
    summarize_row_data$total_rows_raw_na_Left[1]
  summary_df[["missing_raw_data_LeftEye", "percent"]] <-
    round(
      summarize_row_data$total_rows_raw_na_Left[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["missing_raw_data_LeftEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_raw_na_Left[1]
  summary_df[["missing_raw_data_LeftEye", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["missing_raw_data_RightEye", "n"]] <-
    summarize_row_data$total_rows_raw_na_Right[1]
  summary_df[["missing_raw_data_RightEye", "percent"]] <-
    round(
      summarize_row_data$total_rows_raw_na_Right[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["missing_raw_data_RightEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_raw_na_Right[1]
  summary_df[["missing_raw_data_RightEye", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["missing_raw_data_BothEyes", "n"]] <-
    summarize_row_data$total_rows_raw_na_X[1]
  summary_df[["missing_raw_data_BothEyes", "percent"]] <-
    round(
      summarize_row_data$total_rows_raw_na_X[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["missing_raw_data_BothEyes", "percent_numerator"]] <-
    summarize_row_data$total_rows_raw_na_X[1]
  summary_df[["missing_raw_data_BothEyes", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["valid_raw_data", "n"]] <-
    summarize_row_data$total_rows[1] - summarize_row_data$total_rows_raw_na_X[1]
  summary_df[["valid_raw_data", "percent"]] <-
    round(
      (
        summarize_row_data$total_rows[1] - summarize_row_data$total_rows_raw_na_X[1]
      ) / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["valid_raw_data", "percent_numerator"]] <-
    summarize_row_data$total_rows[1] - summarize_row_data$total_rows_raw_na_X[1]
  summary_df[["valid_raw_data", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["blinks_LeftEye", "n"]] <-
    summarize_row_data$total_rows_blinks_Left[1]
  summary_df[["blinks_LeftEye", "percent"]] <-
    round(
      summarize_row_data$total_rows_blinks_Left[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["blinks_LeftEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_blinks_Left[1]
  summary_df[["blinks_LeftEye", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["blinks_RightEye", "n"]] <-
    summarize_row_data$total_rows_blinks_Right[1]
  summary_df[["blinks_RightEye", "percent"]] <-
    round(
      summarize_row_data$total_rows_blinks_Right[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["blinks_RightEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_blinks_Right[1]
  summary_df[["blinks_RightEye", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["blinks_BothEyes", "n"]] <-
    summarize_row_data$total_rows_blinks_Both[1]
  summary_df[["blinks_BothEyes", "percent"]] <-
    round(
      summarize_row_data$total_rows_blinks_Both[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["blinks_BothEyes", "percent_numerator"]] <-
    summarize_row_data$total_rows_blinks_Both[1]
  summary_df[["blinks_BothEyes", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["final_na", "n"]] <-
    summarize_row_data$total_final_rows_na[1]
  summary_df[["final_na", "percent"]] <-
    round(
      summarize_row_data$total_final_rows_na[1] / (
        summarize_row_data$total_final_rows_na[1] + summarize_row_data$total_final_rows[1]
      ),
      4
    )
  summary_df[["final_na", "percent_numerator"]] <-
    summarize_row_data$total_final_rows_na[1]
  summary_df[["final_na", "percent_denominator"]] <-
    (summarize_row_data$total_final_rows_na[1] + summarize_row_data$total_final_rows[1])

  summary_df[["final_valid", "n"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["final_valid", "percent"]] <-
    round(
      summarize_row_data$total_final_rows[1] / (
        summarize_row_data$total_final_rows_na[1] + summarize_row_data$total_final_rows[1]
      ),
      4
    )
  summary_df[["final_valid", "percent_numerator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["final_valid", "percent_denominator"]] <-
    (summarize_row_data$total_final_rows_na[1] + summarize_row_data$total_final_rows[1])

  summary_df[["interpolated_LeftEye", "n"]] <-
    summarize_row_data$total_rows_interpolated_LeftX[1]
  summary_df[["interpolated_LeftEye", "percent"]] <-
    round(
      summarize_row_data$total_rows_interpolated_LeftX[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["interpolated_LeftEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_interpolated_LeftX[1]
  summary_df[["interpolated_LeftEye", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]

  summary_df[["interpolated_RightEye", "n"]] <-
    summarize_row_data$total_rows_interpolated_RightX[1]
  summary_df[["interpolated_RightEye", "percent"]] <-
    round(
      summarize_row_data$total_rows_interpolated_RightX[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["interpolated_RightEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_interpolated_RightX[1]
  summary_df[["interpolated_RightEye", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]

  summary_df[["eye_select_LeftOnly", "n"]] <-
    summarize_row_data$total_rows_eye_select_X_LeftOnly[1]
  summary_df[["eye_select_LeftOnly", "percent"]] <-
    round(
      summarize_row_data$total_rows_eye_select_X_LeftOnly[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["eye_select_LeftOnly", "percent_numerator"]] <-
    summarize_row_data$total_rows_eye_select_X_LeftOnly[1]
  summary_df[["eye_select_LeftOnly", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]

  summary_df[["eye_select_RightOnly", "n"]] <-
    summarize_row_data$total_rows_eye_select_X_RightOnly[1]
  summary_df[["eye_select_RightOnly", "percent"]] <-
    round(
      summarize_row_data$total_rows_eye_select_X_RightOnly[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["eye_select_RightOnly", "percent_numerator"]] <-
    summarize_row_data$total_rows_eye_select_X_RightOnly[1]
  summary_df[["eye_select_RightOnly", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]

  summary_df[["eye_select_mean", "n"]] <-
    summarize_row_data$total_rows_eye_select_X_mean[1]
  summary_df[["eye_select_mean", "percent"]] <-
    round(
      summarize_row_data$total_rows_eye_select_X_mean[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["eye_select_mean", "percent_numerator"]] <-
    summarize_row_data$total_rows_eye_select_X_mean[1]
  summary_df[["eye_select_mean", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]

  summary_df[["eye_selected_mean_X", "n"]] <-
    summarize_row_data$total_rows_eye_select_X_mean[1]
  summary_df[["eye_selected_mean_X", "percent"]] <-
    round(
      summarize_row_data$total_rows_eye_select_X_mean[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["eye_selected_mean_X", "percent_numerator"]] <-
    summarize_row_data$total_rows_eye_select_X_mean[1]
  summary_df[["eye_selected_mean_X", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["eye_selected_mean_X", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_eye_select_X[1]
  summary_df[["eye_selected_mean_X", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_eye_select_X[1]
  summary_df[["eye_selected_mean_X", "group_n"]] <- 1
  summary_df[["eye_selected_mean_X", "group_median"]] <-
    summarize_row_data$median_eye_select_X[1]
  summary_df[["eye_selected_mean_X", "group_mean"]] <-
    summarize_row_data$mean_eye_select_X[1]
  summary_df[["eye_selected_mean_X", "group_min"]] <-
    summarize_row_data$min_eye_select_X[1]
  summary_df[["eye_selected_mean_X", "group_max"]] <-
    summarize_row_data$max_eye_select_X[1]
  summary_df[["eye_selected_mean_X", "group_sd"]] <-
    summarize_row_data$sd_eye_select_X[1]

  summary_df[["eye_selected_mean_Y", "n"]] <-
    summarize_row_data$total_rows_eye_select_Y_mean[1]
  summary_df[["eye_selected_mean_Y", "percent"]] <-
    round(
      summarize_row_data$total_rows_eye_select_Y_mean[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["eye_selected_mean_Y", "percent_numerator"]] <-
    summarize_row_data$total_rows_eye_select_Y_mean[1]
  summary_df[["eye_selected_mean_Y", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["eye_selected_mean_Y", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_eye_select_Y[1]
  summary_df[["eye_selected_mean_Y", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_eye_select_Y[1]
  summary_df[["eye_selected_mean_Y", "group_n"]] <- 1
  summary_df[["eye_selected_mean_Y", "group_median"]] <-
    summarize_row_data$median_eye_select_Y[1]
  summary_df[["eye_selected_mean_Y", "group_mean"]] <-
    summarize_row_data$mean_eye_select_Y[1]
  summary_df[["eye_selected_mean_Y", "group_min"]] <-
    summarize_row_data$min_eye_select_Y[1]
  summary_df[["eye_selected_mean_Y", "group_max"]] <-
    summarize_row_data$max_eye_select_Y[1]
  summary_df[["eye_selected_mean_Y", "group_sd"]] <-
    summarize_row_data$sd_eye_select_Y[1]

  summary_df[["eye_selected_mean_pupil", "n"]] <-
    summarize_row_data$total_rows_eye_select_X_mean[1]
  summary_df[["eye_selected_mean_pupil", "percent"]] <-
    round(
      summarize_row_data$total_rows_eye_select_X_mean[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["eye_selected_mean_pupil", "percent_numerator"]] <-
    summarize_row_data$total_rows_eye_select_X_mean[1]
  summary_df[["eye_selected_mean_pupil", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["eye_selected_mean_pupil", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_eye_select_pupil[1]
  summary_df[["eye_selected_mean_pupil", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_eye_select_pupil[1]
  summary_df[["eye_selected_mean_pupil", "group_n"]] <- 1
  summary_df[["eye_selected_mean_pupil", "group_median"]] <-
    summarize_row_data$median_eye_select_pupil[1]
  summary_df[["eye_selected_mean_pupil", "group_mean"]] <-
    summarize_row_data$mean_eye_select_pupil[1]
  summary_df[["eye_selected_mean_pupil", "group_min"]] <-
    summarize_row_data$min_eye_select_pupil[1]
  summary_df[["eye_selected_mean_pupil", "group_max"]] <-
    summarize_row_data$max_eye_select_pupil[1]
  summary_df[["eye_selected_mean_pupil", "group_sd"]] <-
    summarize_row_data$sd_eye_select_pupil[1]

  summary_df[["eye_selected_mean_distZ", "n"]] <-
    summarize_row_data$total_rows_eye_select_X_mean[1]
  summary_df[["eye_selected_mean_distZ", "percent"]] <-
    round(
      summarize_row_data$total_rows_eye_select_X_mean[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["eye_selected_mean_distZ", "percent_numerator"]] <-
    summarize_row_data$total_rows_eye_select_X_mean[1]
  summary_df[["eye_selected_mean_distZ", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["eye_selected_mean_distZ", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_eye_select_distZ[1]
  summary_df[["eye_selected_mean_distZ", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_eye_select_distZ[1]
  summary_df[["eye_selected_mean_distZ", "group_n"]] <- 1
  summary_df[["eye_selected_mean_distZ", "group_median"]] <-
    summarize_row_data$median_eye_select_distZ[1]
  summary_df[["eye_selected_mean_distZ", "group_mean"]] <-
    summarize_row_data$mean_eye_select_distZ[1]
  summary_df[["eye_selected_mean_distZ", "group_min"]] <-
    summarize_row_data$min_eye_select_distZ[1]
  summary_df[["eye_selected_mean_distZ", "group_max"]] <-
    summarize_row_data$max_eye_select_distZ[1]
  summary_df[["eye_selected_mean_distZ", "group_sd"]] <-
    summarize_row_data$sd_eye_select_distZ[1]

  summary_df[["smoothed_X", "n"]] <-
    summarize_row_data$total_rows_denoise_X[1]
  summary_df[["smoothed_X", "percent"]] <-
    round(
      summarize_row_data$total_rows_denoise_X[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["smoothed_X", "percent_numerator"]] <-
    summarize_row_data$total_rows_denoise_X[1]
  summary_df[["smoothed_X", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["smoothed_X", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_denoise_X[1]
  summary_df[["smoothed_X", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_denoise_X[1]
  summary_df[["smoothed_X", "group_n"]] <- 1
  summary_df[["smoothed_X", "group_median"]] <-
    summarize_row_data$median_denoise_X[1]
  summary_df[["smoothed_X", "group_mean"]] <-
    summarize_row_data$mean_denoise_X[1]
  summary_df[["smoothed_X", "group_min"]] <-
    summarize_row_data$min_denoise_X[1]
  summary_df[["smoothed_X", "group_max"]] <-
    summarize_row_data$max_denoise_X[1]
  summary_df[["smoothed_X", "group_sd"]] <-
    summarize_row_data$sd_denoise_X[1]

  summary_df[["smoothed_Y", "n"]] <-
    summarize_row_data$total_rows_denoise_Y[1]
  summary_df[["smoothed_Y", "percent"]] <-
    round(
      summarize_row_data$total_rows_denoise_Y[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["smoothed_Y", "percent_numerator"]] <-
    summarize_row_data$total_rows_denoise_Y[1]
  summary_df[["smoothed_Y", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["smoothed_Y", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_denoise_Y[1]
  summary_df[["smoothed_Y", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_denoise_Y[1]
  summary_df[["smoothed_Y", "group_n"]] <- 1
  summary_df[["smoothed_Y", "group_median"]] <-
    summarize_row_data$median_denoise_Y[1]
  summary_df[["smoothed_Y", "group_mean"]] <-
    summarize_row_data$mean_denoise_Y[1]
  summary_df[["smoothed_Y", "group_min"]] <-
    summarize_row_data$min_denoise_Y[1]
  summary_df[["smoothed_Y", "group_max"]] <-
    summarize_row_data$max_denoise_Y[1]
  summary_df[["smoothed_Y", "group_sd"]] <-
    summarize_row_data$sd_denoise_Y[1]

  summary_df[["smoothed_pupil", "n"]] <-
    summarize_row_data$total_rows_denoise_pupil[1]
  summary_df[["smoothed_pupil", "percent"]] <-
    round(
      summarize_row_data$total_rows_denoise_pupil[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["smoothed_pupil", "percent_numerator"]] <-
    summarize_row_data$total_rows_denoise_pupil[1]
  summary_df[["smoothed_pupil", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["smoothed_pupil", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_denoise_pupil[1]
  summary_df[["smoothed_pupil", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_denoise_pupil[1]
  summary_df[["smoothed_pupil", "group_n"]] <- 1
  summary_df[["smoothed_pupil", "group_median"]] <-
    summarize_row_data$median_denoise_pupil[1]
  summary_df[["smoothed_pupil", "group_mean"]] <-
    summarize_row_data$mean_denoise_pupil[1]
  summary_df[["smoothed_pupil", "group_min"]] <-
    summarize_row_data$min_denoise_pupil[1]
  summary_df[["smoothed_pupil", "group_max"]] <-
    summarize_row_data$max_denoise_pupil[1]
  summary_df[["smoothed_pupil", "group_sd"]] <-
    summarize_row_data$sd_denoise_pupil[1]

  summary_df[["smoothed_distZ", "n"]] <-
    summarize_row_data$total_rows_denoise_distZ[1]
  summary_df[["smoothed_distZ", "percent"]] <-
    round(
      summarize_row_data$total_rows_denoise_distZ[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["smoothed_distZ", "percent_numerator"]] <-
    summarize_row_data$total_rows_denoise_distZ[1]
  summary_df[["smoothed_distZ", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["smoothed_distZ", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_denoise_distZ[1]
  summary_df[["smoothed_distZ", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_denoise_distZ[1]
  summary_df[["smoothed_distZ", "group_n"]] <- 1
  summary_df[["smoothed_distZ", "group_median"]] <-
    summarize_row_data$median_denoise_distZ[1]
  summary_df[["smoothed_distZ", "group_mean"]] <-
    summarize_row_data$mean_denoise_distZ[1]
  summary_df[["smoothed_distZ", "group_min"]] <-
    summarize_row_data$min_denoise_distZ[1]
  summary_df[["smoothed_distZ", "group_max"]] <-
    summarize_row_data$max_denoise_distZ[1]
  summary_df[["smoothed_distZ", "group_sd"]] <-
    summarize_row_data$sd_denoise_distZ[1]

  summary_df[["smoothed_velocity_X", "n"]] <-
    summarize_row_data$total_rows_smoothVA_X[1]
  summary_df[["smoothed_velocity_X", "percent"]] <-
    round(
      summarize_row_data$total_rows_smoothVA_X[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["smoothed_velocity_X", "percent_numerator"]] <-
    summarize_row_data$total_rows_smoothVA_X[1]
  summary_df[["smoothed_velocity_X", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["smoothed_velocity_X", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_smoothVA_X[1]
  summary_df[["smoothed_velocity_X", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_smoothVA_X[1]
  summary_df[["smoothed_velocity_X", "group_n"]] <- 1
  summary_df[["smoothed_velocity_X", "group_median"]] <-
    summarize_row_data$median_smoothVA_X[1]
  summary_df[["smoothed_velocity_X", "group_mean"]] <-
    summarize_row_data$mean_smoothVA_X[1]
  summary_df[["smoothed_velocity_X", "group_min"]] <-
    summarize_row_data$min_smoothVA_X[1]
  summary_df[["smoothed_velocity_X", "group_max"]] <-
    summarize_row_data$max_smoothVA_X[1]
  summary_df[["smoothed_velocity_X", "group_sd"]] <-
    summarize_row_data$sd_smoothVA_X[1]

  summary_df[["smoothed_velocity_Y", "n"]] <-
    summarize_row_data$total_rows_smoothVA_Y[1]
  summary_df[["smoothed_velocity_Y", "percent"]] <-
    round(
      summarize_row_data$total_rows_smoothVA_Y[1] / summarize_row_data$total_final_rows[1],
      4
    )
  summary_df[["smoothed_velocity_Y", "percent_numerator"]] <-
    summarize_row_data$total_rows_smoothVA_Y[1]
  summary_df[["smoothed_velocity_Y", "percent_denominator"]] <-
    summarize_row_data$total_final_rows[1]
  summary_df[["smoothed_velocity_Y", "mean_absolute_difference"]] <-
    summarize_row_data$mean_abs_diff_smoothVA_Y[1]
  summary_df[["smoothed_velocity_Y", "sd_absolute_difference"]] <-
    summarize_row_data$sd_abs_diff_smoothVA_Y[1]
  summary_df[["smoothed_velocity_Y", "group_n"]] <- 1
  summary_df[["smoothed_velocity_Y", "group_median"]] <-
    summarize_row_data$median_smoothVA_Y[1]
  summary_df[["smoothed_velocity_Y", "group_mean"]] <-
    summarize_row_data$mean_smoothVA_Y[1]
  summary_df[["smoothed_velocity_Y", "group_min"]] <-
    summarize_row_data$min_smoothVA_Y[1]
  summary_df[["smoothed_velocity_Y", "group_max"]] <-
    summarize_row_data$max_smoothVA_Y[1]
  summary_df[["smoothed_velocity_Y", "group_sd"]] <-
    summarize_row_data$sd_smoothVA_Y[1]

  summary_df[["ivt_fixations", "n"]] <-
    summarize_row_data$total_rows_fixation[1]
  summary_df[["ivt_fixations", "percent"]] <-
    round(
      summarize_row_data$total_rows_fixation[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["ivt_fixations", "percent_numerator"]] <-
    summarize_row_data$total_rows_fixation[1]
  summary_df[["ivt_fixations", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]
  # summary_df[["ivt_fixations", "group_n"]] <- summarize_row_data$total_number_fixations[1]
  summary_df[["ivt_fixations", "group_n"]] <-
    fixation_info[[1, "count"]]
  summary_df[["ivt_fixations", "group_median"]] <-
    fixation_info[[1, "median"]]
  summary_df[["ivt_fixations", "group_mean"]] <-
    fixation_info[[1, "mean"]]
  summary_df[["ivt_fixations", "group_min"]] <-
    fixation_info[[1, "min"]]
  summary_df[["ivt_fixations", "group_max"]] <-
    fixation_info[[1, "max"]]
  summary_df[["ivt_fixations", "group_sd"]] <-
    fixation_info[[1, "sd"]]

  summary_df[["ivt_saccades", "n"]] <-
    summarize_row_data$total_rows_saccade[1]
  summary_df[["ivt_saccades", "percent"]] <-
    round(
      summarize_row_data$total_rows_saccade[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["ivt_saccades", "percent_numerator"]] <-
    summarize_row_data$total_rows_saccade[1]
  summary_df[["ivt_saccades", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]
  # summary_df[["ivt_saccades", "group_n"]] <- summarize_row_data$total_number_saccades[1]
  summary_df[["ivt_saccades", "group_n"]] <-
    saccade_info[[1, "count"]]
  summary_df[["ivt_saccades", "group_median"]] <-
    saccade_info[[1, "median"]]
  summary_df[["ivt_saccades", "group_mean"]] <-
    saccade_info[[1, "mean"]]
  summary_df[["ivt_saccades", "group_min"]] <-
    saccade_info[[1, "min"]]
  summary_df[["ivt_saccades", "group_max"]] <-
    saccade_info[[1, "max"]]
  summary_df[["ivt_saccades", "group_sd"]] <-
    saccade_info[[1, "sd"]]

  summary_df[["ivt_blinks", "n"]] <-
    summarize_row_data$total_rows_blink[1]
  summary_df[["ivt_blinks", "percent"]] <-
    round(
      summarize_row_data$total_rows_blink[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["ivt_blinks", "percent_numerator"]] <-
    summarize_row_data$total_rows_blink[1]
  summary_df[["ivt_blinks", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]
  summary_df[["ivt_blinks", "group_n"]] <-
    blink_info[[1, "count"]]
  summary_df[["ivt_blinks", "group_median"]] <-
    blink_info[[1, "median"]]
  summary_df[["ivt_blinks", "group_mean"]] <-
    blink_info[[1, "mean"]]
  summary_df[["ivt_blinks", "group_min"]] <-
    blink_info[[1, "min"]]
  summary_df[["ivt_blinks", "group_max"]] <-
    blink_info[[1, "max"]]
  summary_df[["ivt_blinks", "group_sd"]] <-
    blink_info[[1, "sd"]]

  summary_df[["ivt_missing", "n"]] <-
    summarize_row_data$total_rows_missing[1]
  summary_df[["ivt_missing", "percent"]] <-
    round(
      summarize_row_data$total_rows_missing[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["ivt_missing", "percent_numerator"]] <-
    summarize_row_data$total_rows_missing[1]
  summary_df[["ivt_missing", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]
  summary_df[["ivt_missing", "group_n"]] <-
    missing_info[[1, "count"]]
  summary_df[["ivt_missing", "group_median"]] <-
    missing_info[[1, "median"]]
  summary_df[["ivt_missing", "group_mean"]] <-
    missing_info[[1, "mean"]]
  summary_df[["ivt_missing", "group_min"]] <-
    missing_info[[1, "min"]]
  summary_df[["ivt_missing", "group_max"]] <-
    missing_info[[1, "max"]]
  summary_df[["ivt_missing", "group_sd"]] <-
    missing_info[[1, "sd"]]

  summary_df[["ivt_unclassified", "n"]] <-
    summarize_row_data$total_rows_unclassified[1]
  summary_df[["ivt_unclassified", "percent"]] <-
    round(
      summarize_row_data$total_rows_unclassified[1] / summarize_row_data$total_rows[1],
      4
    )
  summary_df[["ivt_unclassified", "percent_numerator"]] <-
    summarize_row_data$total_rows_unclassified[1]
  summary_df[["ivt_unclassified", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]
  summary_df[["ivt_unclassified", "group_n"]] <-
    unclassified_info[[1, "count"]]
  summary_df[["ivt_unclassified", "group_median"]] <-
    unclassified_info[[1, "median"]]
  summary_df[["ivt_unclassified", "group_mean"]] <-
    unclassified_info[[1, "mean"]]
  summary_df[["ivt_unclassified", "group_min"]] <-
    unclassified_info[[1, "min"]]
  summary_df[["ivt_unclassified", "group_max"]] <-
    unclassified_info[[1, "max"]]
  summary_df[["ivt_unclassified", "group_sd"]] <-
    unclassified_info[[1, "sd"]]

  summary_df[["robustness_proportion_valid_data_to_all_data", "n"]] <-
    (summary_df[["ivt_fixations", "percent_numerator"]] + summary_df[["ivt_saccades", "percent_numerator"]] + summary_df[["ivt_unclassified", "percent_numerator"]])
  summary_df[["robustness_proportion_valid_data_to_all_data", "percent"]] <-
    (summary_df[["ivt_fixations", "percent_numerator"]] + summary_df[["ivt_saccades", "percent_numerator"]] + summary_df[["ivt_unclassified", "percent_numerator"]]) / summary_df[["valid_raw_data", "percent_denominator"]]
  summary_df[["robustness_proportion_valid_data_to_all_data", "percent_numerator"]] <-
    (summary_df[["ivt_fixations", "percent_numerator"]] + summary_df[["ivt_saccades", "percent_numerator"]] + summary_df[["ivt_unclassified", "percent_numerator"]])
  summary_df[["robustness_proportion_valid_data_to_all_data", "percent_denominator"]] <-
    summary_df[["valid_raw_data", "percent_denominator"]]
  summary_df[["robustness_proportion_blink_data_to_missing_data", "n"]] <-
    (summary_df[["ivt_blinks", "percent_numerator"]])
  # summary_df[["robustness_proportion_blink_data_to_missing_data", "percent"]] <- if((summary_df[["ivt_missing", "percent_numerator"]] + summary_df[["ivt_blinks", "percent_numerator"]]) > 0) {(summary_df[["ivt_blinks", "percent_numerator"]]) / (summary_df[["ivt_missing", "percent_numerator"]] + summary_df[["ivt_blinks", "percent_numerator"]])} else{NA}
  if ((summary_df[["ivt_missing", "percent_numerator"]] + summary_df[["ivt_blinks", "percent_numerator"]]) > 0) {
    summary_df[["robustness_proportion_blink_data_to_missing_data", "percent"]] <-
      (summary_df[["ivt_blinks", "percent_numerator"]]) / (summary_df[["ivt_missing", "percent_numerator"]] + summary_df[["ivt_blinks", "percent_numerator"]])
  } else {
    summary_df[["robustness_proportion_blink_data_to_missing_data", "percent"]] <-
      NA
  }
  summary_df[["robustness_proportion_blink_data_to_missing_data", "percent_numerator"]] <-
    (summary_df[["ivt_blinks", "percent_numerator"]])
  summary_df[["robustness_proportion_blink_data_to_missing_data", "percent_denominator"]] <-
    (summary_df[["ivt_missing", "percent_numerator"]] + summary_df[["ivt_blinks", "percent_numerator"]])
  summary_df[["robustness_fixation_duration", "group_median"]] <-
    summary_df[["ivt_fixations", "group_median"]]
  summary_df[["robustness_fixation_duration", "group_mean"]] <-
    summary_df[["ivt_fixations", "group_mean"]]
  summary_df[["robustness_fixation_duration", "group_min"]] <-
    summary_df[["ivt_fixations", "group_min"]]
  summary_df[["robustness_fixation_duration", "group_max"]] <-
    summary_df[["ivt_fixations", "group_max"]]
  summary_df[["robustness_fixation_duration", "group_sd"]] <-
    summary_df[["ivt_fixations", "group_sd"]]

  # Precision metrics (SD + RMS-S2S) populate the dedicated precision_value
  # column -- these are raw visual-angle-degree magnitudes, not ratios, so
  # percent/percent_numerator/percent_denominator are left NA for all 24
  # rows, same as every other column that doesn't apply to a given row.
  precision_variants <- list(
    wholeFile = precision_wholeFile,
    longestFixation = precision_longestFixation,
    medianFixation = precision_medianFixation
  )
  for (variant_name in names(precision_variants)) {
    variant_values <- precision_variants[[variant_name]]
    for (metric_name in names(variant_values)) {
      row_name <- paste0("precision_", metric_name, "_", variant_name)
      summary_df[[row_name, "precision_value"]] <- variant_values[[metric_name]]
    }
  }

  print(summary_df)


  return(summary_df)
}
