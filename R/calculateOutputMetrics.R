#' Calculate Output Metrics
#'
#' @param data dataframe
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom stats median
#' @return summary_df dataframe describing each
#' @export
#'
calculateOutputMetrics <- function(data) {
  summarize_row_data <- data %>%
    dplyr::group_by() %>%
    dplyr::summarise(
      #denominators (number of rows)
      total_rows = dplyr::n(),
      total_final_rows = sum(ifelse(
        .data$gazeY.es.selection != "both_na", 1, 0
      )),
      #total rows of missing data, raw (validity-masked)
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
      #total number of blinks
      total_rows_blinks_Left = sum(ifelse(.data$pupilLeft.blink > 0, 1, 0)),
      total_rows_blinks_Right = sum(ifelse(.data$pupilRight.blink > 0, 1, 0)),
      total_rows_blinks_Both = sum(ifelse(.data$bothEyes.blink, 1, 0)),
      #COMMENTED out since there is no data where X and Y points exist but pupil data doesn't exist.
      # total_rows_PupilData_no_gaze_X = sum(ifelse(is.na(gazeLeftX) & !is.na(pupilLeft), 1, 0)),
      # total_rows_PupilData_no_gaze_Y = sum(ifelse(is.na(gazeRightX) & !is.na(pupilRight), 1, 0)),
      # total_rows_no_PupilData_gaze_X = sum(ifelse(!is.na(gazeLeftX) & is.na(pupilLeft), 1, 0)),
      # total_rows_no_PupilData_gaze_Y = sum(ifelse(!is.na(gazeRightX) & is.na(pupilRight), 1, 0)),
      #interpolated gaps filled
      total_rows_interpolated_LeftX = sum(
        ifelse(
          .data$gazeLeftX.valid %>% replace(is.na(.data), 0) != .data$gazeLeftX.int %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_RightX = sum(
        ifelse(
          .data$gazeRightX.valid %>% replace(is.na(.data), 0) != .data$gazeRightX.int %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_LeftY = sum(
        ifelse(
          .data$gazeLeftY.valid %>% replace(is.na(.data), 0) != .data$gazeLeftY.int %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_RightY = sum(
        ifelse(
          .data$gazeRightY.valid %>% replace(is.na(.data), 0) != .data$gazeRightY.int %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_LeftPupil = sum(
        ifelse(
          .data$pupilLeft.valid %>% replace(is.na(.data), 0) != .data$pupilLeft.int %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_RightPupil = sum(
        ifelse(
          .data$pupilRight.valid %>% replace(is.na(.data), 0) != .data$pupilRight.int %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_LeftDistZ = sum(
        ifelse(
          .data$distanceLeftZ.valid %>% replace(is.na(.data), 0) != .data$distanceLeftZ.int %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_interpolated_RightDistZ = sum(
        ifelse(
          .data$distanceRightZ.valid %>% replace(is.na(.data), 0) != .data$distanceRightZ.int %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      # #COMMENTED OUT LH - created variables are not used and .temp columns break in other eyeselect protocols
      #eye select
      # total_rows_eye_select_LeftX = sum(
      #   ifelse(
      #     .data$gazeLeftX.int %>% replace(is.na(.data), 0) != .data$gpLeft.X.temp %>% replace(is.na(.data), 0),
      #     1,
      #     0
      #   )
      # ),
      # total_rows_eye_select_RightX = sum(
      #   ifelse(
      #     .data$gazeRightX.int %>% replace(is.na(.data), 0) != .data$gpRight.X.temp %>% replace(is.na(.data), 0),
      #     1,
      #     0
      #   )
      # ),
      # total_rows_eye_select_LeftY = sum(
      #   ifelse(
      #     .data$gazeLeftY.int %>% replace(is.na(.data), 0) != .data$gpLeft.Y.temp %>% replace(is.na(.data), 0),
      #     1,
      #     0
      #   )
      # ),
      # total_rows_eye_select_RightY = sum(
      #   ifelse(
      #     .data$gazeRightY.int %>% replace(is.na(.data), 0) != .data$gpRight.Y.temp %>% replace(is.na(.data), 0),
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
      min_eye_select_X = min(.data$gazeX.eyeSelect, na.rm = TRUE),
      min_eye_select_Y = min(.data$gazeY.eyeSelect, na.rm = TRUE),
      min_eye_select_pupil = min(.data$pupil.eyeSelect, na.rm = TRUE),
      min_eye_select_distZ = min(.data$distanceZ.eyeSelect, na.rm = TRUE),
      max_eye_select_X = max(.data$gazeX.eyeSelect, na.rm = TRUE),
      max_eye_select_Y = max(.data$gazeY.eyeSelect, na.rm = TRUE),
      max_eye_select_pupil = max(.data$pupil.eyeSelect, na.rm = TRUE),
      max_eye_select_distZ = max(.data$distanceZ.eyeSelect, na.rm = TRUE),
      sd_eye_select_X = sd(.data$gazeX.eyeSelect, na.rm = TRUE),
      sd_eye_select_Y = sd(.data$gazeY.eyeSelect, na.rm = TRUE),
      sd_eye_select_pupil = sd(.data$pupil.eyeSelect, na.rm = TRUE),
      sd_eye_select_distZ = sd(.data$distanceZ.eyeSelect, na.rm = TRUE),

      #smoothing
      total_rows_denoise_X = sum(
        ifelse(
          .data$gazeX.eyeSelect %>% replace(is.na(.data), 0) != .data$gazeX.smooth %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_denoise_Y = sum(
        ifelse(
          .data$gazeY.eyeSelect %>% replace(is.na(.data), 0) != .data$gazeY.smooth %>% replace(is.na(.data), 0),
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
          .data$pupil.eyeSelect %>% replace(is.na(.data), 0) != .data$pupil.smooth %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_denoise_distZ = sum(
        ifelse(
          .data$distanceZ.eyeSelect %>% replace(is.na(.data), 0) != .data$distanceZ.smooth %>% replace(is.na(.data), 0),
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
      min_denoise_X = min(.data$gazeX.smooth, na.rm = TRUE),
      min_denoise_Y = min(.data$gazeY.smooth, na.rm = TRUE),
      min_denoise_pupil = min(.data$pupil.smooth, na.rm = TRUE),
      min_denoise_distZ = min(.data$distanceZ.smooth, na.rm = TRUE),
      max_denoise_X = max(.data$gazeX.smooth, na.rm = TRUE),
      max_denoise_Y = max(.data$gazeY.smooth, na.rm = TRUE),
      max_denoise_pupil = max(.data$pupil.smooth, na.rm = TRUE),
      max_denoise_distZ = max(.data$distanceZ.smooth, na.rm = TRUE),
      sd_denoise_X = sd(.data$gazeX.smooth, na.rm = TRUE),
      sd_denoise_Y = sd(.data$gazeY.smooth, na.rm = TRUE),
      sd_denoise_pupil = sd(.data$pupil.smooth, na.rm = TRUE),
      sd_denoise_distZ = sd(.data$distanceZ.smooth, na.rm = TRUE),

      #velocity smoothing
      total_rows_smoothVA_X = sum(
        ifelse(
          .data$velocityX_va_ms %>% replace(is.na(.data), 0) != .data$velocityX.smooth_va_ms %>% replace(is.na(.data), 0),
          1,
          0
        )
      ),
      total_rows_smoothVA_Y = sum(
        ifelse(
          .data$velocityY_va_ms %>% replace(is.na(.data), 0) != .data$velocityY.smooth_va_ms %>% replace(is.na(.data), 0),
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
      min_smoothVA_X = min(.data$velocityX.smooth_va_ms, na.rm = TRUE),
      min_smoothVA_Y = min(.data$velocityY.smooth_va_ms, na.rm = TRUE),
      max_smoothVA_X = max(.data$velocityX.smooth_va_ms, na.rm = TRUE),
      max_smoothVA_Y = max(.data$velocityY.smooth_va_ms, na.rm = TRUE),
      sd_smoothVA_X = sd(.data$velocityX.smooth_va_ms, na.rm = TRUE),
      sd_smoothVA_Y = sd(.data$velocityY.smooth_va_ms, na.rm = TRUE),

      #gaze points within fixation/saccade
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
      min = min(.data$duration, na.rm = TRUE),
      max = max(.data$duration, na.rm = TRUE),
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
      min = min(.data$duration, na.rm = TRUE),
      max = max(.data$duration, na.rm = TRUE),
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
      min = min(.data$duration, na.rm = TRUE),
      max = max(.data$duration, na.rm = TRUE),
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
      min = min(.data$duration, na.rm = TRUE),
      max = max(.data$duration, na.rm = TRUE),
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
      min = min(.data$duration, na.rm = TRUE),
      max = max(.data$duration, na.rm = TRUE),
      sd = sd(.data$duration, na.rm = TRUE)
    )

  #setup output dataframe
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
    "ivt_missing"
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
    "group_sd"
  )
  summary_df <-
    data.frame(matrix(
      nrow = length(summary_df_rows),
      ncol = length(summary_df_columns)
    ))
  rownames(summary_df) <- summary_df_rows
  colnames(summary_df) <- summary_df_columns

  #save data to output dataframe
  summary_df[["missing_raw_data_LeftEye", "n"]] <-
    summarize_row_data$total_rows_raw_na_Left[1]
  summary_df[["missing_raw_data_LeftEye", "percent"]] <-
    round(summarize_row_data$total_rows_raw_na_Left[1] / summarize_row_data$total_rows[1],
          4)
  summary_df[["missing_raw_data_LeftEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_raw_na_Left[1]
  summary_df[["missing_raw_data_LeftEye", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["missing_raw_data_RightEye", "n"]] <-
    summarize_row_data$total_rows_raw_na_Right[1]
  summary_df[["missing_raw_data_RightEye", "percent"]] <-
    round(summarize_row_data$total_rows_raw_na_Right[1] / summarize_row_data$total_rows[1],
          4)
  summary_df[["missing_raw_data_RightEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_raw_na_Right[1]
  summary_df[["missing_raw_data_RightEye", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["missing_raw_data_BothEyes", "n"]] <-
    summarize_row_data$total_rows_raw_na_X[1]
  summary_df[["missing_raw_data_BothEyes", "percent"]] <-
    round(summarize_row_data$total_rows_raw_na_X[1] / summarize_row_data$total_rows[1],
          4)
  summary_df[["missing_raw_data_BothEyes", "percent_numerator"]] <-
    summarize_row_data$total_rows_raw_na_X[1]
  summary_df[["missing_raw_data_BothEyes", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["valid_raw_data", "n"]] <-
    summarize_row_data$total_rows[1] - summarize_row_data$total_rows_raw_na_X[1]
  summary_df[["valid_raw_data", "percent"]] <-
    round((
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
    round(summarize_row_data$total_rows_blinks_Left[1] / summarize_row_data$total_rows[1],
          4)
  summary_df[["blinks_LeftEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_blinks_Left[1]
  summary_df[["blinks_LeftEye", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["blinks_RightEye", "n"]] <-
    summarize_row_data$total_rows_blinks_Right[1]
  summary_df[["blinks_RightEye", "percent"]] <-
    round(summarize_row_data$total_rows_blinks_Right[1] / summarize_row_data$total_rows[1],
          4)
  summary_df[["blinks_RightEye", "percent_numerator"]] <-
    summarize_row_data$total_rows_blinks_Right[1]
  summary_df[["blinks_RightEye", "percent_denominator"]] <-
    summarize_row_data$total_rows[1]

  summary_df[["blinks_BothEyes", "n"]] <-
    summarize_row_data$total_rows_blinks_Both[1]
  summary_df[["blinks_BothEyes", "percent"]] <-
    round(summarize_row_data$total_rows_blinks_Both[1] / summarize_row_data$total_rows[1],
          4)
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
    round(summarize_row_data$total_rows_fixation[1] / summarize_row_data$total_rows[1],
          4)
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
    round(summarize_row_data$total_rows_saccade[1] / summarize_row_data$total_rows[1],
          4)
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
    round(summarize_row_data$total_rows_blink[1] / summarize_row_data$total_rows[1],
          4)
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
    round(summarize_row_data$total_rows_missing[1] / summarize_row_data$total_rows[1],
          4)
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
    round(summarize_row_data$total_rows_unclassified[1] / summarize_row_data$total_rows[1],
          4)
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

  print(summary_df)


  return(summary_df)
}
