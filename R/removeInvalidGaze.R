#' removeInvalidGaze
#' function ran from rmInvalidGP which marks invalid points as NA
#'
#' @param data your dataset (processed by formatCols() function)
#' @param whichEye "left" or "right"
#' @param software either "TobiiStudio" or "TobiiPro"
#' @param threshold ONLY for Tobii Studio datasets. A numeric, indicating acceptable threshold for valid data.
#'
#' @return data with new `.valid`-suffixed columns holding invalid points marked as NA; raw columns are left untouched
#' @export
#'
removeInvalidGaze <- function(data, whichEye, software, threshold = 2) {
  #extract relevant gaze point columns
  cols <-
    colnames(data)[grepl(whichEye, colnames(data), ignore.case = TRUE) &
                     !grepl("valid", colnames(data))]
  #define validity column
  validityCol <-
    colnames(data)[grepl(whichEye, colnames(data), ignore.case = TRUE) &
                     grepl("valid", colnames(data))]
  #create new .valid columns and replace gazepoint data with NA for invalid gazepoints, leaving raw columns untouched
  if (str_detect(software, "TobiiPro")) {
    for (i in cols) {
      newCol <- paste0(i, ".valid")
      replaceRows <- data[, validityCol] == "Invalid"
      data[[newCol]] <- data[[i]]
      data[[newCol]][tidyr::replace_na(replaceRows, FALSE)] <- NA
    }
  }
  else if (str_detect(software, "TobiiStudio")) {
    validityVals <- data[[validityCol]]
    for (i in cols) {
      newCol <- paste0(i, ".valid")
      data[[newCol]] <- data[[i]]
      data[[newCol]][validityVals > threshold] <- NA
      #-9999 used to mark missing values, changed to NA
      data[[newCol]][data[[newCol]] == -9999] <- NA
    }
  }
  else {
    stop("removeInvalidGaze: unrecognized software value '", software, "'")
  }
  return(data)
}
