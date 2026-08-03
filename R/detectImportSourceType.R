#' Detect Import Source Type
#'
#' Detects if imported data comes from TobiiPro, TobbiStudio, or another Eye Tracking export type
#'
#' @param data dataframe
#'
#' @return A string
#' @export
#'
#'
detectImportSourceType <- function(data) {
  colNames <- names(data)
  if ("StudioVersionRec" %in% colNames) {
    sourceType <- "TobiiStudio"
  } else if ("Recording software version" %in% colNames) {
    sourceType <- "TobiiPro"
  } else {
    stop("Data import does not match column names expected from Tobii Studio or Tobii Pro")
    # Currently support "TobiiStudio" or "TobiiPro", as listed in function `rmInvalid2.R`
  }

  return(sourceType)
}
