#' Import Data
#'
#' Imports data from `csv`, `tsv`, and `xlsx` files
#'
#' @param filepath path to data file
#' @param ... additional passed parameters from parent function
#'
#' @return A tibble
#' @export
#'
#' @importFrom tools file_ext
#' @importFrom readr read_csv
#' @importFrom readr read_tsv
#' @importFrom readxl read_excel
#'
importData  <- function(filepath, ...) {
  #TODO:add checks for R.util::isFile(filepath)
  fileext <- file_ext(filepath)
  if (fileext == "csv") {
    importedtbl <- read_csv(filepath, ...)
  } else if (fileext == "tsv") {
    importedtbl <- read_tsv(filepath, guess_max = 10000, ...)
  } else if (fileext == "xlsx" || fileext == "xls") {
    importedtbl <- read_excel(filepath, ...)
  } else {
    stop("file must be .csv, .tsv, .xlsx, or .xls")
  }

  return(importedtbl)
}
