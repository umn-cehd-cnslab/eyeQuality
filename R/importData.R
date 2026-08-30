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
#' @importFrom readxl read_excel
#'
importData <- function(filepath, ...) {
  # TODO:add checks for R.util::isFile(filepath)
  fileext <- file_ext(filepath)
  # windows_safe_read_csv()/windows_safe_read_tsv() (see R/windowsLongPath.R):
  # Windows' MAX_PATH (~260 char) limit can make readr report a real file as
  # "does not exist" even though file.exists()/list.files() (a different
  # Windows API) already confirmed it's real -- easy to hit with
  # deeply-nested Box/OneDrive study trees plus this package's own nested
  # derivatives/eyeQuality-v1/ output convention. read_excel() is left as a
  # direct call: readxl's own check_file() gates on the same file.exists()
  # that fails here, and unlike readr/vroom it has no connection-based
  # workaround (it needs a real, seekable file path for its zip/binary
  # reading, not a stream) -- confirmed on a real >260-char path that
  # read_excel() fails there regardless, a real remaining limitation, not
  # an oversight.
  if (fileext == "csv") {
    importedtbl <- windows_safe_read_csv(filepath, ...)
  } else if (fileext == "tsv") {
    importedtbl <- windows_safe_read_tsv(filepath, guess_max = 10000, ...)
  } else if (fileext == "xlsx" || fileext == "xls") {
    importedtbl <- read_excel(filepath, ...)
  } else {
    stop("file must be .csv, .tsv, .xlsx, or .xls")
  }

  return(importedtbl)
}
