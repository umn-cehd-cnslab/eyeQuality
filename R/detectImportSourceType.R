#' Detect Import Source Type
#'
#' Detects which registered eye tracker adapter's raw column layout matches
#' `data`, by calling each registered adapter's `detect()` (see
#' `R/adapter-interface.R`) in turn and returning the name of the first match.
#'
#' @param data dataframe
#'
#' @return A string -- the matching adapter's `name` (e.g. `"TobiiStudio"`,
#'   `"TobiiPro"`).
#' @export
#'
#'
detectImportSourceType <- function(data) {
  adapters <- registered_adapters()

  for (adapter in adapters) {
    if (isTRUE(adapter$detect(data))) {
      return(adapter$name)
    }
  }

  stop("Data import does not match column names expected from Tobii Studio or Tobii Pro")
}
