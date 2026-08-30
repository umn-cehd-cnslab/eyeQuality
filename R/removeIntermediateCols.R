#' Remove columns calculated during intermediate steps of run_preprocess
#'
#' @param data dataframe containing preprocessed data
#' @param ... additional passed parameters from parent function
#'
#' @return data
#'
removeIntermediateCols <- function(data,
                                   ...) {
  # remove columns created in preprocessing pipeline that are not the final outputs / categories
  data <- data[c(
    1:(which(colnames(data) == "gazeLeft.offscreen") - 1), which(colnames(data) == "blink.classification"),
    which(colnames(data) == "offscreen.classification"),
    (which(colnames(data) == "velocityEuclidean.smooth_va_ms") + 1):length(colnames(data))
  )]

  return(data)
}
