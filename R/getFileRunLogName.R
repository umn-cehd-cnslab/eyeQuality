#' get_file_run_log_name
#'
#' @description
#' `getFileRunLogName()` gets file run log name
#'
#' @param filename filename
#' @param batchName text qualifier for the batch run
#'
#' @importFrom fs path_ext_remove
#' @importFrom fs path_dir
#' @importFrom fs path
#'
#' @return list of ET derivatives data files to in BIDS-like directory
#' @export
#'
getFileRunLogName <- function(filename, batchName = NULL) {
  # sink(runlog, append = FALSE, type = "output")
  # base <- basename(path_ext_remove(x))
  base <-
    basename(path_ext_remove(filename)) # CHECK: does this work?
  directory <- path_dir(filename)
  log <-
    paste0(
      basename,
      "_desc-",
      ifelse(is.null(batchName), NULL, paste0(batchName, "_")),
      "preproc_runlog2",
      ".txt"
    )
  logpath <- path(directory, "derivatives", "eyeQuality-v1", log)
  return(logpath)
}
