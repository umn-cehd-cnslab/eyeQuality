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
  # .safe_basename() (R/windowsLongPath.R): base R's own basename() has its
  # own ~300-character path limit on Windows, confirmed independently of
  # windows_long_path()'s own MAX_PATH workaround -- a real failure point
  # for a long filename, not the "CHECK" this line used to be marked with.
  # NOTE this fixes only the basename-extraction step: path_dir()/path()
  # below (fs::path_dir()/fs::path(), used to assemble the rest of this
  # function's return value) have their OWN separate ~260-character internal
  # length rejection, unrelated to and not closed by this fix -- see
  # create_new_filename()'s matching comment in R/saveFiles.R for the full
  # explanation. A sufficiently long filename can still make this whole
  # function error, just past a slightly later point than before.
  base <-
    .safe_basename(path_ext_remove(filename))
  directory <- path_dir(filename)
  log <-
    paste0(
      base,
      "_desc-",
      if (is.null(batchName)) "" else paste0(batchName, "_"),
      "preproc_runlog2",
      ".txt"
    )
  logpath <- path(directory, "derivatives", "eyeQuality-v1", log)
  return(logpath)
}
