#' Save Files
#'
#' @param inputFile string filepath of the input file, to generate save file names
#' @param data dataframe
#' @param events dataframe of event markers, only tobii pro files
#' @param timing list of internal run timing from eyeQuality function
#' @param summaryData data from the calculateOutputMetrics function
#' @param batchName batch name to insert into save files, useful for running batches with different parameters and/or for specific trials
#' @param outputDir optional directory to write output files to, overriding the default `<input_dir>/derivatives/eyeQuality-v1/` location. Default NULL
#' @import dplyr
#' @import tidyr
#' @importFrom utils write.table
#' @export

# saveFiles <- function(inputFile, args, data, events, timing, summaryData, batchName = NULL) {
saveFiles <- function(inputFile, data, events, timing, summaryData, batchName = NULL, outputDir = NULL) {
  eventdesc <- paste0("_desc-", if (is.null(batchName)) "" else paste0(batchName, "_"), "events")
  preprocdesc <- paste0("_desc-", if (is.null(batchName)) "" else paste0(batchName, "_"), "preproc")
  runtimesdesc <- paste0("_desc-", if (is.null(batchName)) "" else paste0(batchName, "_"), "preproc_runtimes")
  qcsummarydesc <- paste0("_desc-", if (is.null(batchName)) "" else paste0(batchName, "_"), "preproc_qcsummary")

  # save event data
  # windows_long_path(): Windows' MAX_PATH (~260 char) limit can make the
  # file() connection write.table() opens fail with "cannot open the
  # connection" even though the target directory exists -- same root cause
  # as importData.R's read side, just as easy to hit here given this
  # package's own nested derivatives/eyeQuality-v1/ output convention plus
  # long BIDS-style "_desc-..._preproc_qcsummary.tsv" filenames.
  write.table(
    data.frame("raw_data_row" = rownames(events), events),
    file = windows_long_path(create_new_filename(inputFile, eventdesc, ".tsv", outputDir = outputDir)),
    row.names = FALSE,
    sep = "\t"
  )
  # save raw data
  write.table(
    data.frame("raw_data_row" = rownames(data), data),
    file = windows_long_path(create_new_filename(inputFile, preprocdesc, ".tsv", outputDir = outputDir)),
    row.names = FALSE,
    sep = "\t"
  )
  # save timing data
  write.table(
    timing,
    file = windows_long_path(create_new_filename(inputFile, runtimesdesc, ".tsv", outputDir = outputDir)),
    row.names = FALSE,
    sep = "\t"
  )
  # save output summary data
  write.table(
    data.frame("qc_metric" = rownames(summaryData), summaryData),
    file = windows_long_path(create_new_filename(inputFile, qcsummarydesc, ".tsv", outputDir = outputDir)),
    row.names = FALSE,
    sep = "\t"
  )
  print("--- FILES SAVED ---")
}

#' create_new_filename: create new output filename from save directory and data file
#'
#' @param inputfile name of input file
#' @param appendname text to append before the file extension
#' @param newFileExtension new file extension to use, if different from filename
#' @param outputDir optional directory to write the output file to, overriding the default `<input_dir>/derivatives/eyeQuality-v1/` location. Default NULL
#'
#' @import fs
#'
#' @export

create_new_filename <- function(inputfile, appendname, newFileExtension = NULL, outputDir = NULL) {
  # Remove file extension (assuming the last occurrence of "." denotes the extension)
  filename <- basename(fs::path_ext_remove(inputfile))
  directory <- fs::path_dir(inputfile)
  newdirectory <- if (is.null(outputDir)) {
    fs::path(directory, "derivatives", "eyeQuality-v1")
  } else {
    fs::path(outputDir)
  }
  file_extension <- fs::path_ext(inputfile)

  # replace with newFileExtension if specified.
  if (!is.null(newFileExtension)) {
    file_extension <- sub("^\\.", "", newFileExtension)
  }

  # create the derivatives directory, if it doesn't exist. Note: this call,
  # and the fs::path() calls just below that assemble the final file path,
  # are deliberately NOT run through windows_long_path() -- verified
  # directly on Windows against real >260-character paths that fs::path(),
  # fs::dir_create(), and base dir.create() all reject any path >= 260
  # characters with their own explicit length check ("Total path length
  # must be less than PATH_MAX: 260" / "'path' too long") *before* the OS
  # is ever consulted, so the "\\?\" extended-length prefix isn't even
  # inspected -- prefixing would only add 4 characters and could push a
  # currently-fine borderline path over that same internal threshold. The
  # windows_long_path() wrapping on the write.table() calls below still
  # protects the actual file write once a valid (< 260 char) path comes
  # back from this function, but for inputs where the *fs::path()-
  # assembled* file_path itself would exceed 260 chars (a long input
  # directory and/or a long BIDS-style "_desc-..._preproc_qcsummary.tsv"
  # filename), create_new_filename() will error out here first, before
  # write.table() is ever reached. Closing that gap would mean rebuilding
  # this function's path assembly on base R (file.path()/dirname()/etc.)
  # instead of fs::path()'s family, which imposes this ceiling on every
  # path it builds or touches, regardless of platform -- a larger change
  # than this fix, left for a follow-up.
  fs::dir_create(newdirectory)

  # Concatenate appendname, filename, and extension (if any)
  newfilename <- fs::path(
    paste0(filename, appendname),
    ext = ifelse(is.null(newFileExtension), file_extension, file_extension)
  )
  file_path <- fs::path(newdirectory, newfilename)

  return(file_path)
}

#' Set sink cmd outputs to save to run log output file
#'
#' @param runlog string of filepath to run log
#' @export

sinkToOutputFile <- function(runlog) {
  # sink(runlog, append = FALSE)
  sink(runlog, append = TRUE)
}


#' Reset sink cmd outputs to default
#'
#' @export

sinkReset <- function() {
  for (i in seq_len(sink.number())) {
    sink()
  }
  # sink(type = "message")
}

print_or_save <- function(expression, savedata, filename = NULL) {
  if (savedata) {
    # windows_long_path(): all three writes below (both cat() calls and the
    # write.table() call) target the same `filename`, so all three need the
    # same protection -- fixing only one still leaves the others able to
    # fail with "cannot open the connection" on a >260-char path (verified
    # directly: the trailing cat("\n", ...) call was still failing this way
    # even after write.table() alone was fixed).
    write_filename <- windows_long_path(filename)
    # if we are saving data, print this to terminal.
    if (typeof(expression) == "character") {
      cat(
        expression,
        file = write_filename,
        append = TRUE
      )
    } else {
      write.table(
        expression,
        file = write_filename,
        append = TRUE,
      )
    }
    cat("\n", file = write_filename, append = TRUE)
  } else {
    # otherwise print to terminal
    print(expression)
  }
}

get_filesizes <- function(filelist) {
  total_size_bytes <- 0
  for (i in filelist) {
    total_size_bytes <- total_size_bytes + file.info(i)$size
  }
  return(total_size_bytes / 1000000) # return in MB
}
