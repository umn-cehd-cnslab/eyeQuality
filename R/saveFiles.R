#' Save Files
#'
#' @param inputFile string filepath of the input file, to generate save file names
#' @param data dataframe
#' @param events dataframe of event markers, only tobii pro files
#' @param timing list of internal run timing from eyeQuality function
#' @param summaryData data from the calcualteOutputMetrics function
#' @param batchName batch name to insert into save files, useful for running batches with different parameters and/or for specific trials
#' @param outputDir optional directory to write output files to, overriding the default `<input_dir>/derivatives/eyeQuality-v1/` location. Default NULL
#' @import dplyr
#' @import tidyr
#' @importFrom utils write.table
#' @export

# saveFiles <- function(inputFile, args, data, events, timing, summaryData, batchName = NULL) {
saveFiles <- function(inputFile, data, events, timing, summaryData, batchName = NULL, outputDir = NULL) {
  eventdesc <- paste0("_desc-", ifelse(is.null(batchName), NULL, paste0(batchName, "_")), "events")
  preprocdesc <- paste0("_desc-", ifelse(is.null(batchName), NULL, paste0(batchName, "_")), "preproc")
  runtimesdesc <- paste0("_desc-", ifelse(is.null(batchName), NULL, paste0(batchName, "_")), "preproc_runtimes")
  qcsummarydesc <- paste0("_desc-", ifelse(is.null(batchName), NULL, paste0(batchName, "_")), "preproc_qcsummary")

  # save event data
  write.table(
    data.frame("raw_data_row" = rownames(events), events),
    file = create_new_filename(inputFile, eventdesc, ".tsv", outputDir = outputDir),
    row.names = FALSE,
    sep = "\t"
  )
  # save raw data
  write.table(
    data.frame("raw_data_row" = rownames(data), data),
    file = create_new_filename(inputFile, preprocdesc, ".tsv", outputDir = outputDir),
    row.names = FALSE,
    sep = "\t"
  )
  # save timing data
  write.table(
    timing,
    file = create_new_filename(inputFile, runtimesdesc, ".tsv", outputDir = outputDir),
    row.names = FALSE,
    sep = "\t"
  )
  # save output summary data
  write.table(
    data.frame("qc_metric" = rownames(summaryData), summaryData),
    file = create_new_filename(inputFile, qcsummarydesc, ".tsv", outputDir = outputDir),
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
    if (!grepl("^\\.", newFileExtension)) {
      file_extension <- paste0(".", newFileExtension)
    }
  }

  fs::dir_create(newdirectory) # create the derivatives directory, if it doesn't exist

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
    # if we are saving data, print this to terminal.
    if (typeof(expression) == "character") {
      cat(
        expression,
        file = filename,
        append = TRUE
      )
    } else {
      write.table(
        expression,
        file = filename,
        append = TRUE,
      )
    }
    cat("\n", file = filename, append = TRUE)
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
