#' Save Files
#'
#' @param inputFile string filepath of the input file, to generate save file names
#' @param data dataframe
#' @param events dataframe of event markers, only tobii pro files
#' @param timing list of internal run timing from eyeQuality function
#' @param summaryData data from the calculateOutputMetrics function
#' @param batchName batch name to insert into save files, useful for running batches with different parameters and/or for specific trials
#' @param outputDir optional directory to write output files to, overriding the default `<input_dir>/derivatives/eyeQuality-v1/` location. Default NULL
#' @param outputStructure one of `"flat"` (default) or `"bids"` (P7-07).
#'   `"flat"` reproduces this function's original behavior exactly: every
#'   output file is written directly into `outputDir` (or the default
#'   `<input_dir>/derivatives/eyeQuality-v1/` location when `outputDir` is
#'   `NULL`), keyed only by filename. `"bids"` is only meaningful when
#'   `outputDir` is also set -- see `create_new_filename()`'s own
#'   `outputStructure` documentation for the derivatives-directory layout it
#'   produces and its fallback behavior for a non-BIDS-named `inputFile`.
#' @import dplyr
#' @import tidyr
#' @importFrom utils write.table
#' @export

# saveFiles <- function(inputFile, args, data, events, timing, summaryData, batchName = NULL) {
saveFiles <- function(inputFile, data, events, timing, summaryData, batchName = NULL, outputDir = NULL, outputStructure = c("flat", "bids")) {
  outputStructure <- match.arg(outputStructure)

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
    file = windows_long_path(create_new_filename(inputFile, eventdesc, ".tsv", outputDir = outputDir, outputStructure = outputStructure)),
    row.names = FALSE,
    sep = "\t"
  )
  # save raw data
  write.table(
    data.frame("raw_data_row" = rownames(data), data),
    file = windows_long_path(create_new_filename(inputFile, preprocdesc, ".tsv", outputDir = outputDir, outputStructure = outputStructure)),
    row.names = FALSE,
    sep = "\t"
  )
  # save timing data
  write.table(
    timing,
    file = windows_long_path(create_new_filename(inputFile, runtimesdesc, ".tsv", outputDir = outputDir, outputStructure = outputStructure)),
    row.names = FALSE,
    sep = "\t"
  )
  # save output summary data
  write.table(
    data.frame("qc_metric" = rownames(summaryData), summaryData),
    file = windows_long_path(create_new_filename(inputFile, qcsummarydesc, ".tsv", outputDir = outputDir, outputStructure = outputStructure)),
    row.names = FALSE,
    sep = "\t"
  )
  print("--- FILES SAVED ---")
}

# .parse_bids_sub_ses (internal): extract "sub-<label>"/"ses-<label>"
# identifiers directly from a file's own basename (P7-07) -- deliberately NOT
# derived from any directory structure inputfile happens to sit in, so
# outputStructure = "bids" placement works identically whether the file was
# discovered via listBidsFiles(layout = "bids")'s subject/session
# subdirectories or layout = "glob"'s flatter matching, and works the same
# even called directly (not via listBidsFiles() at all). Matches each
# identifier's first occurrence anywhere in the basename via
# "sub-[A-Za-z0-9]+"/"ses-[A-Za-z0-9]+" -- e.g.
# "sub-01_ses-02_task-x_recording-eyetracking_physio.tsv" yields
# sub = "sub-01", ses = "ses-02". Shared by create_new_filename() (the
# derivatives-output side of the bids/flat split),
# get_qcsummary_output_path() (R/eyeQualityBatch.R's resumability check, kept
# in sync with create_new_filename()'s own placement logic), and
# eyeQualityBatch()'s copyRawFile raw-file-copy step (the raw side of the
# split), so all three agree on exactly which files count as BIDS-named.
#
# @param path a file path (or bare filename); basename is extracted
#   internally via .safe_basename() rather than base::basename(), for the
#   same long-path-safety reason used throughout this file.
# @return a list(sub = <character>, ses = <character>) if both identifiers
#   are present in the basename, or NULL if either is missing.
# @keywords internal
# @noRd
.parse_bids_sub_ses <- function(path) {
  base <- .safe_basename(path)
  sub_match <- regmatches(base, regexpr("sub-[A-Za-z0-9]+", base))
  ses_match <- regmatches(base, regexpr("ses-[A-Za-z0-9]+", base))
  if (length(sub_match) == 0 || length(ses_match) == 0) {
    return(NULL)
  }
  list(sub = sub_match, ses = ses_match)
}

#' create_new_filename: create new output filename from save directory and data file
#'
#' @param inputfile name of input file
#' @param appendname text to append before the file extension
#' @param newFileExtension new file extension to use, if different from filename
#' @param outputDir optional directory to write the output file to, overriding the default `<input_dir>/derivatives/eyeQuality-v1/` location. Default NULL
#' @param outputStructure one of `"flat"` (default) or `"bids"` (P7-07),
#'   opt-in and only meaningful when `outputDir` is also set. `"flat"` (the
#'   default) reproduces this function's pre-P7-07 behavior exactly: every
#'   file is written directly under `outputDir` (or the default
#'   `<input_dir>/derivatives/eyeQuality-v1/` location when `outputDir` is
#'   `NULL`), keyed only by filename -- no sub-/ses- structure. `"bids"`
#'   parses `sub-<label>`/`ses-<label>` identifiers directly out of
#'   `inputfile`'s own basename (via
#'   `"sub-[A-Za-z0-9]+"`/`"ses-[A-Za-z0-9]+"`, not from any directory
#'   structure `inputfile` happens to sit in -- this works the same way
#'   whether the file was discovered via `listBidsFiles(layout = "bids")` or
#'   `layout = "glob"`) and writes to
#'   `<outputDir>/derivatives/eyeQuality-v1/sub-<label>/ses-<label>/`
#'   instead, mirroring the formal BIDS derivatives convention (the raw side
#'   of that same split, a copy of the original input file under
#'   `<outputDir>/sub-<label>/ses-<label>/`, is handled separately by
#'   `eyeQualityBatch()`'s `copyRawFile` argument -- this function only ever
#'   writes derivative output). A file whose basename does not match both
#'   identifiers falls back to this function's `"flat"` placement (with a
#'   `warning()`, not an error) rather than failing outright -- this keeps a
#'   mixed-naming batch from aborting entirely over one oddly-named file.
#'
#' @import fs
#'
#' @export

create_new_filename <- function(inputfile, appendname, newFileExtension = NULL, outputDir = NULL, outputStructure = c("flat", "bids")) {
  outputStructure <- match.arg(outputStructure)
  # trimws(): a stray leading/trailing space in inputfile/outputDir (e.g. a
  # copy-pasted path, or a directoryBIDS value loaded from a hand-edited
  # batch_config.yaml) propagates straight through fs::path_dir()/fs::path()
  # below into fs::dir_create(newdirectory) a few lines down, which then
  # rejects the space-corrupted path with a confusing "[EINVAL] Failed to
  # make directory ' C:'"-style error that gives no hint the actual problem
  # is whitespace -- confirmed by direct reproduction, not theoretical. The
  # Shiny apps already trim at their own directory-selection reactive (the
  # more likely entry point in practice), but this function is also called
  # directly (not just via the apps), so it defends itself here too rather
  # than relying only on callers to have already done so.
  inputfile <- trimws(inputfile)
  if (!is.null(outputDir)) {
    outputDir <- trimws(outputDir)
  }
  # Remove file extension (assuming the last occurrence of "." denotes the extension)
  # .safe_basename() (see R/windowsLongPath.R): base R's own basename() has
  # its own ~300-character path limit on Windows, distinct from (and not
  # fixed by) windows_long_path()/windows_safe_read_*() -- a real, confirmed
  # failure point for a long inputfile path.
  filename <- .safe_basename(fs::path_ext_remove(inputfile))
  directory <- fs::path_dir(inputfile)

  # P7-07: outputStructure = "bids" only changes anything when outputDir is
  # also set -- with no outputDir, the default <input_dir>/derivatives/
  # eyeQuality-v1/ location already preserves each file's own directory, so
  # there is no "flattening" for "bids" mode to undo. bids_ids stays NULL
  # (falling through to the same flat placement as outputStructure = "flat")
  # whenever outputDir is NULL, outputStructure is "flat", or inputfile's
  # basename doesn't carry both a sub-/ses- identifier -- see
  # .parse_bids_sub_ses()'s own header comment.
  bids_ids <- NULL
  if (identical(outputStructure, "bids") && !is.null(outputDir)) {
    bids_ids <- .parse_bids_sub_ses(inputfile)
    if (is.null(bids_ids)) {
      warning(
        "create_new_filename: '", .safe_basename(inputfile), "' does not match the expected ",
        "sub-<label>...ses-<label> naming pattern required for outputStructure = \"bids\"; ",
        "falling back to flat output placement for this file.",
        call. = FALSE
      )
    }
  }

  newdirectory <- if (is.null(outputDir)) {
    fs::path(directory, "derivatives", "eyeQuality-v1")
  } else if (!is.null(bids_ids)) {
    fs::path(outputDir, "derivatives", "eyeQuality-v1", bids_ids$sub, bids_ids$ses)
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
