#' batch_preprocess is used to run the preprocessing function for a set of files in a BIDS like directory.
#' This will process files in parallel, and compute total runtime to process.
#'
#' @param directoryBIDS filepath to the BIDS directory of ET data to process
#' @param batchName string label for what to call the batch. This will be added to the output files
#' @param numberCores optional parameter to specify number of cores to use. If not specified, function will use 80\\% of available cores
#' @param displayDimensionX_mm integer of display width in millimeters, passed through to `eyeQuality()` for every file in the batch. For example our 1920x1080 screen has a width of 594 mm
#' @param displayDimensionY_mm integer of display height in millimeters, passed through to `eyeQuality()` for every file in the batch. For example our 1920x1080 screen has a height of 344 mm
#' @param verbose optional Boolean, passed through to `eyeQuality()` for
#'   every file in the batch. When `TRUE`, emits additional structured,
#'   human-readable data-quality diagnostics during processing for each
#'   file. Purely additive; does not change any file's processed output.
#'   Default `FALSE`.
#' @param outputDir optional directory to write output files to, passed
#'   through to `eyeQuality()`/`saveFiles()` for every file in the batch and
#'   also used to locate each file's existing `qcsummary` output (if any) for
#'   the resumability check described under `force` below. Overrides the
#'   default `<input_dir>/derivatives/eyeQuality-v1/` location. Default `NULL`.
#' @param force optional Boolean controlling resumability. `eyeQualityBatch()`
#'   checks, for every candidate file, whether a `qcsummary` output already
#'   exists for the given `batchName` (and `outputDir`, if set) before
#'   dispatching it for (re)processing. When `FALSE` (the default), files with
#'   an existing `qcsummary` output are skipped rather than reprocessed -
#'   useful for resuming an interrupted or partially-completed batch run
#'   without redoing already-finished work. When `TRUE`, every candidate file
#'   is (re)processed regardless of any existing output, matching this
#'   function's original (pre-resumability) behavior. Default `FALSE`.
#' @param ... additional parameters
#'
#' @section Failure detail: any file whose `eyeQuality()` call raises an
#'   error is recorded, along with that error's message, in the batch summary
#'   text file's "Files that failed processing" section (one filepath line
#'   followed by an indented `"  error: ..."` line per failed file); this
#'   detail is what `parsePreprocessingBatchSummary(info_to_extract =
#'   "failedfiles")` returns. A file with no captured error message reached
#'   that section some other way this run (e.g. its worker process ended
#'   without raising a catchable R error) and gets a placeholder message
#'   instead of a blank one.
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_glue
#' @import parallel
#'
#' @return data
#' @export
#'
eyeQualityBatch <-
  function(directoryBIDS,
           batchName,
           numberCores = NULL,
           displayDimensionX_mm = 594,
           displayDimensionY_mm = 344,
           verbose = FALSE,
           outputDir = NULL,
           force = FALSE,
           ...) {
    # options(error=traceback)

    if (is.null(batchName) ||
      !is.character(batchName) ||
      length(batchName) != 1 ||
      is.na(batchName) ||
      nchar(batchName) == 0) {
      stop("eyeQualityBatch: 'batchName' must be a non-empty character string")
    }

    if (!is.null(numberCores) &&
      (!is.numeric(numberCores) ||
        length(numberCores) != 1 ||
        is.na(numberCores) ||
        numberCores %% 1 != 0 ||
        numberCores < 1)) {
      stop("eyeQualityBatch: 'numberCores' must be a positive integer when supplied")
    }

    batch_run_summary <-
      paste0(
        directoryBIDS,
        paste0("/preprocessing_batch_summary_desc-", batchName, ".txt")
      )
    # batch_run_out <- paste0(directoryBIDS, paste0("/preprocessing_batch_output_desc-", batchName, ".txt"))
    batch_run_debug <-
      paste0(
        directoryBIDS,
        paste0("/preprocessing_batch_debug_desc-", batchName, ".txt")
      )

    # save outputs to file at root of directoryBIDS
    # sinkToOutputFile(batch_run_summary)

    starttime <- getCurrentTime()

    tsv_files_to_batch_process <-
      listBidsFiles(directoryBIDS, ...)

    print_or_save(
      stringr::str_glue("-----------------"),
      TRUE,
      batch_run_summary
    )

    # print(paste0("starting batch run: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    print_or_save(
      paste0("starting batch run: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      TRUE,
      batch_run_summary
    )

    # print(tsv_files_to_batch_process)
    print_or_save(
      paste(tsv_files_to_batch_process, collapse = "\n"),
      TRUE,
      batch_run_summary
    )

    # P7-03: resumability. Before dispatching any work to the cluster, check
    # each candidate file for an existing qcsummary output matching this
    # batchName (and outputDir, if set) - see get_qcsummary_output_path()
    # below. force = TRUE reproduces the original (pre-resumability)
    # behavior of unconditionally reprocessing every candidate file; this
    # check is intentionally done here, before any worker is spun up, rather
    # than inside the parLapply() worker function, so a fully-resumed batch
    # with nothing left to do never pays the cost of creating a cluster at
    # all (see the length(files_to_process) == 0 branch below).
    if (isTRUE(force)) {
      files_to_skip <- character(0)
      files_to_process <- tsv_files_to_batch_process
    } else {
      already_done_mask <- if (length(tsv_files_to_batch_process) == 0) {
        logical(0)
      } else {
        file.exists(vapply(
          tsv_files_to_batch_process,
          get_qcsummary_output_path,
          character(1),
          batchName = batchName,
          outputDir = outputDir
        ))
      }
      files_to_skip <- tsv_files_to_batch_process[already_done_mask]
      files_to_process <- tsv_files_to_batch_process[!already_done_mask]
    }

    print_or_save(
      stringr::str_glue(
        "------ Skipped files (already processed, resumability; n = {length(files_to_skip)}):  "
      ),
      TRUE,
      batch_run_summary
    )
    print_or_save(
      paste(files_to_skip, collapse = "\n"),
      TRUE,
      batch_run_summary
    )

    # P7-04: named list of per-file failure detail (error message string, or
    # NULL for a file that completed without raising a catchable error),
    # keyed by input filepath. Populated below by parLapply()'s return value
    # when there's anything to dispatch; stays an empty list for a
    # fully-resumed batch (files_to_process is empty, no cluster is created)
    # so it's always defined by the time the "Files that failed processing"
    # section further down looks it up, regardless of which branch ran.
    processing_results <- list()

    if (length(files_to_process) == 0) {
      # Nothing left to do - every candidate file already has a matching
      # qcsummary output. Skip cluster creation entirely (makeCluster(0)
      # would error) rather than spinning up a no-op worker pool.
      print_or_save(
        stringr::str_glue("number of cores = 0"),
        TRUE,
        batch_run_summary
      )
      print_or_save(
        stringr::str_glue(
          "--- all {length(tsv_files_to_batch_process)} candidate file(s) already processed for batchName '{batchName}' (resumability: nothing to dispatch this run). Use force = TRUE to reprocess."
        ),
        TRUE,
        batch_run_summary
      )
    } else {
      # Create a cluster with multiple cores for parallel processing
      if (is.null(numberCores)) {
        # if numberCores not specified, use the lesser to of
        # (a) 85% of available cores or
        # (b) number of files you need to process
        numcores <-
          ifelse(
            # check if there are fewer files the possible number of cores
            floor(detectCores() * 0.85) < length(files_to_process),
            ifelse(floor(detectCores() * 0.85) <= 0, # if 85% of cores <= 0, use 1 core
              1,
              floor(detectCores() * 0.85)
            ),
            # otherwise use 85% of available cores),
            length(files_to_process) #
          )
      } else {
        # otherwise use specified number of cores
        numcores <- ifelse(
          numberCores < length(files_to_process),
          ifelse(numberCores <= 0, 1, numberCores),
          length(files_to_process)
        )
      }
      # print(stringr::str_glue("number of cores = {numcores}"))
      print_or_save(
        stringr::str_glue("number of cores = {numcores}"),
        TRUE,
        batch_run_summary
      )

      if (.Platform$OS.type == "windows") {
        # cl <- parallel::makeCluster(numcores, outfile = batch_run_debug)
        cl <- parallel::makeCluster(numcores, outfile = "")
      } else {
        # should be "unix" on Linux or Mac
        cl <-
          parallel::makeCluster(numcores, outfile = "", type = "FORK")
      }

      # Parallelize the processing of TSV files
      parallel::clusterExport(cl, "eyeQuality") # Export the eyeQuality function to the cluster
      # P7-04: each worker's tryCatch now returns a value (rather than only
      # printing to the console/log as before) so the actual error detail
      # survives the trip back to the master process. parLapply() collects
      # every worker's return value into a list gathered on the master
      # (serialized back over the socket connection for a PSOCK cluster on
      # Windows; returned directly from the forked child for a FORK cluster
      # on Unix) regardless of cluster type, so this is where cross-worker
      # error propagation actually happens - a file's error handler running
      # inside a worker process has no other way to reach the master.
      # Returns conditionMessage(e) (a plain character string) on error, or
      # NULL on success; console/log printing of the full condition +
      # traceback is preserved unchanged for a human watching the run live.
      processing_results <- parallel::parLapply(
        cl,
        files_to_process,
        fun = function(x) {
          # sink(file = getFileRunLogName(x, batchName), append = FALSE)
          tryCatch(
            {
              eyeQuality(
                x,
                displayDimensionX_mm = displayDimensionX_mm,
                displayDimensionY_mm = displayDimensionY_mm,
                saveData = TRUE,
                batchName = batchName,
                verbose = verbose,
                outputDir = outputDir,
                ...
              )
              NULL
            },
            # error = function(e)
            error = function(e) {
              print(e)
              print(traceback())
              conditionMessage(e)
            }
          )
          # sink()
        }
        # file = paste0(x, "/derivatives/eyeQuality-v1/", runtime_Log)
      )
      names(processing_results) <- files_to_process

      # Stop the cluster
      parallel::stopCluster(cl)
    }

    endtime <- getCurrentTime()


    # print(stringr::str_glue("--- BATCH PREPROCESSING COMPLETE! (run duration: {getPipelineTiming(starttime, endtime)})"))
    print_or_save(
      stringr::str_glue(
        "--- BATCH PREPROCESSING COMPLETE! (run duration: {getPipelineTiming(starttime, endtime)})"
      ),
      TRUE,
      batch_run_summary
    )

    # P7-03: this used to be a list.files()-based regex search rooted at
    # directoryBIDS, compared back against tsv_files_to_batch_process via a
    # gsub()/grepl() reconstruction of each raw file's basename. That both
    # ignored outputDir entirely (it would never find qcsummary output
    # written elsewhere) and was fragile against regex metacharacters in
    # filenames. Replaced with the same deterministic per-file
    # get_qcsummary_output_path() existence check used for the resumability
    # skip logic above, so "successfully processed"/"failed" reporting stays
    # consistent with outputDir and with what was actually skipped/dispatched
    # this run.
    qcsummary_paths_all <- if (length(tsv_files_to_batch_process) == 0) {
      character(0)
    } else {
      vapply(
        tsv_files_to_batch_process,
        get_qcsummary_output_path,
        character(1),
        batchName = batchName,
        outputDir = outputDir
      )
    }
    completed_mask <- if (length(qcsummary_paths_all) == 0) {
      logical(0)
    } else {
      file.exists(qcsummary_paths_all)
    }

    completedfiles <- qcsummary_paths_all[completed_mask]
    print_or_save(
      stringr::str_glue(
        "------ Successfully processed files (n = {length(completedfiles)}):  "
      ),
      TRUE,
      batch_run_summary
    )
    print_or_save(
      paste(completedfiles, collapse = "\n"),
      TRUE,
      batch_run_summary
    )

    # every candidate file with no matching qcsummary output, whether it was
    # dispatched this run and failed, or was never dispatched in the first
    # place (shouldn't happen outside of force = TRUE / a fresh run, but kept
    # as a straightforward complement of completedfiles rather than assuming)
    failedfiles <- tsv_files_to_batch_process[!completed_mask]

    # P7-04: look up each failed file's captured error message (see
    # processing_results above). A file can end up in failedfiles without a
    # captured message - e.g. it was never dispatched this run at all (a
    # skipped file can't land here since a skip requires an existing
    # qcsummary output, but this stays a defensive fallback rather than an
    # assumption), or its worker process ended (crashed/killed) without ever
    # reaching the tryCatch's error handler - so a placeholder is substituted
    # in that case rather than leaving the line blank.
    failed_errors <- vapply(
      failedfiles,
      function(f) {
        msg <- if (f %in% names(processing_results)) processing_results[[f]] else NULL
        if (is.null(msg) || !nzchar(trimws(msg))) {
          "(no error detail captured for this run - file was not dispatched this run, or its worker process ended without raising a catchable R error)"
        } else {
          sanitize_error_message_for_summary(msg)
        }
      },
      character(1),
      USE.NAMES = FALSE
    )

    print_or_save(
      stringr::str_glue(
        "------ Files that failed processing (n = {length(failedfiles)}):  "
      ),
      TRUE,
      batch_run_summary
    )
    # P7-04: each failed file gets two lines - its path, then an indented
    # "  error: <message>" line directly beneath it - rather than the
    # one-line-per-file format used by the successfulfiles/skippedfiles
    # sections. rbind()+as.vector() interleaves the two vectors column-major
    # (file1, error1, file2, error2, ...). Kept as a distinct block (not
    # folded into a one-liner) so the empty-failures case stays a clean no-op
    # rather than needing to special-case rbind() on zero-length input.
    failed_lines <- if (length(failedfiles) == 0) {
      character(0)
    } else {
      as.vector(rbind(failedfiles, paste0("  error: ", failed_errors)))
    }
    print_or_save(
      paste(failed_lines, collapse = "\n"),
      TRUE,
      batch_run_summary
    )

    print_or_save(
      stringr::str_glue("--- BATCH PROCESSING SUMMARY:  "),
      TRUE,
      batch_run_summary
    )
    # print(stringr::str_glue('"directory": "{directoryBIDS}", "data size (MB)": "{get_filesizes(tsv_files_to_batch_process)}", "n (ET Files)": "{length(tsv_files_to_batch_process)}", "n (preprocessed)": "{length(completedfiles)}", "n (failed preprocessing)": "{length(tsv_files_to_batch_process)-length(completedfiles)}", "run duration": "{getPipelineTiming(starttime, endtime)}",  "runtime (s)": "{calculateTimeDifference(starttime, endtime)}"'))
    print_or_save(
      stringr::str_glue(
        '"directory": "{directoryBIDS}", "data size (MB)": "{get_filesizes(tsv_files_to_batch_process)}", "n (ET Files)": "{length(tsv_files_to_batch_process)}", "n (preprocessed)": "{length(completedfiles)}", "n (failed preprocessing)": "{length(tsv_files_to_batch_process)-length(completedfiles)}", "run duration": "{getPipelineTiming(starttime, endtime)}",  "runtime (s)": "{calculateTimeDifference(starttime, endtime)}"'
      ),
      TRUE,
      batch_run_summary
    )



    # sinkReset()
    # sink()
  }

#' get_qcsummary_output_path (internal): compute the qcsummary output path
#' `saveFiles()` would write for a given raw input file / batchName /
#' outputDir combination, without requiring that the file already exist.
#' Mirrors `saveFiles()`'s `qcsummarydesc` naming convention and
#' `create_new_filename()`'s path-assembly logic (see `R/saveFiles.R`) -
#' keep the two in sync if that convention ever changes. Deliberately
#' reimplemented here rather than calling `create_new_filename()` directly:
#' that function's `fs::dir_create(newdirectory)` side effect is appropriate
#' when actually about to write a file, but not for a pure existence check
#' run against every candidate file before any processing has happened (which
#' would otherwise eagerly create empty `derivatives/eyeQuality-v1/`
#' directories for files never ultimately processed this run). Used by
#' `eyeQualityBatch()`'s resumability check (P7-03) to decide, before
#' dispatching any work, whether a given file already has a matching
#' qcsummary output on disk.
#'
#' @param inputFile filepath of the raw input file
#' @param batchName batch name, as passed to `eyeQualityBatch()`/`eyeQuality()`
#' @param outputDir optional directory overriding the default
#'   `<input_dir>/derivatives/eyeQuality-v1/` location. Default `NULL`.
#'
#' @return character filepath of the qcsummary output `saveFiles()` would
#'   write for this file/batchName/outputDir combination (whether or not it
#'   currently exists)
#' @keywords internal
#' @noRd
get_qcsummary_output_path <- function(inputFile, batchName = NULL, outputDir = NULL) {
  qcsummarydesc <- paste0(
    "_desc-",
    if (is.null(batchName)) "" else paste0(batchName, "_"),
    "preproc_qcsummary"
  )

  filename <- basename(fs::path_ext_remove(inputFile))
  directory <- fs::path_dir(inputFile)
  newdirectory <- if (is.null(outputDir)) {
    fs::path(directory, "derivatives", "eyeQuality-v1")
  } else {
    fs::path(outputDir)
  }
  newfilename <- fs::path(paste0(filename, qcsummarydesc), ext = "tsv")

  as.character(fs::path(newdirectory, newfilename))
}

#' sanitize_error_message_for_summary (internal): collapse a caught error's
#' `conditionMessage()` down to a single line for the "Files that failed
#' processing" section of the batch summary text file (P7-04). Some errors
#' (e.g. multi-sentence messages, or ones that embed a nested condition's
#' text) can themselves contain embedded newlines; since
#' `parsePreprocessingBatchSummary()`'s "failedfiles" branch parses that
#' section as exactly two lines per failed file (the filepath, then an
#' indented `"  error: ..."` line), an unsanitized multi-line message would
#' silently corrupt that fixed 2-lines-per-entry structure. Embedded
#' newlines/carriage-returns are collapsed to a single space rather than
#' dropped, so the message stays readable; surrounding whitespace is trimmed.
#'
#' @param msg character error message (typically `conditionMessage(e)`)
#'
#' @return character, single-line
#' @keywords internal
#' @noRd
sanitize_error_message_for_summary <- function(msg) {
  trimws(gsub("[\r\n]+", " ", msg))
}
