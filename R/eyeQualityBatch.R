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
#' @param ... additional parameters
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

    # Create a cluster with multiple cores for parallel processing
    if (is.null(numberCores)) {
      # if numberCores not specified, use the lesser to of
      # (a) 85% of available cores or
      # (b) number of files you need to process
      numcores <-
        ifelse(
          # check if there are fewer files the possible number of cores
          floor(detectCores() * 0.85) < length(tsv_files_to_batch_process),
          ifelse(floor(detectCores() * 0.85) <= 0, # if 85% of cores <= 0, use 1 core
            1,
            floor(detectCores() * 0.85)
          ),
          # otherwise use 85% of available cores),
          length(tsv_files_to_batch_process) #
        )
    } else {
      # otherwise use specified number of cores
      numcores <- ifelse(
        numberCores < length(tsv_files_to_batch_process),
        ifelse(numberCores <= 0, 1, numberCores),
        length(tsv_files_to_batch_process)
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
    parallel::parLapply(
      cl,
      tsv_files_to_batch_process,
      fun = function(x) {
        # sink(file = getFileRunLogName(x, batchName), append = FALSE)
        tryCatch(
          eyeQuality(
            x,
            displayDimensionX_mm = displayDimensionX_mm,
            displayDimensionY_mm = displayDimensionY_mm,
            saveData = TRUE,
            batchName = batchName,
            verbose = verbose,
            ...
          ),
          # error = function(e)
          error = function(e) {
            print(e)
            print(traceback())
          }
        )
        # sink()
      }
      # file = paste0(x, "/derivatives/eyeQuality-v1/", runtime_Log)
    )

    # Stop the cluster
    parallel::stopCluster(cl)

    endtime <- getCurrentTime()


    # print(stringr::str_glue("--- BATCH PREPROCESSING COMPLETE! (run duration: {getPipelineTiming(starttime, endtime)})"))
    print_or_save(
      stringr::str_glue(
        "--- BATCH PREPROCESSING COMPLETE! (run duration: {getPipelineTiming(starttime, endtime)})"
      ),
      TRUE,
      batch_run_summary
    )

    qcsummaryPattern <-
      paste0(
        "_desc-",
        if (is.null(batchName)) "" else paste0(batchName, "_"),
        "preproc_qcsummary\\.tsv$"
      )

    completedfiles <-
      list.files(
        directoryBIDS,
        pattern = qcsummaryPattern,
        recursive = TRUE,
        full.names = TRUE
      )
    # completedfiles <- listBidsDerivativeFiles(directoryBIDS, modality_pattern = qcsummaryPattern)
    # print(completedfiles)
    # print_or_save(completedfiles, TRUE, batch_run_summary)
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

    # compare list to batch process with the completed file list.
    failedfiles <-
      tsv_files_to_batch_process[!grepl(
        gsub("\\|$", "", paste0(
          fs::path_file(gsub(qcsummaryPattern, "", completedfiles)),
          sep = "|",
          collapse = ""
        )),
        tsv_files_to_batch_process
      )]


    print_or_save(
      stringr::str_glue(
        "------ Files that failed processing (n = {length(failedfiles)}):  "
      ),
      TRUE,
      batch_run_summary
    )
    print_or_save(
      paste(failedfiles, collapse = "\n"),
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
