#' eyeQuality wrapper - run all preprocessing functions
#'
#' @param filepath path for data file as .tsv
#' @param displayDimensionX_mm integer of display width in millimeters. For example our 1920x1080 screen has a width of 594 mm
#' @param displayDimensionY_mm integer of display height in millimeters. For example our 1920x1080 screen has a width of 344 mm
#' @param data instead of passing a filepath, you can pass a data frame with your loaded data. Default NULL
#' @param eyeSelection_method string with possible values "Maximize", "Strict", "Left", or "Right". Default "Maximize"
#' @param smoothGaze_boolean Boolean indicating if we should run smoothGaze script
#' @param validityThreshold optional numeric on the generic `confidence` (0-1)
#'   scale (`1` = highest confidence, see `?eyeQuality-schema`) specifying the
#'   minimum per-sample confidence to retain; passed through to the detected
#'   adapter's `normalize_validity()`. Default `NULL`, meaning "use whatever
#'   validity threshold the detected adapter considers its own default" (see
#'   each adapter's `default_thresholds`, e.g. Tobii Studio's native `0`-`4`
#'   validity-code threshold of `2`, equivalent to a `confidence` cutoff of
#'   `0.5`; Tobii Pro's validity is a binary `"Valid"`/`"Invalid"` flag with
#'   no threshold concept at all). Replaces the pre-Phase-3
#'   `maxValidityThreshold` argument, which operated on Tobii Studio's
#'   native `0`-`4` scale directly and had no meaning for Tobii Pro.
#' @param saveData Boolean indicating if we should suppress output, and save data to files
#' @param includeIntermediates Boolean indicating if column outputs from all intermediate steps of the preprocessing should be included. Default = FALSE
#' @param studioEvents optional list of two values specifying the start and ending event labels for Tobii Studio files, c(startEventName, endEventName) . Default NULL
#' @param proEvents optional list of two values specifying the start and ending event labels for Tobii Pro files, c(startEventName, endEventName) . Default NULL
#' @param timeStart optional Recording Timestamp of the first data point to include. Default NULL
#' @param timeEnd optional Recording Timestamp of the last data point to include. Default NULL
#' @param batchName optional string to append output files with a specific batch run label. Default NULL
#' @param outputDir optional directory to write output files to when saveData = TRUE, overriding the default `<input_dir>/derivatives/eyeQuality-v1/` location. Default NULL
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @importFrom readr read_delim
#' @importFrom stringr str_glue
#'
#' @return preprocessed data
#' @export

eyeQuality <- function(filepath,
                       displayDimensionX_mm,
                       displayDimensionY_mm,
                       data = NULL,
                       eyeSelection_method = "Maximize",
                       smoothGaze_boolean = TRUE,
                       validityThreshold = NULL,
                       saveData = FALSE,
                       includeIntermediates = FALSE,
                       studioEvents = NULL,
                       proEvents = NULL,
                       timeStart = NULL,
                       timeEnd = NULL,
                       batchName = NULL,
                       outputDir = NULL,
                       ...) {
  # Wrapper
  runtime_start <- getCurrentTime()

  namedargs <- list(
    filepath = filepath,
    displayDimensionX_mm = displayDimensionX_mm,
    displayDimensionY_mm = displayDimensionY_mm,
    data = data,
    eyeSelection_method = eyeSelection_method,
    smoothGaze_boolean = smoothGaze_boolean,
    saveData = saveData,
    studioEvents = studioEvents,
    proEvents = proEvents,
    timeStart = timeStart,
    timeEnd = timeEnd
  )
  args <- list(...)
  args <- append(args, namedargs)

  if (saveData) {
    # get file path for run log
    # runtime_Log <- create_new_filename(filepath, "_RUNLOG", ".txt")
    runtime_Log <-
      create_new_filename(filepath,
        paste0(
          "_desc-",
          if (is.null(batchName)) "" else paste0(batchName, "_"),
          "preproc_runlog"
        ),
        ".txt",
        outputDir = outputDir
      )
    # if we are saving the output, sink all cmd messages to file
    sinkToOutputFile(runtime_Log)
    # save initial function call to run log.
    # inputFunctionCall <- match.call(expand.dots = TRUE)
    # lapply(args, write, runtime_Log, append = TRUE)
  } else {
    (runtime_Log <- NULL)
  }

  argValueSummaries <- vapply(args, function(argValue) {
    if (is.data.frame(argValue)) {
      paste0("<data.frame: ", nrow(argValue), "x", ncol(argValue), ">")
    } else if (is.list(argValue)) {
      paste0("<list: length ", length(argValue), ">")
    } else if (is.null(argValue)) {
      "NULL"
    } else if (is.atomic(argValue) && length(argValue) == 1) {
      as.character(argValue)
    } else if (is.atomic(argValue)) {
      paste0("<", class(argValue)[1], ": length ", length(argValue), ">")
    } else {
      paste0("<", class(argValue)[1], ">")
    }
  }, character(1))
  argList <-
    paste0(
      "eyeQuality(",
      paste(stringr::str_glue("{names(args)} = {argValueSummaries}"), collapse = ", "),
      ")"
    )
  print(paste0("command run: \n", argList))
  # print_or_save(argList, saveData, runtime_Log)

  # load data
  if (is.null(data)) {
    data <- importData(filepath)
  } else {
    # data <- data
  }

  # data <- sampleTobiiProTbl #for test data
  runtime_loadData <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 01. importData complete. (run duration: {getPipelineTiming(runtime_start, runtime_loadData)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 01. importData complete. (run duration: {getPipelineTiming(runtime_start, runtime_loadData)})"), saveData, runtime_Log)

  # detect software
  # detectImportSourceType() (P3-03) already dispatches through the adapter
  # registry and owns the "no adapter matched" error message; look up the
  # matching adapter object here so the remaining pipeline steps below can
  # call its standardize()/extract_events()/normalize_validity() methods
  # directly instead of the old software-string-dispatching functions.
  software <- detectImportSourceType(data)
  adapter <- registered_adapters()[[software]]
  runtime_detectImportSourceType <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 02. detectImportSourceType returns software = {software}. (run duration: {getPipelineTiming(runtime_loadData, runtime_detectImportSourceType)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 02. detectImportSourceType returns software = {software}. (run duration: {getPipelineTiming(runtime_loadData, runtime_detectImportSourceType)})"), saveData, runtime_Log)

  # format data to standard columns across software
  data <- adapter$standardize(data)

  runtime_standardizeColumnNames <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 03. standardizeColumnNames complete. (run duration: {getPipelineTiming(runtime_detectImportSourceType, runtime_standardizeColumnNames)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 03. standardizeColumnNames complete. (run duration: {getPipelineTiming(runtime_detectImportSourceType, runtime_standardizeColumnNames)})"), saveData, runtime_Log)

  # Check that any valid gaze data exists. If not, abort.
  leftDataExists <- checkGazeDataExists(data, gazeColumn = "gazeLeftX", ...)
  rightDataExists <- checkGazeDataExists(data, gazeColumn = "gazeRightX", ...)
  if (!leftDataExists & !rightDataExists) {
    diagnosticText <- paste0(
      "No valid gaze data exists. Preprocessing for file",
      filepath,
      " has been aborted."
    )
    print(diagnosticText)
    stop(diagnosticText)
  }

  runtime_checkGazeDataExists <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 04. checkGazeDataExists complete. (run duration: {getPipelineTiming(runtime_standardizeColumnNames, runtime_checkGazeDataExists)})"
    )
  )

  # save & remove event rows
  data_list <- adapter$extract_events(data)
  data <- data_list$gaze # gazestream data
  eventData <- data_list$events # event data

  runtime_extractEventRows <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 05. extractEventRows complete. (run duration: {getPipelineTiming(runtime_standardizeColumnNames, runtime_extractEventRows)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 04. extractEventRows complete. (run duration: {getPipelineTiming(runtime_standardizeColumnNames, runtime_extractEventRows)})"), saveData, runtime_Log)

  # check timestamp data is correctly ordered
  ordered <-
    checkOrderedTimestamps(data, timestamps = "recordingTimestamp_ms")
  if (!ordered) {
    diagnosticText <- paste0(
      "Data is not chronologically ordered based on timestamp. Pre-processing for file ",
      filepath,
      " has been aborted."
    )
    print(diagnosticText)
    # print_or_save(paste0("Data is not chronologically ordered based on timestamp. Pre-processing for file ", filepath, " has been aborted."), saveData, runtime_Log)
    stop(diagnosticText)
  }

  # get specific time range to process
  if (!isempty(studioEvents) && !isempty(proEvents)) {
    # set correct event identity based on software version
    if (software == "TobiiStudio") {
      firstEvent <- studioEvents[1]
      lastEvent <- studioEvents[2]
    } else if (software == "TobiiPro") {
      firstEvent <- proEvents[1]
      lastEvent <- proEvents[2]
    }
    # Specify start and end timestamp for task
    taskTimes <-
      getEventTimes(eventData,
        firstEvent = firstEvent,
        lastEvent = lastEvent,
        ...
      )
    if (length(taskTimes) == 2 &&
      !anyNA(taskTimes) && taskTimes[[1]] < taskTimes[[2]]) {
      data <- setTimestamps(data, taskTimes[[1]], taskTimes[[2]], ...)
      runtime_setTimestamps <- getCurrentTime()
      print(
        stringr::str_glue(
          "--- 05a. setTimestamps complete based on event parameter inputs. (run duration: {getPipelineTiming(runtime_extractEventRows, runtime_setTimestamps)})"
        )
      )
      # print_or_save(stringr::str_glue("--- 04a. setTimestamps complete based on event parameter inputs. (run duration: {getPipelineTiming(runtime_extractEventRows, runtime_setTimestamps)})"), saveData, runtime_Log)
    } else {
      # if event cannot be identified
      runtime_setTimestamps <- getCurrentTime()
      diagnosticText <- paste0(
        "Start or end timestamps could not be identified. Pre-processing for file ",
        filepath,
        " aborted."
      )
      print(diagnosticText)
      # print_or_save(paste0("Start or end timestamps could not be identified. Pre-processing for file ", filepath, " aborted."), saveData, runtime_Log)
      stop(diagnosticText)
    }
  } else if (!isempty(timeStart) & !isempty(timeEnd)) {
    # set based on input time stamps
    data <- setTimestamps(data, timeStart, timeEnd, ...)
    runtime_setTimestamps <- getCurrentTime()
    print(
      stringr::str_glue(
        "--- 05b. setTimestamps complete based on timestamp inputs. (run duration: {getPipelineTiming(runtime_extractEventRows, runtime_setTimestamps)})"
      )
    )
    # print_or_save(stringr::str_glue("--- 04b. setTimestamps complete based on timestamp inputs. (run duration: {getPipelineTiming(runtime_extractEventRows, runtime_setTimestamps)})"), saveData, runtime_Log)
  } else {
    # run whole file if no time range or events are specified
    runtime_setTimestamps <- getCurrentTime()
    print("No time range specified. Running pre-processing on full data file.")
    # print_or_save("No time range specified. Running pre-processing on full data file.", saveData, runtime_Log)
  }

  # Calculate recording Hz
  recordingFrequency_hz <- calculateFrequency_hz(data)
  runtime_calculateFrequency_hz <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 06. calculateFrequency_hz returns recordingFrequency_hz = {recordingFrequency_hz}. (run duration: {getPipelineTiming(runtime_setTimestamps, runtime_calculateFrequency_hz)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 05. calculateFrequecny_hz returns recordingFrequency_hz = {recordingFrequency_hz}. (run duration: {getPipelineTiming(runtime_setTimestamps, runtime_calculateFrequency_hz)})"), saveData, runtime_Log)

  # Mark invalid datapoints as NA
  ## check - create a function to mark event placeholders as NA?'
  # normalize_validity() operates on both eyes in a single call (unlike the
  # old per-eye removeInvalidGaze() calls it replaces) and additionally
  # computes new confidenceLeft/confidenceRight columns (?eyeQuality-schema)
  # that the pre-refactor pipeline never produced. These are stripped below
  # alongside the .valid columns when includeIntermediates = FALSE, and kept
  # when includeIntermediates = TRUE, the same treatment as every other
  # pipeline-internal column added since P1-05.
  #
  # validityThreshold (P3-08) is adapter-independent and expressed on the
  # generic confidence (0-1) scale, 1 = highest confidence. normalize_validity()
  # itself still takes `threshold` on each adapter's own device-native validity
  # scale (?new_eyetracker_adapter) -- when the caller doesn't override
  # anything, pass threshold = NULL straight through so each adapter falls
  # back to its own default_thresholds, which is exactly the pre-P3-08
  # default behavior (Tobii Studio's default_thresholds$validityThreshold is
  # 2, matching the old maxValidityThreshold default exactly; Tobii Pro
  # ignores threshold entirely either way). When the caller does supply
  # validityThreshold, convert it onto the calling adapter's native scale --
  # currently only Tobii Studio's normalize_validity() threshold has
  # scale-specific meaning (native 0-4, confidence = 1 - validity/4, per
  # .tobii_studio_confidence()/?eyeQuality-schema), so invert that mapping;
  # Tobii Pro's normalize_validity() ignores `threshold` regardless of value.
  nativeValidityThreshold <- if (is.null(validityThreshold)) {
    NULL
  } else if (software == "TobiiStudio") {
    (1 - validityThreshold) * 4
  } else {
    validityThreshold
  }
  data <- adapter$normalize_validity(data, threshold = nativeValidityThreshold)
  runtime_removeInvalidGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 07. removeInvalidGaze complete. (run duration: {getPipelineTiming(runtime_calculateFrequency_hz, runtime_removeInvalidGaze)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 06. removeInvalidGaze complete. (run duration: {getPipelineTiming(runtime_calculateFrequency_hz, runtime_removeInvalidGaze)})"), saveData, runtime_Log)

  # flag off-screen gazepoints (in pixel space)
  # get resolution
  displayResolutionX_px <-
    as.numeric(unique(data$resolutionWidth[!is.na(data$resolutionWidth)]))
  displayResolutionY_px <-
    as.numeric(unique(data$resolutionHeight[!is.na(data$resolutionHeight)]))
  # mark out-of-range offscreen gp for each eye
  data <-
    removeOffscreenGaze(
      data,
      gazeX = "gazeLeftX.valid",
      gazeY = "gazeLeftY.valid",
      distanceZ = "distanceLeftZ.valid",
      overwrite = c("pupilLeft.valid"),
      displayResolutionX_px,
      displayResolutionY_px,
      displayDimensionX_mm,
      displayDimensionY_mm,
      ...
    )
  data <-
    removeOffscreenGaze(
      data,
      gazeX = "gazeRightX.valid",
      gazeY = "gazeRightY.valid",
      distanceZ = "distanceRightZ.valid",
      overwrite = c("pupilRight.valid"),
      displayResolutionX_px,
      displayResolutionY_px,
      displayDimensionX_mm,
      displayDimensionY_mm,
      ...
    )

  runtime_removeOffscreenGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 08. removeOffscreenGaze complete. (run duration: {getPipelineTiming(runtime_removeInvalidGaze, runtime_removeOffscreenGaze)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 07. removeOffscreenGaze complete. (run duration: {getPipelineTiming(runtime_removeInvalidGaze, runtime_removeOffscreenGaze)})"), saveData, runtime_Log)

  # interpolation
  columnsToInterpolate <-
    c(
      "gazeLeftX.valid",
      "gazeLeftY.valid",
      "gazeRightX.valid",
      "gazeRightY.valid",
      "distanceLeftZ.valid",
      "distanceRightZ.valid",
      "pupilLeft.valid",
      "pupilRight.valid"
    )
  data <- interpolateGaze(data, recordingFrequency_hz, columnsToInterpolate, ...)
  runtime_interpolateGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 09. interpolateGaze complete. (run duration: {getPipelineTiming(runtime_removeOffscreenGaze, runtime_interpolateGaze)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 08. interpolateGaze complete. (run duration: {getPipelineTiming(runtime_removeOffscreenGaze, runtime_interpolateGaze)})"), saveData, runtime_Log)

  # blink detection
  data <-
    classifyBlinks(data,
      pupilLeft = "pupilLeft.int",
      pupilRight = "pupilRight.int",
      recordingFrequency_hz,
      ...
    )
  if (eyeSelection_method == "Maximize" || eyeSelection_method == "Strict") {
    data$blink.classification <- data$bothEyes.blink
  } else if (eyeSelection_method == "Left") {
    data$blink.classification <- data$pupilLeft.blink
  } else if (eyeSelection_method == "Right") {
    data$blink.classification <- data$pupilRight.blink
  }
  runtime_classifyBlinks <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 10. classifyBlinks complete. (run duration: {getPipelineTiming(runtime_interpolateGaze, runtime_classifyBlinks)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 09. classifyBlinks complete. (run duration: {getPipelineTiming(runtime_interpolateGaze, runtime_classifyBlinks)})"), saveData, runtime_Log)

  # eye selection
  data <- eyeSelection(data, eyeSelection_method = eyeSelection_method, ...)
  runtime_eyeSelection <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 11. eyeSelection complete. (run duration: {getPipelineTiming(runtime_classifyBlinks, runtime_eyeSelection)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 10. eyeSelection complete. (run duration: {getPipelineTiming(runtime_classifyBlinks, runtime_eyeSelection)})"), saveData, runtime_Log)

  # Check that valid gaze data exists after selection
  dataExists <- checkGazeDataExists(data, gazeColumn = "gazeX.eyeSelect", ...)
  if (!dataExists) {
    diagnosticText <- paste0(
      "No valid gaze data exists after eye selection. Preprocessing for file",
      filepath,
      " has been aborted."
    )
    print(diagnosticText)
    stop(diagnosticText)
  }

  runtime_checkGazeDataExists2 <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 12. checkGazeDataExists complete. (run duration: {getPipelineTiming(runtime_eyeSelection, runtime_checkGazeDataExists2)})"
    )
  )

  # Classify all data as onscreen or offscreen (exclusionary / within range)
  data <- classifyOffscreenGaze(data, displayResolutionX_px, displayResolutionY_px, eyeSelection_method, ...)

  runtime_classifyOffscreenGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 13. classifyOffscreenGaze complete. (run duration: {getPipelineTiming(runtime_checkGazeDataExists2, runtime_classifyOffscreenGaze)})"
    )
  )
  # smoothing - denoise function
  columnsToSmooth <-
    colnames(data)[grepl("\\.eyeSelect$", colnames(data), ignore.case = TRUE)] # get interpolated column names
  # noise_reduction_check <- ifelse(("smoothGaze_boolean" %in% names(args)) & smoothGaze_boolean == FALSE, FALSE, TRUE) #by default we denoise
  data <-
    smoothGaze(data, recordingFrequency_hz, columnsToSmooth, smoothGaze_boolean = smoothGaze_boolean, ...)
  runtime_smoothGaze <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 13. smoothGaze complete. (run duration: {getPipelineTiming(runtime_classifyOffscreenGaze, runtime_smoothGaze)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 11. smoothGaze complete. (run duration: {getPipelineTiming(runtime_eyeSelection, runtime_smoothGaze)})"), saveData, runtime_Log)

  # calculate visual angle
  if (smoothGaze_boolean) {
    data <-
      calculateGaze_va(
        data,
        displayResolutionX_px,
        displayResolutionY_px,
        displayDimensionX_mm,
        displayDimensionY_mm,
        gazeX = "gazeX.smooth",
        gazeY = "gazeY.smooth",
        distanceZ = "distanceZ.smooth",
        ...
      )
  } else if (!smoothGaze_boolean) {
    data <-
      calculateGaze_va(
        data,
        displayResolutionX_px,
        displayResolutionY_px,
        displayDimensionX_mm,
        displayDimensionY_mm,
        gazeX = "gazeX.eyeSelect",
        gazeY = "gazeY.eyeSelect",
        distanceZ = "distanceZ.eyeSelect",
        ...
      )
  }
  runtime_calculateGaze_va <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 14. calculateGaze_va complete. (run duration: {getPipelineTiming(runtime_smoothGaze, runtime_calculateGaze_va)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 12. calculateGaze_va complete. (run duration: {getPipelineTiming(runtime_smoothGaze, runtime_calculateGaze_va)})"), saveData, runtime_Log)

  # calculate velocity (using VA calculated gaze points)
  data <-
    calculateVelocity_va_ms(data,
      gazeX_va = "gazeX_va",
      gazeY_va = "gazeY_va",
      timestamp = "recordingTimestamp_ms",
      ...
    )
  runtime_calculateVelocity_va_ms <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 15. calculateVelocity_va_ms complete. (run duration: {getPipelineTiming(runtime_calculateGaze_va, runtime_calculateVelocity_va_ms)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 13. calculateVelocity_va_ms complete. (run duration: {getPipelineTiming(runtime_calculateGaze_va, runtime_calculateVelocity_va_ms})"), saveData, runtime_Log)

  ## smooth velocity
  velCols <-
    colnames(data)[grepl("_va_ms", colnames(data), ignore.case = TRUE)] # get velocity columns
  data <- smoothVelocity(data, recordingFrequency_hz, velocity = velCols, ...)
  runtime_smoothVelocity <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 16. smoothVelocity complete. (run duration: {getPipelineTiming(runtime_calculateVelocity_va_ms, runtime_smoothVelocity)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 14. smoothVelocity complete. (run duration: {getPipelineTiming(runtime_calculateVelocity_va_ms, runtime_smoothVelocity)})"), saveData, runtime_Log)

  # filter -  classifyGazeIVT from Liz, includes mergeAdj, removeShortfix
  IVT_list <-
    classifyGazeIVT(
      data,
      velocity = "velocityEuclidean.smooth_va_ms",
      gazeX_va = "gazeX_va",
      gazeY_va = "gazeY_va",
      recordingFrequency_hz = recordingFrequency_hz,
      ...
    )
  # Summarized IVT file
  data <- IVT_list[[1]]
  data$IVT.classification[which(data$IVT.classification == "missing" &
    data$blink.classification == 1)] <- "blink"
  # All metrics IVT file
  data_IVT_all <- IVT_list[[2]]

  runtime_classifyGazeIVT <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 17. classifyGazeIVT complete. (run duration: {getPipelineTiming(runtime_smoothVelocity, runtime_classifyGazeIVT)})"
    )
  )
  # print_or_save(stringr::str_glue("--- 15. classifyGazeIVT complete. (run duration: {getPipelineTiming(runtime_smoothVelocity, runtime_classifyGazeIVT)})"), saveData, runtime_Log)

  # Assign final column labels
  data <- assignFinalColumnNames(data, smoothGaze_boolean, ...)

  runtime_assignFinalColumnNames <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- 18. assignFinalColumnNames complete. (run duration: {getPipelineTiming(runtime_classifyGazeIVT, runtime_assignFinalColumnNames)})"
    )
  )

  # get final runtime
  runtime_final <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- PREPROCESSING COMPLETE (preprocessing run duration: {getPipelineTiming(runtime_start, runtime_final)})"
    )
  )
  # print_or_save(stringr::str_glue("--- PREPROCESSING COMPLETE (preprocessing run duration: {getPipelineTiming(runtime_start, runtime_final)})"), saveData, runtime_Log)


  # data quality - robustness, contiguity, precision, %missingness, etc...
  summaryMetrics <- calculateOutputMetrics(data)
  runtime_calcOutputMetrics <- getCurrentTime()
  print(
    stringr::str_glue(
      "--- OUTPUT METRICS CALCULATED (total run duration: {getPipelineTiming(runtime_start, runtime_calcOutputMetrics)})"
    )
  )
  # print_or_save(stringr::str_glue("--- OUTPUT METRICS CALCULATED (total run duration: {getPipelineTiming(runtime_start, runtime_calcOutputMetrics)})"), saveData, runtime_Log)

  times <- list(
    start = runtime_start,
    loadData = runtime_loadData,
    detectImportSourceType = runtime_detectImportSourceType,
    standardizeColumnNames = runtime_standardizeColumnNames,
    extractEventRows = runtime_extractEventRows,
    setTimestamps = runtime_setTimestamps,
    calculateFrequency_hz = runtime_calculateFrequency_hz,
    removeInvalidGaze = runtime_removeInvalidGaze,
    removeOffscreenGaze = runtime_removeOffscreenGaze,
    interpolateGaze = runtime_interpolateGaze,
    classifyBlinks = runtime_classifyBlinks,
    eyeSelection = runtime_eyeSelection,
    smoothGaze = runtime_smoothGaze,
    calculateGaze_va = runtime_calculateGaze_va,
    calculateVelocity_va_ms = runtime_calculateVelocity_va_ms,
    smoothVelocity = runtime_smoothVelocity,
    classifyGazeIVT = runtime_classifyGazeIVT,
    final = runtime_final,
    calcOutputMetrics = runtime_calcOutputMetrics,
    total = getCurrentTime()
  )

  durations <- list(
    loadData_duration = calculateTimeDifference(runtime_start, runtime_loadData,
      units =
        "secs"
    ),
    detectImportSourceType_duration = calculateTimeDifference(runtime_loadData, runtime_detectImportSourceType,
      units =
        "secs"
    ),
    standardizeColumnNames_duration = calculateTimeDifference(runtime_detectImportSourceType, runtime_standardizeColumnNames,
      units =
        "secs"
    ),
    extractEventRows_duration = calculateTimeDifference(runtime_standardizeColumnNames, runtime_extractEventRows,
      units =
        "secs"
    ),
    setTimestamps_duration = calculateTimeDifference(runtime_extractEventRows, runtime_setTimestamps,
      units =
        "secs"
    ),
    calculateFrequency_hz_duration = calculateTimeDifference(runtime_setTimestamps, runtime_calculateFrequency_hz,
      units =
        "secs"
    ),
    removeInvalidGaze_duration = calculateTimeDifference(runtime_calculateFrequency_hz, runtime_removeInvalidGaze,
      units =
        "secs"
    ),
    removeOffscreenGaze_duration = calculateTimeDifference(runtime_removeInvalidGaze, runtime_removeOffscreenGaze,
      units =
        "secs"
    ),
    interpolateGaze_duration = calculateTimeDifference(runtime_removeOffscreenGaze, runtime_interpolateGaze,
      units =
        "secs"
    ),
    classifyBlinks_duration = calculateTimeDifference(runtime_interpolateGaze, runtime_classifyBlinks,
      units =
        "secs"
    ),
    eyeSelection_duration = calculateTimeDifference(runtime_classifyBlinks, runtime_eyeSelection,
      units =
        "secs"
    ),
    smoothGaze_duration = calculateTimeDifference(runtime_eyeSelection, runtime_smoothGaze,
      units =
        "secs"
    ),
    calculateGaze_va_duration = calculateTimeDifference(runtime_smoothGaze, runtime_calculateGaze_va,
      units =
        "secs"
    ),
    calculateVelocity_va_ms_duration = calculateTimeDifference(runtime_calculateGaze_va, runtime_calculateVelocity_va_ms,
      units =
        "secs"
    ),
    smoothVelocity_duration = calculateTimeDifference(runtime_calculateVelocity_va_ms, runtime_smoothVelocity,
      units =
        "secs"
    ),
    classifyGazeIVT_duration = calculateTimeDifference(runtime_smoothVelocity, runtime_classifyGazeIVT,
      units =
        "secs"
    ),
    final_duration = calculateTimeDifference(runtime_classifyGazeIVT, runtime_final,
      units =
        "secs"
    ),
    calcOutputMetrics_duration = calculateTimeDifference(runtime_final, runtime_calcOutputMetrics,
      units =
        "secs"
    ),
    total_duration = calculateTimeDifference(runtime_start, runtime_calcOutputMetrics,
      units =
        "secs"
    )
  )

  timingData <- do.call(c, list(times, durations))

  # Clean up final dataset output

  # remove columns created in preprocessing pipeline that are not the final outputs / categories
  if (!includeIntermediates) {
    # remove temporary eye selection calculation columns
    tempCols <- c(
      colnames(data)[grepl("\\.temp$", colnames(data), ignore.case = TRUE)],
      colnames(data)[grepl("\\.es.selection$", colnames(data), ignore.case = TRUE)],
      colnames(data)[grepl("\\.valid$", colnames(data), ignore.case = TRUE)],
      "confidenceLeft",
      "confidenceRight"
    )
    for (t in tempCols) {
      data[[t]] <- NULL
    }

    data <- removeIntermediateCols(data, ...)
  }

  if (saveData) {
    # save event data
    # saveFiles(filepath, args, data, eventData, timingData, summaryMetrics, batchName)
    saveFiles(
      filepath,
      data,
      eventData,
      timingData,
      summaryMetrics,
      batchName,
      outputDir
    )
    sinkReset()
  } else {
    return(data)
  }
}
