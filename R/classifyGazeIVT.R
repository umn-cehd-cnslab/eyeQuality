#' Apply IVT Classifier
#'
#' @param data a dataframe
#' @param input_velocity string - variable column name containing velocity (in visual angles)
#' @param gazeX_va string - variable column name containing X gaze coordinates in visual angles
#' @param gazeY_va string - variable column name containing Y gaze coordinates in visual angles
#' @param recHz recording frequency in Hz
#' @param IVT_thresh fixation classifier velocity threshold in VA/s. Default: 50 VA / s
#' @param max_ang_adj_fix maximum distance (in VA) between two fixations to be merged. Default: 0/5 VA
#' @param max_time_adj_fix maximum time lapsed (in ms) between two fixations to be merged. Default: 75 ms
#' @param min_fix_dur threshold for minimum duration of a given fixation to be considered acceptable. Default: 60 ms
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @return outputs_dfs
#' @export
#'
classifyGazeIVT <-
  function(data,
           velocity,
           gazeX_va,
           gazeY_va,
           recordingFrequency_hz,
           fixationVelocityThreshold = 50,
           maxAdjacentFixationAngle = 0.5,
           maxAdjacentFixationTime = 75,
           minFixationDuration = 60,
           ...) {
    ivt_env <- new.env()

    # save original dataset for variable merge at end
    data_v1 <- data

    # Classify Fixations
    fixation_flag <- TRUE

    print("Applying IVT Fixation Classification Algorithm...")
    print(paste0("Thresholding Saccades: ", fixationVelocityThreshold, " VA/sec"))
    # calculate sampling interval
    sampling_interval <- round(1000 / recordingFrequency_hz, 3)
    # Create new IVT variables
    data$IVT.saccade <-
      data$IVT.fixation <-
      data$class <-
      data$class.adj <-
      data$class.adj.shortfix <-
      rep(NA, length(data[[velocity]]))

    # Initial classification based on euclidean velocity
    data$class[which(is.na(data[[velocity]]))] <- "missing"
    data$class[which(data[[velocity]] > fixationVelocityThreshold)] <-
      "saccade"
    data$class[which(data[[velocity]] <= fixationVelocityThreshold)] <-
      "fixation"
    # mark valid gazepoints with no velocity calculated as "unclassified"
    data$class[which(is.na(data[[velocity]]) &
      !is.na(data[[gazeX_va]]))] <- "unclassified"


    # If no fixations detected, do not continue
    if (sum(data$class %in% "fixation") == 0) {
      fixation_flag <- FALSE
      data$IVT.classification <- data$class
      data$fix.ind <- NA
    }

    if (fixation_flag) {
      # Establish Start and End Points of Fixation Class Cluster
      # invisible(list2env(findFixationIndices(data$class), envir = globalenv()))
      invisible(list2env(findFixationIndices(data$class), envir = ivt_env))

      # Load proposed fixations and merge fixations that are very close both spatially and temporally
      # data[,c("class.adj","class.adj.euc","class.adj.gap.dur","class.adj.xva","class.adj.yva","class.adj.num","class.adj.dur")] <- mergeAdjacentFixations(data, entry.class = data$class, rle_fix_index, xcoords = data[[gazeX_va]], ycoords = data[[gazeY_va]], end.fix, start.fix, lengths.fix, max_ang_adj_fix, max_time_adj_fix)
      data[, c(
        "class.adj",
        "class.adj.euc",
        "class.adj.gap.dur",
        "class.adj.xva",
        "class.adj.yva",
        "class.adj.num",
        "class.adj.dur"
      )] <-
        mergeAdjacentFixations(
          data,
          entryClass = data$class,
          ivt_env$rle_fix_index,
          gazeX = data[[gazeX_va]],
          gazeY = data[[gazeY_va]],
          fixationEnd = ivt_env$end.fix,
          fixationStart = ivt_env$start.fix,
          fixationLength = ivt_env$lengths.fix,
          mergeDistance_va = maxAdjacentFixationAngle,
          mergeTimeGap_ms = maxAdjacentFixationTime
        )
      if (sum(data$class.adj %in% "fixation") == 0) {
        fixation_flag <- FALSE
        data$IVT.classification <- data$class.adj
        data$fix.ind <- NA
      }

      if (fixation_flag) {
        # Establish Updated Start and End Points of Fixation Class Cluster
        # invisible(list2env(findFixationIndices(data$class.adj), envir = globalenv()))
        invisible(list2env(findFixationIndices(data$class.adj), envir = ivt_env))

        # Load proposed fixations and remove small fixations
        data[, c("class.adj.shortfix")] <- NA
        # data[,c("class.adj.shortfix","class.adj.euc","class.adj.gap.dur","class.adj.xva","class.adj.yva","class.adj.num","class.adj.dur")] <- removeShortFixations(data, entry.class = data$class.adj, rle_fix_index, end.fix, start.fix, lengths.fix, min_fix_dur)
        data[, c(
          "class.adj.shortfix",
          "class.adj.euc",
          "class.adj.gap.dur",
          "class.adj.xva",
          "class.adj.yva",
          "class.adj.num",
          "class.adj.dur"
        )] <-
          removeShortFixations(
            data,
            entryClass = data$class.adj,
            rleFixIndex = ivt_env$rle_fix_index,
            fixationEnd = ivt_env$end.fix,
            fixationStart = ivt_env$start.fix,
            fixationLength = ivt_env$lengths.fix,
            shortFixationThreshold_ms = minFixationDuration
          )
        if (sum(data$class.adj.shortfix %in% "fixation") == 0) {
          fixation_flag <- FALSE
          data$IVT.classification <- data$class.adj.shortfix
          data$fix.ind <- NA
        }

        # Establish Start and End Points of Fixation Class Cluster
        if (fixation_flag) {
          # invisible(list2env(findFixationIndices(data$class.adj.shortfix), envir = globalenv()))
          invisible(list2env(
            findFixationIndices(data$class.adj.shortfix),
            envir = ivt_env
          ))

          # Assign index numbers to final classification of fixations and saccades
          ## replicated using Dalrymple script in R and python and output meaningful content to save
          # saccades
          data$sac.ind <- NA
          if (length(ivt_env$rle_sac_index) > 0) {
            for (isac in 1:length(ivt_env$rle_sac_index)) {
              # data$sac.ind[start.sac[rle_sac_index][isac]:end.sac[rle_sac_index][isac]] <- isac
              # data$IVT.saccade[start.sac[rle_sac_index][isac]:end.sac[rle_sac_index][isac]] <- "saccade"
              data$sac.ind[ivt_env$start.sac[ivt_env$rle_sac_index][isac]:ivt_env$end.sac[ivt_env$rle_sac_index][isac]] <-
                isac
              data$IVT.saccade[ivt_env$start.sac[ivt_env$rle_sac_index][isac]:ivt_env$end.sac[ivt_env$rle_sac_index][isac]] <-
                "saccade"
            }
          }
          # fixations
          fixind <- levels(factor(data$class.adj.num))
          data$fix.ind <- NA
          if (length(fixind) > 0) {
            for (ifix in 1:length(fixind)) {
              data$fix.ind[data$class.adj.num %in% as.integer(fixind)[ifix]] <-
                ifix
              data$IVT.fixation[data$class.adj.num %in% as.integer(fixind)[ifix]] <-
                "fixation"
            }
          }
          # convert duration to ms
          data$class.adj.dur_ms <-
            data$class.adj.dur * sampling_interval

          # select which columns to save in output file
          output_fixations <-
            data.frame(
              recordingTimestamp_ms = data$recordingTimestamp_ms,
              IVT.classification = data$class.adj.shortfix,
              # Final Classification
              IVT.fixationIndex = data$fix.ind,
              IVT.saccadeIndex = data$sac.ind,
              IVT.fixationDuration_ms = data$class.adj.dur_ms
            )

          # merge output columns to original input file
          data_output <-
            merge(data_v1, output_fixations, by = "recordingTimestamp_ms")
        }
      }
    }

    # output list of 1). the file with only summary variables and 2). a file with the full output of all variable calculated in the function
    if (fixation_flag) {
      output_dfs <- list(data_output, data)
    } else if (!fixation_flag) {
      output_dfs <- list(data, NA)
    }
    return(output_dfs)
  }
