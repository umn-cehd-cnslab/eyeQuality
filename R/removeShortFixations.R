#' Removes proposed fixations that are too short
#'
#' @param data dataframe
#' @param entryClass label for entry classification
#' @param rleFixIndex list of integers, generated from output of function findFixationIndices
#' @param fixationEnd list of fixation endpoints as indexes, generated from output of function findFixationIndices
#' @param fixationStart list of fixation start points as indexes, generated from output of function findFixationIndices
#' @param fixationLength integer list of fixation durations, generated from output of function findFixationIndices
#' @param shortFixationThreshold_ms threshold for minimum duration of a given fixation to be considered acceptable. Recommended default: 60 ms
#' @param sampling_interval recording's actual sample-to-sample interval, in ms (i.e. 1000 / recordingFrequency_hz). Used to convert a fixation length in samples to ms when comparing against shortFixationThreshold_ms. Default: 3.3, matching a ~303 Hz recording
#'
#' @return return list of updated fixation metrics, including classification (class.adj), duration, and distance
#' @export
#'
removeShortFixations <-
  function(data,
           entryClass,
           rleFixIndex,
           fixationEnd,
           fixationStart,
           fixationLength,
           shortFixationThreshold_ms,
           sampling_interval = 3.3) {
    print("Removing short fixations")
    fix_x <-
      fix_y <-
      fix_euc_adj <- gap_dur_adj <- rep(NA, length(rleFixIndex))
    data$class.adj.shortfix <- entryClass

    for (ifix in 1:length(rleFixIndex)) {
      if (fixationLength[rleFixIndex][ifix] * sampling_interval < shortFixationThreshold_ms) {
        end.change.index <- fixationEnd[rleFixIndex][ifix]
        start.change.index <- fixationStart[rleFixIndex][ifix]
        data$class.adj.shortfix[start.change.index:end.change.index] <-
          "unclassified"
        data$class.adj.euc[start.change.index:end.change.index] <- NA
        data$class.adj.gap.dur[start.change.index:end.change.index] <-
          NA
        data$class.adj.xva[start.change.index:end.change.index] <- NA
        data$class.adj.yva[start.change.index:end.change.index] <- NA
        data$class.adj.num[start.change.index:end.change.index] <- NA
        data$class.adj.dur[start.change.index:end.change.index] <- NA
      }
    }

    return(data[, c(
      "class.adj.shortfix",
      "class.adj.euc",
      "class.adj.gap.dur",
      "class.adj.xva",
      "class.adj.yva",
      "class.adj.num",
      "class.adj.dur"
    )])
  }
