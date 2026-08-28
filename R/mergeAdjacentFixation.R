#' Merge Adjacent Fixations
#'
#' @param data dataframe
#' @param entryClass list of IVT-assigned class assignments per gazepoint
#' @param rleFixIndex list of integers, generated from output of function findFixationIndices
#' @param gazeX list of x gaze coordinates as visual angles
#' @param gazeY list of y gaze coordinates as visual angles
#' @param fixationEnd list of fixation endpoints as indexes, generated from output of function findFixationIndices
#' @param fixationStart list of fixation start points as indexes, generated from output of function findFixationIndices
#' @param fixationLength list of fixation durations, generated from output of function findFixationIndices
#' @param mergeDistance_va maximum distance (in VA) between two fixations to be merged. Default: 0/5 VA
#' @param mergeTimeGap_ms maximum time lapsed (in ms) between two fixations to be merged. Default: 75 ms
#' @param sampling_interval recording's actual sample-to-sample interval, in ms (i.e. 1000 / recordingFrequency_hz). Used to convert a gap duration in samples to ms when comparing against mergeTimeGap_ms. Default: 3.3, matching a ~303 Hz recording
#'
#' @return return list of updated fixation metrics, including classification (class.adj), duration, and distance
#' @export
#'
# This function iterates through each proposed fixation in a time intensive manner (see Tobii white paper for merge fixation logic)
mergeAdjacentFixations <-
  function(data,
           entryClass,
           rleFixIndex,
           gazeX,
           gazeY,
           fixationEnd,
           fixationStart,
           fixationLength,
           mergeDistance_va = 0.5,
           mergeTimeGap_ms = 75,
           sampling_interval = 3.3) {
    # list2env(findFixationIndices(data.raw.df$class), envir = globalenv())

    fix_x <-
      fix_y <-
      fix_euc_adj <- gap_dur_adj <- rep(NA, length(rleFixIndex))

    data$class.adj <- entryClass
    data$class.adj.euc <-
      data$class.adj.gap.dur <-
      data$class.adj.xva <-
      data$class.adj.yva <-
      data$class.adj.num <- data$class.adj.dur <- NA

    ifix <- 1
    fixation_set_complete <- FALSE
    while (fixation_set_complete == FALSE) {
      fixation_complete <- FALSE
      fixation_b_complete <- FALSE

      # primary forward fixation merge loop

      while (fixation_complete == FALSE) {
        if (ifix == 1 | ifix %% 100 == 0) {
          print(paste(
            "Fixation: ",
            ifix,
            "; Indices: ",
            fixationStart[rleFixIndex][ifix],
            " to ",
            fixationEnd[rleFixIndex][ifix]
          ))
        }

        # starting point exists
        # if(length(grep("TRUE",!is.na(gazeX[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]])))>0){

        # initial proposed fixation identify median x,y coordinate
        fix_x[ifix] <-
          data$class.adj.xva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
          median(gazeX[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]],
            na.rm =
              TRUE
          )
        fix_y[ifix] <-
          data$class.adj.yva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
          median(gazeY[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]],
            na.rm =
              TRUE
          )

        # assess entire length of proposed new fixation
        dur <-
          length(fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix])

        # assess temporal distance between this fixation and prior fixation
        start.change.index <- fixationEnd[rleFixIndex][ifix] + 1
        end.change.index <- fixationStart[rleFixIndex][ifix + 1] - 1

        if (!is.na(fix_x[ifix]) & !is.na(fix_y[ifix])) {
          # assuming there is an index for a later point and
          if (!is.na(end.change.index + 1)) {
            # next fixations identified
            fix_x[ifix + 1] <-
              median(gazeX[fixationStart[rleFixIndex][ifix + 1]:fixationEnd[rleFixIndex][ifix + 1]],
                na.rm =
                  TRUE
              )
            fix_y[ifix + 1] <-
              median(gazeY[fixationStart[rleFixIndex][ifix + 1]:fixationEnd[rleFixIndex][ifix + 1]],
                na.rm =
                  TRUE
              )

            # assess euclidean distance between this fixation and prior fixation
            fix_euc <-
              sqrt((fix_x[ifix] - fix_x[ifix + 1])^2 + (fix_y[ifix] - fix_y[ifix +
                1])^2)
            fix_euc_adj[ifix] <- fix_euc

            gap_dur <- length(end.change.index:start.change.index)
            gap_dur_adj[ifix] <- gap_dur

            if ((is.na(fix_euc))) {
              print("NA FOUND IN SECOND FIXATION")
            }

            # if two consecutive fixations are fairly close to eachother...
            if (!is.na(fix_euc) & fix_euc < mergeDistance_va) {
              # ... and if not too much time has transpired, then merge
              if (gap_dur * sampling_interval < mergeTimeGap_ms) {
                data$class.adj[start.change.index:end.change.index] <- "fixation"
              }
              if (gap_dur * sampling_interval >= mergeTimeGap_ms) {
                data$class.adj.xva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                  fix_x[ifix]
                data$class.adj.yva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                  fix_y[ifix]
                data$class.adj.euc[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                  fix_euc
                data$class.adj.gap.dur[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                  gap_dur
                data$class.adj.dur[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                  dur
                data$class.adj.num[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                  ifix
                # print(data[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix + 1],c("class","class.adj","class.adj.euc","class.adj.gap.dur","class.adj.xva","class.adj.yva","class.adj.num")])
                # print(paste("Too Long: ",gap_dur*sampling_interval, " ms duration ,", fix_euc, "va distance between fixations"))
                fixation_complete <- TRUE
              }
            }
            if (!is.na(fix_euc) &
              fix_euc >= mergeDistance_va | (is.na(fix_euc))) {
              data$class.adj.xva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                fix_x[ifix]
              data$class.adj.yva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                fix_y[ifix]
              data$class.adj.euc[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                fix_euc
              data$class.adj.gap.dur[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                gap_dur
              data$class.adj.dur[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                dur
              data$class.adj.num[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                ifix
              fixation_complete <- TRUE
            }
          }
          if (is.na(end.change.index + 1)) {
            data$class.adj.xva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
              fix_x[ifix]
            data$class.adj.yva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
              fix_y[ifix]
            data$class.adj.dur[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
              dur
            data$class.adj.num[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
              ifix
            fixation_complete <- TRUE
          }

          # setup for another pass through the while loop or exit if done
          if (fixation_complete == FALSE) {
            rleFixIndex <- which(rle(data$class.adj)$values == "fixation")
            rle_sac_index <-
              which(rle(data$class.adj)$values == "saccade")

            fixationEnd <- cumsum(rle(data$class.adj)$lengths)
            fixationStart <- c(1, lag(fixationEnd)[-1] + 1)
            fixationLength <- fixationEnd - fixationStart + 1

            # check if ran out of potential fixations to merge, if not back to top of while loop
            if (ifix >= length(rleFixIndex)) {
              data$class.adj.xva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                fix_x[ifix]
              data$class.adj.yva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                fix_y[ifix]
              data$class.adj.euc[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                fix_euc
              data$class.adj.gap.dur[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                gap_dur
              data$class.adj.dur[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                dur
              data$class.adj.num[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                ifix

              fix_x[ifix] <-
                data$class.adj.xva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                median(gazeX[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]],
                  na.rm =
                    TRUE
                )
              fix_y[ifix] <-
                data$class.adj.yva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
                median(gazeY[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]],
                  na.rm =
                    TRUE
                )

              fixation_set_complete <- TRUE
              print("Done")
              break
            }
          }
        }
        if (is.na(fix_x[ifix]) | is.na(fix_y[ifix])) {
          print("NA FOUND IN FIRST FIXATION")
          data$class.adj[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
            "unclassified"

          rleFixIndex <-
            which(rle(data$class.adj)$values == "fixation")
          rle_sac_index <-
            which(rle(data$class.adj)$values == "saccade")

          fixationEnd <- cumsum(rle(data$class.adj)$lengths)
          fixationStart <- c(1, lag(fixationEnd)[-1] + 1)
          fixationLength <- fixationEnd - fixationStart + 1

          fixation_complete <- TRUE
          fixation_b_complete <- TRUE

          ifix <- ifix - 1
        }
      }

      # primary backward fixation merge loop

      if (ifix >= 2) {
        while (fixation_b_complete == FALSE) {
          fix_xb <-
            median(gazeX[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]],
              na.rm =
                TRUE
            )
          fix_yb <-
            median(gazeY[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]],
              na.rm =
                TRUE
            )

          # prev fixations identified
          fix_xa <-
            median(gazeX[fixationStart[rleFixIndex][ifix - 1]:fixationEnd[rleFixIndex][ifix - 1]],
              na.rm =
                TRUE
            )
          fix_ya <-
            median(gazeY[fixationStart[rleFixIndex][ifix - 1]:fixationEnd[rleFixIndex][ifix - 1]],
              na.rm =
                TRUE
            )

          # assess length of proposed fixation
          dur <-
            length(fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix])

          # assess euclidean distance between this fixation and prior fixation
          fix_euc <- sqrt((fix_xb - fix_xa)^2 + (fix_yb - fix_ya)^2)

          # assess temporal distance between this fixation and prior fixation
          start.change.index <- fixationEnd[rleFixIndex][ifix - 1] + 1
          end.change.index <- fixationStart[rleFixIndex][ifix] - 1

          gap_dur <- length(end.change.index:start.change.index)

          # if two consecutive fixations are fairly close to eachother...
          if (!is.na(fix_euc) &
            !is.na(mergeDistance_va) & fix_euc < mergeDistance_va) {
            # ... and if not too much time has transpired, then merge
            if (gap_dur * sampling_interval < mergeTimeGap_ms) {
              data$class.adj[start.change.index:end.change.index] <- "fixation"
              ifix <- ifix - 1
            }
            if (gap_dur * sampling_interval >= mergeTimeGap_ms) {
              fixation_b_complete <- TRUE
            }
          }
          if ((!is.na(fix_euc) &
            !is.na(mergeDistance_va)) &
            fix_euc >= mergeDistance_va | (is.na(fix_euc) | is.na(mergeDistance_va))) {
            fixation_b_complete <- TRUE
          }

          # setup for another pass through the while loop or exit if done
          if (fixation_b_complete == FALSE) {
            rleFixIndex <- which(rle(data$class.adj)$values == "fixation")
            rle_sac_index <-
              which(rle(data$class.adj)$values == "saccade")

            fixationEnd <- cumsum(rle(data$class.adj)$lengths)
            fixationStart <- c(1, lag(fixationEnd)[-1] + 1)
            fixationLength <- fixationEnd - fixationStart + 1

            # check if ran out of potential fixations to merge, if not back to top of while loop
            if (ifix == 1) {
              ifix <- ifix - 1
              fixation_b_complete <- TRUE
            }
          }

          if (ifix > 1) {
            data$class.adj.xva[fixationStart[rleFixIndex][ifix - 1]:fixationEnd[rleFixIndex][ifix -
              1]] <- fix_xa
            data$class.adj.yva[fixationStart[rleFixIndex][ifix - 1]:fixationEnd[rleFixIndex][ifix -
              1]] <- fix_ya
            data$class.adj.xva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
              fix_xb
            data$class.adj.yva[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
              fix_yb
            data$class.adj.euc[fixationStart[rleFixIndex][ifix - 1]:fixationEnd[rleFixIndex][ifix -
              1]] <- fix_euc
            data$class.adj.gap.dur[fixationStart[rleFixIndex][ifix - 1]:fixationEnd[rleFixIndex][ifix -
              1]] <- gap_dur
            data$class.adj.dur[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
              dur
            data$class.adj.num[fixationStart[rleFixIndex][ifix - 1]:fixationEnd[rleFixIndex][ifix -
              1]] <- ifix - 1
            data$class.adj.num[fixationStart[rleFixIndex][ifix]:fixationEnd[rleFixIndex][ifix]] <-
              ifix
          }
        }
      }

      ifix <- ifix + 1

      # check if done with fixations
      if (ifix > length(rleFixIndex)) {
        fixation_set_complete <- TRUE
        print("Done merging fixations")
      }
    }

    return(data[, c(
      "class.adj",
      "class.adj.euc",
      "class.adj.gap.dur",
      "class.adj.xva",
      "class.adj.yva",
      "class.adj.num",
      "class.adj.dur"
    )])
    # return(data$class.adj)
  }
