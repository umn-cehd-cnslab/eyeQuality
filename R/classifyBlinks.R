#' Function to detect blinks adapted from a noise-based algorithm developed by Hershman, Henik, & Cohen (2018) https://link.springer.com/article/10.3758/s13428-017-1008-1; https://osf.io/jyz43/
#'
#' @param data dataframe
#' @param pupilLeft name of columns containing left pupil data to be analyzed
#' @param pupilRight name of columns containing right pupil data to be analyzed
#' @param recordingFrequency_hz integer from calculateFrequency_hz.R
#' @param maxArtifactLength_ms integer maximum length of valid data segment to be considered an artifact
#' @param minBlinkLength_ms integer min value for missing that could be considered a blink
#' @param maxBlinkLength_ms integer max value for missing that could be considered a blink
#' @param verbose optional Boolean. When `TRUE`, emits P3-10 diagnostics:
#'   candidate missing-data runs longer than `maxBlinkLength_ms` (not
#'   classified as blinks -- possible tracking loss or extended eye closure
#'   rather than a genuine blink) and a per-eye blink count summary. Default
#'   `FALSE`; purely additive, does not change blink classification.
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @importFrom forecast ma
#' @importFrom data.table as.data.table
#' @importFrom stats na.omit
#' @importFrom pracma isempty
#' @importFrom utils head
#' @importFrom utils tail
#'
#'
#' @return dataframe with new columns indicating presence/absence of blinks for each pupil data type
#' @export
#'
# Code from: Hershman, R., Henik, A. & Cohen, N. A novel blink detection method based on pupillometry noise.
#   Behav Res 50, 107–114 (2018). https://doi.org/10.3758/s13428-017-1008-1
# NOTE: this function operates assuming NA for missing and that a blink must fall btw 100-400 ms based on the literature (see above paper);
# non-blink data could be missing
#
# STEPS\cr
# 1.	Read in pupil data\cr
# 2.	Find start stop of missing pupil data as candidate blink markers\cr
# 3.	Smooth the pupil data\cr
# 4.	Find true blink onsets/offsets based on how quickly pupil data are changing on either side of the candidate blink markers\cr
# 5.	Remove duplicate values such that [a, b, b, c] => [a, c]\cr
# 6.	Check to make sure blinks are within 100-400 ms\cr
# 7.	Demarcate blinks in data frame
# 8.  Create composite blink column (if data for both pupils is input)

classifyBlinks <- function(data,
                           pupilLeft = "pupilLeft.int",
                           pupilRight = "pupilRight.int",
                           recordingFrequency_hz,
                           maxArtifactLength_ms = 15,
                           minBlinkLength_ms = 100,
                           maxBlinkLength_ms = 400,
                           verbose = FALSE,
                           ...) {
  # This adaptation to R was made with the supervision and encouragement of Dr William Paul Boyce.
  # For more information about this adaptation and for more R solutions, don't hesitate to contact him: paul.boyce@ntu.edu.sg
  # Dr. Jason Geller helped diagnose a coding error in the original script where matching was being done on index instead of time
  # based_noise_blinks_detection <- function(pupil_data, sampling_rate_in_hz){
  #

  sampling_interval <-
    round(1000 / recordingFrequency_hz) # compute the sampling time interval in milliseconds.
  artifact_interval <- round(maxArtifactLength_ms / sampling_interval)

  # added by Eli to define the lengths of missing we are willing to consider blinks (original code working with clean adult data) --can integrate with Liz's robustness code eventually
  blink_length_min <-
    minBlinkLength_ms / sampling_interval # set the minimum blink length threshold (in frames)
  blink_length_max <-
    maxBlinkLength_ms / sampling_interval # set the maximum blink length threshold (in frames)

  # gathering all pupil data: should be left and right pupil diameters
  pupil_data_types <- c(pupilLeft, pupilRight)
  blinkCols <- rep(NA, length(pupil_data_types))

  blink.summary <- data.table()

  for (x in 1:length(pupil_data_types)) {
    pupil_type <- pupil_data_types[x]
    if (is.na(pupil_type)) {
      next
    }
    pupil_data <- as.matrix(as.numeric(data[[pupil_type]]))

    # creating a logical vector indicating when there are blinks based on pupil data absence
    pupil_data[is.na(pupil_data)] <- 0 # changes missing to 0
    blinks_data <- as.logical(pupil_data == 0)
    # remove small artifacts (under 15 ms in length)
    rle_noblink_index <- which(rle(blinks_data)$values == FALSE)
    end.nb <- cumsum(rle(blinks_data)$lengths)
    start.nb <- c(1, lag(end.nb)[-1] + 1)
    lengths.nb <- end.nb - start.nb + 1
    artifact_ind <-
      which(lengths.nb[rle_noblink_index] <= artifact_interval)
    if (!isempty(artifact_ind)) {
      for (a in artifact_ind) {
        if ((rle_noblink_index[a] == 1 ||
          lengths.nb[rle_noblink_index[a] - 1] >= lengths.nb[rle_noblink_index][a]) ||
          (rle_noblink_index[a] == length(lengths.nb) ||
            lengths.nb[rle_noblink_index[a] + 1] >= lengths.nb[rle_noblink_index][a])) {
          blinks_data[start.nb[rle_noblink_index][a]:end.nb[rle_noblink_index][a]] <-
            TRUE
        }
      }
    }
    pupil_data[blinks_data] <- 0

    # creates a vector with negative values for onsets and positive values for offsets
    blinks <-
      c(-1 * which(diff(blinks_data) %in% 1), which(diff(blinks_data) %in% -1) +
        1)

    # Case 1: there are no blinks
    if (length(blinks) == 0) {
      res <- NA
      print(paste("No blinks were detected for ", pupil_type))
    } else {
      # Sort the blinks by absolute value. in this way we are getting an array of blinks when the offset appears after the onset
      blinks <- blinks[order(abs(blinks))]
      print(paste("Demarcating blinks for ", pupil_type))

      # ISSUE: because our data often has really short instances of missing (e.g., 1-2 frames), the above code the demarcates onset and offset (which uses the frame on either)
      # end of the missing value, produces two or more onsets (neg values) or two offsets (pos values) in a row; I deal with this starting at around line 36
      # where I skip any consecutive onsets/offsets
      # some consecutive onsets/offsets --detect repeating pos or neg numbers

      # Edge cases
      # Case 2: the data starts with a blink. In this case, blink onset will be defined as the first missing value.
      # if (length(blinks)>0 && blinks[1]>0 && pupil_data[1]==0)
      if (length(blinks) > 0 && blinks[1] > 0 && pupil_data[1] == 0) {
        blinks <- c(-1, blinks)
      }

      # Case 3: the data ends with a blink. In this case, blink offset will be defined as the last missing sample
      if (length(blinks) > 0 &&
        tail(blinks, 1) < 0 && tail(pupil_data, 1) == 0) {
        blinks <- c(blinks, nrow(pupil_data))
      }
      # Smoothing the data in order to increase the difference between the measurement noise and the eyelid signal.
      ms_4_smoothing <-
        10 # using a gap of 10 ms for the smoothing
      samples2smooth <-
        ceiling(ms_4_smoothing / sampling_interval) # amount of samples to smooth
      smooth_data <-
        ma(pupil_data, samples2smooth) # moving average smoothing #ma from forcast
      smooth_data[1] <- pupil_data[1]
      smooth_data[2] <- pupil_data[2]

      smooth_data[smooth_data == 0] <-
        NA
      # replace zeros with NaN values
      diff_smooth_data <- diff(smooth_data)


      # Finding the blinks' onset and offset
      blink <-
        1
      # initialize blink index for iteration
      blinks_data_positions <-
        matrix(0, length(blinks), 1) # initialize the array of blinks

      while (blink < length(blinks)) {
        # set the onsets candidate and check that it's the right sign; if the sign is incorrect (indicating duplicate offset or new onset),
        # then populate an NA in the data position frame and move to the next blink
        onset_candidate <- blinks[blink]
        while (onset_candidate > 0) {
          # indicating a double offset
          blinks_data_positions[blink] <-
            NA # populate NA in that position
          blink <-
            blink + 1 # skips that value and go to the next blink
          onset_candidate <-
            blinks[blink] # sets onset to subsequent value; try the next value to see if it's actually an onset
        }
        blink <-
          blink + 1 # increase the value to test for the offset


        # set the offset candidate and check that it's the right sign; if the sign is incorrect (indicating a duplicate onset instead of new offset),
        # then populate an NA and move to the next blink
        offset_candidate <- blinks[blink]
        if (offset_candidate > 0) {
          blink <- blink + 1
        } # increase the value for the next blink

        while (offset_candidate < 0) {
          # indicating a double onset (due to super short interval)
          blinks_data_positions[blink] <- NA
          blink <- blink + 1 # skips that value
          offset_candidate <-
            blinks[blink] # sets offset candidate to subsequent value
        }

        # find blink onset
        data_before <-
          diff_smooth_data[2:abs(onset_candidate)] # returns all the data before the candidate (from 2nd timestamp)

        blink_onset <-
          tail(which(data_before > 0), 1) # returns the last sample before the decline

        # Case 2 (the data starts with a blink. In this case, blink onset will be defined as the first missing value.)
        if (isempty(blink_onset == TRUE)) {
          ## CHECK THIS LOGIC - LH
          ifelse(
            onset_candidate == blinks[1],
            blink_onset <- 0,
            blink_onset <- -abs(onset_candidate)
          )
        }

        # correct the onset if we are not in case 2; isn't onset always neg?
        if (onset_candidate > 0 ||
          isTRUE(pupil_data[onset_candidate + 2, 1] > 0)) {
          blink_onset <- blink_onset + 2
        }

        # find blink offset
        data_after <-
          diff_smooth_data[abs(offset_candidate):length(diff_smooth_data)] # returns all data after the candidate
        blink_offset <-
          abs(offset_candidate) + head(which(data_after < 0), 1) # returns the last sample before the pupil increase

        # Case 3 (the data ends with a blink. In this case, blink offset will be defined as the last missing sample.)
        if (length(blink_offset) == 0) {
          blink_offset <- nrow(pupil_data) + 1
        }

        # insert the onset into the result array
        blinks_data_positions[blink - 2] <- blink_onset

        # insert the offset into the result array
        blinks_data_positions[blink - 1] <- blink_offset - 1
      }

      blinks_data_positions <- na.omit(blinks_data_positions)

      # removing duplicate values
      res <- blinks_data_positions
      id <- 1

      while (id < length(res) - 2) {
        if (res[id] > 0 && res[id] == res[id + 1]) {
          toremove <- matrix(TRUE, length(res), 1)

          toremove[id] <- FALSE
          toremove[id + 1] <- FALSE
          res <- res[toremove]
        } else {
          id <- id + 1
        }
      }


      # added by Eli (in MATLAB)
      ## Remove blink positions that are outside blink thresholds (min/max)
      non_blink_index <- {
      }
      # initialize non_blink_index
      for (i in 1:(length(res) / 2)) {
        gapLength <- res[2 * i] - abs(res[2 * i - 1])
        # P3-10: candidate gaps rejected for being too *long* (as opposed to
        # too short, which is much more commonly just measurement noise) are
        # the diagnostic-worthy case -- they may indicate tracking loss or an
        # extended eye closure rather than a genuine blink. Reporting only,
        # does not change which candidates get filtered below.
        if (isTRUE(verbose) && gapLength > blink_length_max) {
          .emit_diagnostic(
            paste0(
              "rows ~", abs(res[2 * i - 1]), "-", res[2 * i], ": candidate missing-data run (",
              gapLength, " sample(s)) for ", pupil_type,
              " exceeds maxBlinkLength_ms (", maxBlinkLength_ms,
              "ms) -- not classified as a blink; possible tracking loss or extended eye closure"
            ),
            verbose
          )
        }
        if (gapLength < blink_length_min |
          gapLength > blink_length_max) {
          non_blink_index <- rbind(non_blink_index, 2 * i - 1, 2 * i)
        }
      }
      res[non_blink_index] <- NA
      res <- na.omit(res)
    }
    ## Create final cleaned data array. Column 1 = blink start, Column 2 = blink end
    # and populate binary blink variable in data (1=blink, 0=no blink)
    blink_col <- as.data.frame(matrix(0, nrow(data), 1))
    if (x == 1) {
      colnames(blink_col) <-
        blinkCols[x] <- "pupilLeft.blink"
    } else if (x == 2) {
      colnames(blink_col) <-
        blinkCols[x] <- "pupilRight.blink"
    }

    blinks_indices <- matrix(0, length(res) / 2, 2)
    if (sum(!is.na(res) != 0)) {
      # blinks_indices = matrix(0,length(res)/2,2)
      for (i in 1:(length(res) / 2)) {
        blinks_indices[i, 1] <- res[2 * i - 1]
        blinks_indices[i, 2] <- res[2 * i]
        blink_col[blinks_indices[i, 1]:blinks_indices[i, 2], 1] <- 1
      }
      .emit_diagnostic(
        paste0(nrow(blinks_indices), " blink(s) detected for ", pupil_type),
        verbose
      )
    }
    # } else if (sum(!is.na(res) == 0)){
    # blinks_indices = matrix(0,length(res)/2,2)
    # }

    # save blink indices for later
    assign(paste0("bl_indices_eye", x), blinks_indices)

    # Save out cleaned pupil data (used to determine blinks)
    pupilCol <- paste0(pupil_type, ".clean")
    pupil_data[pupil_data == 0] <- NA
    data[pupilCol] <- as.data.frame(pupil_data)$V1
    # save blink assignment
    data <- cbind(data, blink_col)
  }
  # save blink assignments, across pupil columns
  if (length(pupil_data_types) == 2) {
    eye1 <- blinkCols[1]
    eye2 <- blinkCols[2]
    data$bothEyes.blink <-
      ifelse(data[[eye1]] == 1 & data[[eye2]] == 1, 1, 0)
    #
    #     #check if blink data was only available for one eye
    #     for (r in nrow(bl_indices_eye1)){
    #       if (sum(data[[eye2]][bl_indices_eye1[r,1]]:data[[eye2]][bl_indices_eye1[r,2]])) {
    #         data$bothEyes.blink[bl_indices_eye1[r,1]:bl_indices_eye1[r,2]] <- 1
    #       }
    #     }
    #     for (r in nrow(bl_indices_eye2)){
    #       if (sum(data[[eye1]][bl_indices_eye2[r,1]]:data[[eye1]][bl_indices_eye2[r,2]])) {
    #         data$bothEyes.blink[bl_indices_eye2[r,1]:bl_indices_eye2[r,2]] <- 1
    #       }
    #     }
    # remove composite blinks <100ms
    rle_blink_index <- which(rle(data$bothEyes.blink)$values == 1)
    end.bl <- cumsum(rle(data$bothEyes.blink)$lengths)
    start.bl <- c(1, lag(end.bl)[-1] + 1)
    lengths.bl <- end.bl - start.bl + 1
    short_bl_ind <-
      which(lengths.bl[rle_blink_index] <= blink_length_min)
    if (!isempty(short_bl_ind)) {
      for (bl in short_bl_ind) {
        data$bothEyes.blink[start.bl[rle_blink_index][bl]:end.bl[rle_blink_index][bl]] <-
          0
      }
    }
  } else if (length(pupil_data_types != 2)) {
    print(
      "You have input pupil data for more or less than two variables, so a composite blink column is not being calculated"
    )
  }
  removeCols <- colnames(data)[grepl("\\.clean$", colnames(data), ignore.case = TRUE)] # get intermediate cleaned pupil columns
  for (r in removeCols) {
    data[[r]] <- NULL
  }
  return(data)
}
