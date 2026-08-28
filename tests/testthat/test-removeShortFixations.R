# removeShortFixations() reclassifies a proposed fixation cluster as
# "unclassified" if its duration, converted to ms via the recording's actual
# sample-to-sample interval (fixationLength * sampling_interval), falls under
# shortFixationThreshold_ms. sampling_interval defaults to 3.3 (a ~303 Hz
# recording) when not supplied, but classifyGazeIVT() passes the recording's
# real sampling_interval (1000 / recordingFrequency_hz) through to this
# helper. Expected values confirmed by direct execution.

test_that("removeShortFixations keeps a long fixation and unclassifies a short one", {
  # cluster 1: 20 samples -> 20*3.3 = 66ms >= 60ms threshold -> kept
  # cluster 2: 5 samples -> 5*3.3 = 16.5ms < 60ms threshold -> unclassified
  entryClass <- c(rep("fixation", 20), rep("saccade", 3), rep("fixation", 5))
  idx <- findFixationIndices(entryClass)
  data <- data.frame(
    class.adj = entryClass,
    class.adj.euc = NA,
    class.adj.gap.dur = NA,
    class.adj.xva = NA,
    class.adj.yva = NA,
    class.adj.num = NA,
    class.adj.dur = NA
  )

  result <- removeShortFixations(
    data,
    entryClass = entryClass,
    rleFixIndex = idx$rle_fix_index,
    fixationEnd = idx$end.fix,
    fixationStart = idx$start.fix,
    fixationLength = idx$lengths.fix,
    shortFixationThreshold_ms = 60
  )

  expect_true(all(result$class.adj.shortfix[1:20] == "fixation"))
  expect_true(all(result$class.adj.shortfix[21:23] == "saccade"))
  expect_true(all(result$class.adj.shortfix[24:28] == "unclassified"))
})

test_that("removeShortFixations nulls out the fixation metric columns for a removed short fixation", {
  entryClass <- c(rep("fixation", 20), rep("saccade", 3), rep("fixation", 5))
  idx <- findFixationIndices(entryClass)
  data <- data.frame(
    class.adj = entryClass,
    class.adj.euc = 1,
    class.adj.gap.dur = 1,
    class.adj.xva = 1,
    class.adj.yva = 1,
    class.adj.num = 1,
    class.adj.dur = 1
  )

  result <- removeShortFixations(
    data,
    entryClass = entryClass,
    rleFixIndex = idx$rle_fix_index,
    fixationEnd = idx$end.fix,
    fixationStart = idx$start.fix,
    fixationLength = idx$lengths.fix,
    shortFixationThreshold_ms = 60
  )

  expect_true(all(is.na(result$class.adj.xva[24:28])))
  expect_true(all(is.na(result$class.adj.num[24:28])))
  expect_true(all(!is.na(result$class.adj.xva[1:20])))
})

test_that("removeShortFixations uses the recording's actual sampling_interval, not a hardcoded 3.3", {
  # cluster 2 is 5 samples long. Whether it is "too short" to keep depends
  # entirely on the recording's real sampling rate:
  #   - at the old hardcoded 3.3 ms/sample assumption: 5 * 3.3 = 16.5ms,
  #     under the 60ms shortFixationThreshold_ms -> gets unclassified
  #   - at a real ~60 Hz recording (sampling_interval ~16.667 ms/sample):
  #     5 * 16.667 = 83.335ms, over the 60ms threshold -> must be kept
  # This is the same raw fixation-length data in both cases -- only
  # sampling_interval differs -- so the differing outcome demonstrates the fix.
  entryClass <- c(rep("fixation", 20), rep("saccade", 3), rep("fixation", 5))
  idx <- findFixationIndices(entryClass)
  data <- data.frame(
    class.adj = entryClass,
    class.adj.euc = NA,
    class.adj.gap.dur = NA,
    class.adj.xva = NA,
    class.adj.yva = NA,
    class.adj.num = NA,
    class.adj.dur = NA
  )

  # default sampling_interval (3.3) preserves the old ~303 Hz-assumed behavior
  result_default <- removeShortFixations(
    data,
    entryClass = entryClass,
    rleFixIndex = idx$rle_fix_index,
    fixationEnd = idx$end.fix,
    fixationStart = idx$start.fix,
    fixationLength = idx$lengths.fix,
    shortFixationThreshold_ms = 60
  )
  expect_true(all(result_default$class.adj.shortfix[24:28] == "unclassified"))

  # a real ~60 Hz sampling_interval correctly keeps the 5-sample fixation
  sampling_interval_60hz <- round(1000 / 60, 3)
  result_60hz <- removeShortFixations(
    data,
    entryClass = entryClass,
    rleFixIndex = idx$rle_fix_index,
    fixationEnd = idx$end.fix,
    fixationStart = idx$start.fix,
    fixationLength = idx$lengths.fix,
    shortFixationThreshold_ms = 60,
    sampling_interval = sampling_interval_60hz
  )
  expect_true(all(result_60hz$class.adj.shortfix[24:28] == "fixation"))
})
