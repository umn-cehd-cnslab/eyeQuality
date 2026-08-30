# mergeAdjacentFixations() decides whether two temporally-adjacent proposed
# fixation clusters (separated by a short saccade run) should be merged into
# one fixation, based on the median-position Euclidean distance between them
# (mergeDistance_va) and the gap duration converted to ms via the recording's
# actual sample-to-sample interval (gap_dur * sampling_interval).
# sampling_interval defaults to 3.3 (a ~303 Hz recording) when not supplied,
# but classifyGazeIVT() passes the recording's real sampling_interval
# (1000 / recordingFrequency_hz) through to this helper. Expected values
# below were confirmed by direct execution.

fixtureIndices <- function(entryClass) {
  findFixationIndices(entryClass)
}

test_that("mergeAdjacentFixations merges two nearby fixation clusters across a short saccade gap", {
  # 5 fixation samples, 2 saccade samples, 5 fixation samples
  entryClass <- c(rep("fixation", 5), rep("saccade", 2), rep("fixation", 5))
  # second cluster is close (0.1, 0.1) to the first (0, 0):
  # euclidean distance = sqrt(0.1^2 + 0.1^2) = 0.1414214, well under the
  # default mergeDistance_va = 0.5; gap_dur = 2 samples -> 2*3.3 = 6.6ms,
  # well under the default mergeTimeGap_ms = 75
  gazeX <- c(rep(0, 5), NA, NA, rep(0.1, 5))
  gazeY <- c(rep(0, 5), NA, NA, rep(0.1, 5))

  idx <- fixtureIndices(entryClass)
  data <- data.frame(class = entryClass)

  result <- mergeAdjacentFixations(
    data,
    entryClass = entryClass,
    rleFixIndex = idx$rle_fix_index,
    gazeX = gazeX,
    gazeY = gazeY,
    fixationEnd = idx$end.fix,
    fixationStart = idx$start.fix,
    fixationLength = idx$lengths.fix,
    mergeDistance_va = 0.5,
    mergeTimeGap_ms = 75
  )

  # the saccade gap (indices 6:7) is absorbed into a single 12-sample fixation
  expect_true(all(result$class.adj == "fixation"))
  expect_equal(unique(round(result$class.adj.euc, 7)), 0.1414214)
  expect_equal(unique(result$class.adj.gap.dur), 2)
})

test_that("mergeAdjacentFixations does not merge two distant fixation clusters", {
  entryClass <- c(rep("fixation", 5), rep("saccade", 2), rep("fixation", 5))
  # second cluster is far (5, 0) from the first (0, 0):
  # euclidean distance = 5, well over the default mergeDistance_va = 0.5
  gazeX <- c(rep(0, 5), NA, NA, rep(5, 5))
  gazeY <- c(rep(0, 5), NA, NA, rep(0, 5))

  idx <- fixtureIndices(entryClass)
  data <- data.frame(class = entryClass)

  result <- mergeAdjacentFixations(
    data,
    entryClass = entryClass,
    rleFixIndex = idx$rle_fix_index,
    gazeX = gazeX,
    gazeY = gazeY,
    fixationEnd = idx$end.fix,
    fixationStart = idx$start.fix,
    fixationLength = idx$lengths.fix,
    mergeDistance_va = 0.5,
    mergeTimeGap_ms = 75
  )

  # the saccade gap (indices 6:7) stays "saccade" -- clusters were not merged
  expect_equal(result$class.adj, entryClass)
  expect_equal(unique(na.omit(result$class.adj.euc)), 5)
})

test_that("mergeAdjacentFixations uses the recording's actual sampling_interval, not a hardcoded 3.3", {
  # Two spatially-close fixation clusters (euclidean distance well under
  # mergeDistance_va) separated by a 10-sample saccade gap. Whether the gap
  # is "short enough" to merge across depends entirely on the recording's
  # real sampling rate:
  #   - at the old hardcoded 3.3 ms/sample assumption: 10 * 3.3 = 33ms,
  #     under the 75ms mergeTimeGap_ms threshold -> clusters get merged
  #   - at a real ~60 Hz recording (sampling_interval ~16.667 ms/sample):
  #     10 * 16.667 = 166.67ms, over the 75ms threshold -> clusters must
  #     NOT be merged
  # This is the same raw gaze data in both cases -- only sampling_interval
  # differs -- so the differing outcome demonstrates the fix.
  entryClass <- c(rep("fixation", 5), rep("saccade", 10), rep("fixation", 5))
  gazeX <- c(rep(0, 5), rep(NA, 10), rep(0.1, 5))
  gazeY <- c(rep(0, 5), rep(NA, 10), rep(0.1, 5))
  idx <- fixtureIndices(entryClass)

  # default sampling_interval (3.3) preserves the old ~303 Hz-assumed behavior
  data_default <- data.frame(class = entryClass)
  result_default <- mergeAdjacentFixations(
    data_default,
    entryClass = entryClass,
    rleFixIndex = idx$rle_fix_index,
    gazeX = gazeX,
    gazeY = gazeY,
    fixationEnd = idx$end.fix,
    fixationStart = idx$start.fix,
    fixationLength = idx$lengths.fix,
    mergeDistance_va = 0.5,
    mergeTimeGap_ms = 75
  )
  expect_true(all(result_default$class.adj == "fixation"))

  # a real ~60 Hz sampling_interval correctly keeps the gap unmerged
  sampling_interval_60hz <- round(1000 / 60, 3)
  data_60hz <- data.frame(class = entryClass)
  result_60hz <- mergeAdjacentFixations(
    data_60hz,
    entryClass = entryClass,
    rleFixIndex = idx$rle_fix_index,
    gazeX = gazeX,
    gazeY = gazeY,
    fixationEnd = idx$end.fix,
    fixationStart = idx$start.fix,
    fixationLength = idx$lengths.fix,
    mergeDistance_va = 0.5,
    mergeTimeGap_ms = 75,
    sampling_interval = sampling_interval_60hz
  )
  expect_equal(result_60hz$class.adj, entryClass)
  expect_true(all(result_60hz$class.adj[6:15] == "saccade"))
})
