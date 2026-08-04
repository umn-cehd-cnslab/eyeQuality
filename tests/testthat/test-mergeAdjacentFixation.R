# mergeAdjacentFixations() decides whether two temporally-adjacent proposed
# fixation clusters (separated by a short saccade run) should be merged into
# one fixation, based on the median-position Euclidean distance between them
# (mergeDistance_va) and the gap duration converted to ms via a fixed
# 3.3 ms/sample assumption (gap_dur * 3.3) -- NOT the recording's actual
# sampling interval. This is independent of recordingFrequency_hz, which
# classifyGazeIVT() does NOT pass down into this helper; expected values
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
