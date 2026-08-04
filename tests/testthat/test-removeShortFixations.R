# removeShortFixations() reclassifies a proposed fixation cluster as
# "unclassified" if its duration, converted to ms via a fixed 3.3 ms/sample
# assumption (fixationLength * 3.3), falls under shortFixationThreshold_ms.
# Like mergeAdjacentFixations(), this 3.3 factor is independent of the
# recording's actual sampling rate. Expected values confirmed by direct
# execution.

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
