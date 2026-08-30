# classifyGazeIVT() runs the full IVT (velocity-threshold) fixation/saccade
# classification pipeline: threshold velocity into fixation/saccade/missing,
# merge adjacent close fixations, drop short fixations, and index the final
# fixations/saccades. It returns a two-element list: [[1]] a summary
# data frame merged back onto the original input by recordingTimestamp_ms,
# [[2]] the full intermediate working data frame -- or list(data, NA) if no
# fixation ever survives classification. Expected values below were
# confirmed by direct execution.

test_that("classifyGazeIVT classifies a fixation/saccade/short-fixation sequence as expected", {
  # 100 Hz -> 10ms/sample (sampling_interval = 1000/100 = 10). Layout:
  #   rows 1-2:   velocity NA, gaze NA -> "missing"
  #   rows 3-22:  velocity 10 (<= 50 threshold) -> fixation cluster of 20
  #               samples, far from the trailing cluster (no merge), long
  #               enough to survive removeShortFixations (20*10=200ms >= 60ms)
  #   rows 23-25: velocity 100 (> 50 threshold) -> saccade
  #   rows 26-28: velocity 10 -> a second, distant fixation cluster of only
  #               3 samples -- too short to survive (3*10=30ms < 60ms) and
  #               gets reclassified "unclassified"
  n <- 28
  velocity <- c(NA, NA, rep(10, 20), rep(100, 3), rep(10, 3))
  gazeX_va <- c(NA, NA, rep(0, 20), rep(0, 3), rep(10, 3))
  gazeY_va <- c(NA, NA, rep(0, 20), rep(0, 3), rep(10, 3))
  recordingTimestamp_ms <- seq(0, by = 10, length.out = n)

  data <- data.frame(
    recordingTimestamp_ms = recordingTimestamp_ms,
    velocity = velocity,
    gazeX_va = gazeX_va,
    gazeY_va = gazeY_va
  )

  result <- classifyGazeIVT(
    data,
    velocity = "velocity",
    gazeX_va = "gazeX_va",
    gazeY_va = "gazeY_va",
    recordingFrequency_hz = 100,
    fixationVelocityThreshold = 50,
    maxAdjacentFixationAngle = 0.5,
    maxAdjacentFixationTime = 75,
    minFixationDuration = 60
  )

  expect_length(result, 2)
  summary <- result[[1]]

  expect_equal(
    summary$IVT.classification,
    c(rep("missing", 2), rep("fixation", 20), rep("saccade", 3), rep("unclassified", 3))
  )
  expect_equal(summary$IVT.fixationIndex[3:22], rep(1, 20))
  expect_true(all(is.na(summary$IVT.fixationIndex[c(1:2, 23:28)])))
  expect_equal(summary$IVT.saccadeIndex[23:25], rep(1, 3))
  # fixation duration in ms = sample count * sampling interval (10ms)
  expect_equal(unique(summary$IVT.fixationDuration_ms[3:22]), 200)
})

test_that("classifyGazeIVT passes the recording's actual sampling_interval down to mergeAdjacentFixations/removeShortFixations", {
  # 60 Hz -> sampling_interval = round(1000/60, 3) = 16.667ms/sample. Layout:
  #   row 1:      velocity NA, gaze NA -> "missing"
  #   rows 2-21:  velocity 10 -> fixation cluster of 20 samples at (0,0)
  #   rows 22-24: velocity 100 -> saccade
  #   rows 25-29: velocity 10 -> a second, distant (gaze (10,10), so never
  #               merge-eligible) fixation cluster of only 5 samples
  #
  # Before the fix, removeShortFixations() always used a hardcoded 3.3
  # ms/sample assumption regardless of recordingFrequency_hz: 5 * 3.3 =
  # 16.5ms < 60ms minFixationDuration, so this 5-sample cluster would have
  # been wrongly dropped ("unclassified") even though it is a genuine ~83ms
  # fixation at the recording's real 60 Hz rate. With the fix,
  # classifyGazeIVT() passes its already-computed sampling_interval through:
  # 5 * 16.667 = 83.335ms >= 60ms, so the cluster correctly survives as a
  # second fixation.
  n <- 29
  sampling_interval_60hz <- round(1000 / 60, 3)
  velocity <- c(NA, rep(10, 20), rep(100, 3), rep(10, 5))
  gazeX_va <- c(NA, rep(0, 20), rep(0, 3), rep(10, 5))
  gazeY_va <- c(NA, rep(0, 20), rep(0, 3), rep(10, 5))
  recordingTimestamp_ms <- round(seq(0, by = sampling_interval_60hz, length.out = n), 3)

  data <- data.frame(
    recordingTimestamp_ms = recordingTimestamp_ms,
    velocity = velocity,
    gazeX_va = gazeX_va,
    gazeY_va = gazeY_va
  )

  result <- classifyGazeIVT(
    data,
    velocity = "velocity",
    gazeX_va = "gazeX_va",
    gazeY_va = "gazeY_va",
    recordingFrequency_hz = 60,
    fixationVelocityThreshold = 50,
    maxAdjacentFixationAngle = 0.5,
    maxAdjacentFixationTime = 75,
    minFixationDuration = 60
  )

  summary <- result[[1]]

  expect_equal(
    summary$IVT.classification,
    c("missing", rep("fixation", 20), rep("saccade", 3), rep("fixation", 5))
  )
  expect_equal(summary$IVT.fixationIndex[2:21], rep(1, 20))
  expect_equal(summary$IVT.fixationIndex[25:29], rep(2, 5))
  expect_true(all(is.na(summary$IVT.fixationIndex[c(1, 22:24)])))
  expect_equal(summary$IVT.saccadeIndex[22:24], rep(1, 3))
  expect_equal(unique(summary$IVT.fixationDuration_ms[2:21]), 20 * sampling_interval_60hz)
  expect_equal(unique(summary$IVT.fixationDuration_ms[25:29]), 5 * sampling_interval_60hz)
})

test_that("classifyGazeIVT returns list(data, NA) when no fixation ever survives classification", {
  # every valid sample is above the saccade threshold -- no fixation-eligible
  # sample exists at all
  data <- data.frame(
    recordingTimestamp_ms = seq(0, by = 10, length.out = 5),
    velocity = c(NA, 100, 100, 100, NA),
    gazeX_va = c(NA, 0, 1, 2, NA),
    gazeY_va = c(NA, 0, 1, 2, NA)
  )

  result <- classifyGazeIVT(
    data,
    velocity = "velocity",
    gazeX_va = "gazeX_va",
    gazeY_va = "gazeY_va",
    recordingFrequency_hz = 100
  )

  expect_length(result, 2)
  expect_true(is.na(result[[2]]))
  expect_equal(
    result[[1]]$IVT.classification,
    c("missing", "saccade", "saccade", "saccade", "missing")
  )
  expect_true(all(is.na(result[[1]]$fix.ind)))
})
