# classifyGazeIVT() runs the full IVT (velocity-threshold) fixation/saccade
# classification pipeline: threshold velocity into fixation/saccade/missing,
# merge adjacent close fixations, drop short fixations, and index the final
# fixations/saccades. It returns a two-element list: [[1]] a summary
# data frame merged back onto the original input by recordingTimestamp_ms,
# [[2]] the full intermediate working data frame -- or list(data, NA) if no
# fixation ever survives classification. Expected values below were
# confirmed by direct execution.

test_that("classifyGazeIVT classifies a fixation/saccade/short-fixation sequence as expected", {
  # 100 Hz -> 10ms/sample. Layout:
  #   rows 1-2:   velocity NA, gaze NA -> "missing"
  #   rows 3-22:  velocity 10 (<= 50 threshold) -> fixation cluster of 20
  #               samples, far from the trailing cluster (no merge), long
  #               enough to survive removeShortFixations (20*3.3=66ms >= 60ms)
  #   rows 23-25: velocity 100 (> 50 threshold) -> saccade
  #   rows 26-28: velocity 10 -> a second, distant fixation cluster of only
  #               3 samples -- too short to survive (3*3.3=9.9ms < 60ms) and
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
