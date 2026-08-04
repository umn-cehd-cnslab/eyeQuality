# classifyBlinks() flags NA runs in pupil data as blinks when their duration
# falls within [minBlinkLength_ms, maxBlinkLength_ms]. These synthetic
# fixtures use a smoothly oscillating (non-flat) pupil baseline, since a flat
# baseline defeats the onset/offset detection step (it walks outward from a
# candidate gap looking for the nearest real rise/decline in the smoothed
# signal). Expected flagged ranges were confirmed by running classifyBlinks()
# directly against these fixtures -- the algorithm's onset/offset correction
# steps shift the flagged span a few samples outward from the raw NA run, so
# exact indices below reflect actual output, not just the raw gap boundaries.

test_that("classifyBlinks flags a 200ms NA gap (within min/max blink length) as a blink", {
  n <- 60
  baseline <- round(5 + sin(2 * pi * (1:n) / 20), 2)
  pupil <- baseline
  pupil[26:45] <- NA # 20 samples = 200ms at 100Hz, within [100, 400]ms

  data <- data.frame(pupilLeft.int = pupil, pupilRight.int = pupil)

  result <- classifyBlinks(
    data,
    pupilLeft = "pupilLeft.int",
    pupilRight = "pupilRight.int",
    recordingFrequency_hz = 100
  )

  expect_true(all(result$pupilLeft.blink[23:46] == 1))
  expect_true(all(result$pupilLeft.blink[1:22] == 0))
  expect_true(all(result$pupilLeft.blink[47:60] == 0))
  expect_true(all(result$bothEyes.blink[23:46] == 1))
})

test_that("classifyBlinks does not flag a 600ms NA dropout (beyond maxBlinkLength_ms) as a blink", {
  n <- 100
  baseline <- round(5 + sin(2 * pi * (1:n) / 20), 2)
  pupil <- baseline
  pupil[26:85] <- NA # 60 samples = 600ms at 100Hz, beyond the default 400ms max

  data <- data.frame(pupilLeft.int = pupil, pupilRight.int = pupil)

  result <- classifyBlinks(
    data,
    pupilLeft = "pupilLeft.int",
    pupilRight = "pupilRight.int",
    recordingFrequency_hz = 100
  )

  expect_true(all(result$pupilLeft.blink == 0))
  expect_true(all(result$bothEyes.blink == 0))
})
