test_that("aggregate_by_time correctly downsamples and centers time", {
  # Simulating 30Hz data (roughly 0.033s intervals)
  df <- data.frame(
    time = c(0.000, 0.033, 0.066, 0.100, 0.133, 0.166),
    au12 = c(2, 4, 6, 8, 10, 12),
    character_col = c("a", "b", "c", "d", "e", "f")
  )

  # Aggregate to 0.1s bins
  agg_df <- aggregate_by_time(df, time_var = time, bin_width = 0.1)

  # The non-numeric column should be dropped
  expect_false("character_col" %in% names(agg_df))

  # Bin 1 (0 to <0.1): elements 1, 2, 3 (mean of 2, 4, 6 is 4)
  # Center of Bin 1 is 0.05
  # Bin 2 (0.1 to <0.2): elements 4, 5, 6 (mean of 8, 10, 12 is 10)
  # Center of Bin 2 is 0.15
  expect_equal(nrow(agg_df), 2)
  expect_equal(agg_df$time, c(0.05, 0.15))
  expect_equal(agg_df$au12, c(4, 10))
})

test_that("smooth_signal moving_average works correctly", {
  x <- c(2, 4, 6, 8, 10)
  # A window of 3 centered on the middle elements
  smoothed <- smooth_signal(x, method = "moving_average", window = 3)

  expect_true(is.na(smoothed[1]))
  expect_true(is.na(smoothed[5]))
  expect_equal(smoothed[2], mean(c(2, 4, 6)))
  expect_equal(smoothed[3], mean(c(4, 6, 8)))
})

test_that("smooth_signal sgolay and butterworth execute if signal is installed", {
  # Gracefully skip these tests if the user doesn't have 'signal' installed
  skip_if_not_installed("signal")

  x <- sin(seq(0, 2 * pi, length.out = 100)) + rnorm(100, 0, 0.1)

  sg_smoothed <- smooth_signal(x, method = "sgolay", window = 5, sg_order = 3)
  expect_equal(length(sg_smoothed), 100)
  expect_false(any(is.na(sg_smoothed)))

  bw_smoothed <- smooth_signal(x, method = "butterworth", bw_cutoff = 0.2)
  expect_equal(length(bw_smoothed), 100)
  expect_false(any(is.na(bw_smoothed)))
})
