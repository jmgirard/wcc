# Tests for smooth_signal() -----------------------------------------------

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

test_that("smooth_signal catches invalid inputs", {
  # Main input validation
  expect_error(smooth_signal(c("a", "b", "c")), "must be a numeric vector")

  # Moving average validation
  expect_error(smooth_signal(1:10, method = "moving_average", window = -1), "positive integer")
  expect_error(smooth_signal(1:10, method = "moving_average", window = 2.5), "positive integer")

  # Savitzky-Golay validation
  expect_error(smooth_signal(1:10, method = "sgolay", window = 4), "odd integer")
  expect_error(smooth_signal(1:10, method = "sgolay", window = 5, sg_order = 5), "strictly less than")

  # Butterworth validation
  expect_error(smooth_signal(1:10, method = "butterworth", bw_cutoff = 1.5), "between 0 and 1")
  expect_error(smooth_signal(1:10, method = "butterworth", bw_cutoff = c(0.1, 0.2)), "single numeric value")
  expect_error(smooth_signal(1:10, method = "butterworth", bw_order = 0), "positive integer")
})


# Tests for aggregate_by_time() -------------------------------------------

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

test_that("aggregate_by_time handles na.rm correctly", {
  df <- data.frame(
    time = c(0.1, 0.2, 0.3),
    val = c(2, NA, 6)
  )

  # With na.rm = TRUE (default), the mean of 2 and 6 is 4
  agg_true <- aggregate_by_time(df, time_var = time, bin_width = 0.5, na.rm = TRUE)
  expect_equal(agg_true$val, 4)

  # With na.rm = FALSE, the mean of 2, NA, 6 is NA
  agg_false <- aggregate_by_time(df, time_var = time, bin_width = 0.5, na.rm = FALSE)
  expect_true(is.na(agg_false$val))
})

test_that("aggregate_by_time catches invalid inputs", {
  df <- data.frame(time = 1:10, val = 1:10)

  # Data structure and type errors
  expect_error(aggregate_by_time(list(a = 1), time_var = time, bin_width = 1), "must be a data frame")
  expect_error(aggregate_by_time(df, time_var = time, bin_width = -1), "single positive number")
  expect_error(aggregate_by_time(df, time_var = time, bin_width = c(1, 2)), "single positive number")
  expect_error(aggregate_by_time(df, time_var = time, bin_width = 1, na.rm = "TRUE"), "single logical value")
})


# Tests for trim_edges() --------------------------------------------------

test_that("trim_edges works on vectors, data frames, and matrices", {
  # Vector trimming
  v <- 1:10
  expect_equal(trim_edges(v, trim_length = 2), 3:8)

  # Data frame trimming
  df <- data.frame(a = 1:5, b = letters[1:5])
  df_trimmed <- trim_edges(df, trim_length = 1)
  expect_equal(nrow(df_trimmed), 3)
  expect_equal(df_trimmed$a, 2:4)

  # Matrix trimming
  mat <- matrix(1:20, nrow = 10)
  mat_trimmed <- trim_edges(mat, trim_length = 3)
  expect_equal(nrow(mat_trimmed), 4)
})

test_that("trim_edges catches invalid inputs and extreme trims", {
  v <- 1:5
  df <- data.frame(a = 1:5)

  # Invalid argument types
  expect_error(trim_edges(v, trim_length = -1), "single positive integer")
  expect_error(trim_edges(v, trim_length = 1.5), "single positive integer")
  expect_error(trim_edges(list(a = 1:5), trim_length = 1), "vector, matrix, or data frame")

  # Edge case where trim_length exceeds data dimensions
  expect_error(trim_edges(v, trim_length = 3), "too large; it would remove all elements")
  expect_error(trim_edges(df, trim_length = 3), "too large; it would remove all rows")
})
