# Setup some simple signals for testing
set.seed(42)
n_len <- 100
t <- seq(0, 4 * pi, length.out = n_len)
sig1 <- sin(t)
# sig2 is identical to sig1 but shifted by 5 indices
sig2 <- dplyr::lag(sig1, n = 5, default = 0)

test_that("wdtw returns expected structure and classes", {
  res <- wdtw(
    x = sig1,
    y = sig2,
    window_size = 20,
    lag_max = 10,
    window_increment = 5,
    lag_increment = 1,
    scale_data = FALSE
  )

  expect_s3_class(res, "wdtw_res")
  expect_type(res$settings, "list")
  expect_type(res$mean_distance, "double")
  expect_s3_class(res$results_df, "data.frame")

  # Check expected columns
  expect_true(all(c("row", "col", "i", "tau", "dtw_dist") %in% names(res$results_df)))

  # Check basic dimensions based on the parameters
  expected_lags <- length(seq(-10, 10, by = 1))
  expect_equal(length(unique(res$results_df$tau)), expected_lags)
})

test_that("wdtw correctly scales data when scale_data = TRUE", {
  # Create a drastically scaled version of sig1
  sig1_large <- sig1 * 1000

  res_unscaled <- wdtw(sig1, sig1_large, window_size = 10, lag_max = 5, scale_data = FALSE)
  res_scaled <- wdtw(sig1, sig1_large, window_size = 10, lag_max = 5, scale_data = TRUE)

  # Scaled distances should be drastically smaller than unscaled distances for these vectors
  expect_true(res_scaled$mean_distance < res_unscaled$mean_distance)
})

test_that("wdtw logic correctly identifies perfect alignment", {
  # Comparing sig1 to itself should yield a minimum distance of 0 at lag 0
  res <- wdtw(
    x = sig1,
    y = sig1,
    window_size = 15,
    lag_max = 5,
    scale_data = FALSE # Keep false to avoid floating point precision issues near 0
  )

  # Find the row with the minimum overall distance
  min_idx <- which.min(res$results_df$dtw_dist)
  best_lag <- res$results_df$tau[min_idx]
  min_dist <- res$results_df$dtw_dist[min_idx]

  expect_equal(best_lag, 0)
  expect_equal(min_dist, 0)
})

test_that("S3 methods for wdtw_res work without error", {
  res <- wdtw(sig1, sig2, window_size = 20, lag_max = 10)

  # Expect no errors when printing or plotting
  expect_no_error(capture.output(print(res)))
  expect_no_error(p <- plot(res))
  expect_s3_class(p, "ggplot")
})

test_that("pick_optima handles wdtw_res correctly across search methods", {
  res <- wdtw(sig1, sig1, window_size = 15, lag_max = 5, scale_data = FALSE)

  # 1. Test default global search for wdtw
  # It should automatically infer search_method = "global" and find_min = TRUE
  optima_global <- pick_optima(res)

  expect_s3_class(optima_global, "wdtw_optima")
  expect_equal(attr(optima_global, "search_method"), "global")
  expect_true(attr(optima_global, "find_min"))

  # For perfectly aligned identical signals, the absolute minimum must be at lag 0
  expect_true(all(optima_global$optimum_lag == 0, na.rm = TRUE))

  # 2. Test explicit local search with strict monotonicity
  optima_local <- pick_optima(res, search_method = "local", L_size = 3, strict_monotonic = TRUE)

  expect_s3_class(optima_local, "wdtw_optima")
  expect_equal(attr(optima_local, "search_method"), "local")
  expect_true(attr(optima_local, "find_min"))

  # The local valley search should also successfully identify lag 0
  expect_true(all(optima_local$optimum_lag == 0, na.rm = TRUE))
})

test_that("wdtw handles out-of-bounds cleanly by returning NA", {
  res <- wdtw(sig1, sig2, window_size = 90, lag_max = 20)

  # Because window_size + lag_max > length(sig), many edge windows will fail
  # the C++ bounds check and should cleanly return NA rather than crashing
  expect_true(any(is.na(res$results_df$dtw_dist)))
})
