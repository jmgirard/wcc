# Helper function to quickly mock up a wcc_res object for testing
create_mock_wcc <- function(wcc_vals, tau_max, i_val = 100) {
  lags <- seq(-tau_max, tau_max)
  df <- data.frame(
    i = i_val,
    tau = lags,
    wcc = wcc_vals
  )

  obj <- list(
    results_df = df,
    settings = list(lag_max = tau_max)
  )
  class(obj) <- c("wcc_res", "list")
  obj
}

test_that("pick_optima validates inputs and handles errors properly", {
  mock_wcc <- create_mock_wcc(c(0.1, 0.5, 0.9, 0.5, 0.1), tau_max = 2)

  # Test class check
  expect_error(pick_optima(list(), search_method = "global"), "wcc_res or wdtw_res object")

  # Test even L_size C++ exception (requires local search)
  expect_error(pick_optima(mock_wcc, L_size = 4, search_method = "local"), "L_size must be an odd integer")

  # Test missing L_size for local search
  expect_error(pick_optima(mock_wcc, search_method = "local"), "An L_size must be provided")
})

test_that("pick_optima finds a perfect central peak (local search)", {
  # Peak exactly at lag 0
  mock_wcc <- create_mock_wcc(c(0.1, 0.5, 0.9, 0.5, 0.1), tau_max = 2)
  res <- pick_optima(mock_wcc, L_size = 3, search_method = "local")

  expect_equal(res$optimum_lag, 0)
  expect_equal(res$optimum_value, 0.9)
  expect_equal(res$i, 100)
})

test_that("pick_optima finds shifted off-center peaks (local search)", {
  # Peak at lag +1
  mock_pos <- create_mock_wcc(c(0.1, 0.2, 0.5, 0.8, 0.3), tau_max = 2)
  res_pos <- pick_optima(mock_pos, L_size = 3, search_method = "local")

  expect_equal(res_pos$optimum_lag, 1)
  expect_equal(res_pos$optimum_value, 0.8)

  # Peak at lag -2 (requires tau_max >= 3 so lag -3 exists for the flank)
  mock_neg <- create_mock_wcc(c(0.4, 0.9, 0.5, 0.2, 0.1, 0.1, 0.1), tau_max = 3)
  res_neg <- pick_optima(mock_neg, L_size = 3, search_method = "local")

  expect_equal(res_neg$optimum_lag, -2)
  expect_equal(res_neg$optimum_value, 0.9)
})

test_that("strict_monotonic toggle enforces flank constraints (local search)", {
  # Peak is at lag 0 (0.9).
  # Moving left from the peak, the values go from 0.5 to 0.7.
  # This violates the rule that values must strictly decrease away from the peak.
  mock_wcc <- create_mock_wcc(c(0.7, 0.5, 0.9, 0.4, 0.1), tau_max = 2)

  # With strict_monotonic = FALSE (default), it should find the local max
  res_loose <- pick_optima(mock_wcc, L_size = 5, strict_monotonic = FALSE, search_method = "local")
  expect_equal(res_loose$optimum_lag, 0)

  # With strict_monotonic = TRUE, it should reject this peak and return NAs
  res_strict <- pick_optima(mock_wcc, L_size = 5, strict_monotonic = TRUE, search_method = "local")
  expect_true(is.na(res_strict$optimum_lag))
  expect_true(is.na(res_strict$optimum_value))
})

test_that("pick_optima finds the correct absolute extremum (global search)", {
  # Create a WCC surface with two peaks, one higher than the other
  mock_wcc <- create_mock_wcc(c(0.2, 0.9, 0.4, 0.7, 0.1), tau_max = 2)

  # Global search should bypass L_size/flanks and just grab the absolute max
  res_global <- pick_optima(mock_wcc, search_method = "global")

  expect_equal(res_global$optimum_lag, -1)
  expect_equal(res_global$optimum_value, 0.9)
})

# Helper function to quickly mock up a wdtw_res object for testing
create_mock_wdtw <- function(dist_vals, tau_max, i_val = 100) {
  lags <- seq(-tau_max, tau_max)
  df <- data.frame(
    i = i_val,
    tau = lags,
    dtw_dist = dist_vals
  )

  obj <- list(
    results_df = df,
    settings = list(lag_max = tau_max)
  )
  class(obj) <- c("wdtw_res", "list")
  obj
}

test_that("print.wcc_optima produces expected console output", {
  mock_wcc <- create_mock_wcc(c(0.1, 0.5, 0.9, 0.5, 0.1), tau_max = 2)
  res <- pick_optima(mock_wcc, L_size = 3, search_method = "local")

  # This will capture the full cli header and the dataframe
  expect_snapshot(print(res))
})

test_that("summary.wcc_optima calculates and prints correctly", {
  df_valid <- data.frame(i = 100, tau = -2:2, wcc = c(0.1, 0.5, 0.9, 0.5, 0.1))
  df_invalid <- data.frame(i = 101, tau = -2:2, wcc = c(0.9, 0.7, 0.5, 0.4, 0.1))

  mock_wcc <- list(
    results_df = rbind(df_valid, df_invalid),
    settings = list(lag_max = 2)
  )
  class(mock_wcc) <- c("wcc_res", "list")

  res <- pick_optima(mock_wcc, L_size = 5, strict_monotonic = TRUE, search_method = "local")

  expect_snapshot(summary(res))
})

# Helper function to quickly mock up a wdtw_res object for testing
create_mock_wdtw <- function(dist_vals, tau_max, i_val = 100) {
  lags <- seq(-tau_max, tau_max)
  df <- data.frame(
    i = i_val,
    tau = lags,
    dtw_dist = dist_vals
  )

  obj <- list(
    results_df = df,
    settings = list(lag_max = tau_max)
  )
  class(obj) <- c("wdtw_res", "list")
  obj
}

test_that("print.wcc_optima produces expected console output", {
  mock_wcc <- create_mock_wcc(c(0.1, 0.5, 0.9, 0.5, 0.1), tau_max = 2)
  res <- pick_optima(mock_wcc, L_size = 3, search_method = "local")

  expect_snapshot(print(res))
})

test_that("summary.wcc_optima calculates and prints correctly", {
  # Create a scenario with one valid peak and one invalid peak
  df_valid <- data.frame(i = 100, tau = -2:2, wcc = c(0.1, 0.5, 0.9, 0.5, 0.1))
  df_invalid <- data.frame(i = 101, tau = -2:2, wcc = c(0.9, 0.7, 0.5, 0.4, 0.1))

  mock_wcc <- list(
    results_df = rbind(df_valid, df_invalid),
    settings = list(lag_max = 2)
  )
  class(mock_wcc) <- c("wcc_res", "list")

  res <- pick_optima(mock_wcc, L_size = 5, strict_monotonic = TRUE, search_method = "local")

  expect_snapshot(summary(res))
})

test_that("print.wdtw_optima produces expected console output", {
  mock_wdtw <- create_mock_wdtw(c(5.0, 3.2, 1.1, 3.2, 5.0), tau_max = 2)
  res <- pick_optima(mock_wdtw)

  expect_snapshot(print(res))
})

test_that("summary.wdtw_optima calculates and prints correctly", {
  # Mock a negative lag (y leads x) scenario
  mock_wdtw <- create_mock_wdtw(c(1.1, 3.2, 5.0, 6.1, 7.0), tau_max = 2)
  res <- pick_optima(mock_wdtw, search_method = "global")

  expect_snapshot(summary(res))
})

test_that("wcc accurately calculates correlations for identical and shifted series", {
  # Use a continuous sine wave to prevent zero-variance edge cases and boundary limits
  master <- sin(seq(0, 10, length.out = 50))

  # 1. Identical series test
  x_ident <- master[1:30]
  y_ident <- master[1:30]

  res_ident <- wcc(x_ident, y_ident, window_size = 10, lag_max = 3)
  df_ident <- res_ident$results_df

  # The correlation at lag 0 should be precisely 1 for all time windows
  lag_zero_wcc <- df_ident$wcc[df_ident$tau == 0]
  expect_equal(lag_zero_wcc, rep(1, length(lag_zero_wcc)))

  # 2. Shifted series test (y is delayed by 1 step relative to x)
  x_shift <- master[2:31]
  y_shift <- master[1:30]

  res_shift <- wcc(x_shift, y_shift, window_size = 10, lag_max = 3)
  opt_shift <- pick_optima(res_shift, search_method = "global")

  # The global peak for every window should be exactly 1 at a lag of magnitude 1.
  expect_equal(abs(opt_shift$optimum_lag), rep(1, nrow(opt_shift)))
  expect_equal(opt_shift$optimum_value, rep(1, nrow(opt_shift)))
})

test_that("wdtw accurately calculates distances for identical and shifted series", {
  master <- sin(seq(0, 10, length.out = 50))

  # 1. Identical series test
  x_ident <- master[1:30]
  y_ident <- master[1:30]

  # Set scale_data = FALSE to verify the absolute raw distance logic
  res_ident <- wdtw(x_ident, y_ident, window_size = 10, lag_max = 3, scale_data = FALSE)
  df_ident <- res_ident$results_df

  # The distance at lag 0 should be exactly 0 for all time windows
  lag_zero_dtw <- df_ident$dtw_dist[df_ident$tau == 0]
  expect_equal(lag_zero_dtw, rep(0, length(lag_zero_dtw)))

  # 2. Shifted series test (y is delayed by 1 step relative to x)
  x_shift <- master[2:31]
  y_shift <- master[1:30]

  res_shift <- wdtw(x_shift, y_shift, window_size = 10, lag_max = 3, scale_data = FALSE)
  opt_shift <- pick_optima(res_shift, search_method = "global")

  # The global minimum distance should be exactly 0 at a lag of magnitude 1
  expect_equal(abs(opt_shift$optimum_lag), rep(1, nrow(opt_shift)))
  expect_equal(opt_shift$optimum_value, rep(0, nrow(opt_shift)))
})
