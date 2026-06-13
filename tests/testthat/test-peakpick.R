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

test_that("pick_peaks validates inputs and handles errors properly", {
  mock_wcc <- create_mock_wcc(c(0.1, 0.5, 0.9, 0.5, 0.1), tau_max = 2)

  # Test wcc_res class check
  expect_error(pick_peaks(list(), L_size = 3), "wcc_res or wdtw_res object")

  # Test even L_size C++ exception
  expect_error(pick_peaks(mock_wcc, L_size = 4), "L_size must be an odd integer")
})

test_that("pick_peaks finds a perfect central peak", {
  # Peak exactly at lag 0
  mock_wcc <- create_mock_wcc(c(0.1, 0.5, 0.9, 0.5, 0.1), tau_max = 2)
  res <- pick_peaks(mock_wcc, L_size = 3)

  expect_equal(res$peak_lag, 0)
  expect_equal(res$peak_value, 0.9)
  expect_equal(res$i, 100)
})

test_that("pick_peaks finds shifted off-center peaks", {
  # Peak at lag +1
  mock_pos <- create_mock_wcc(c(0.1, 0.2, 0.5, 0.8, 0.3), tau_max = 2)
  res_pos <- pick_peaks(mock_pos, L_size = 3)

  expect_equal(res_pos$peak_lag, 1)
  expect_equal(res_pos$peak_value, 0.8)

  # Peak at lag -2 (requires tau_max >= 3 so lag -3 exists for the flank)
  mock_neg <- create_mock_wcc(c(0.4, 0.9, 0.5, 0.2, 0.1, 0.1, 0.1), tau_max = 3)
  res_neg <- pick_peaks(mock_neg, L_size = 3)

  expect_equal(res_neg$peak_lag, -2)
  expect_equal(res_neg$peak_value, 0.9)
})

test_that("strict_monotonic toggle enforces flank constraints", {
  # Peak is at lag 0 (0.9).
  # Moving left from the peak, the values go from 0.5 to 0.7.
  # This violates the rule that values must strictly decrease away from the peak.
  # We use L_size = 5 so the window is wide enough to see the 0.7 anomaly.
  mock_wcc <- create_mock_wcc(c(0.7, 0.5, 0.9, 0.4, 0.1), tau_max = 2)

  # With strict_monotonic = FALSE (default), it should find the local max
  res_loose <- pick_peaks(mock_wcc, L_size = 5, strict_monotonic = FALSE)
  expect_equal(res_loose$peak_lag, 0)

  # With strict_monotonic = TRUE, it should reject this peak and return NAs
  res_strict <- pick_peaks(mock_wcc, L_size = 5, strict_monotonic = TRUE)
  expect_true(is.na(res_strict$peak_lag))
  expect_true(is.na(res_strict$peak_value))
})
