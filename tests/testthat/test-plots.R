library(testthat)
library(ggplot2)

# -------------------------------------------------------------------------
# Fixtures: Generate standard mock data for tests
# -------------------------------------------------------------------------

# Create simple sine waves for predictable plotting patterns
set.seed(42)
sig1 <- sin(seq(0, 4 * pi, length.out = 30))
sig2 <- sin(seq(0, 4 * pi, length.out = 30) + 0.5)
time_vec <- seq(0, 14.5, by = 0.5)

# Generate wcc and wdtw objects
mock_wcc <- wcc(sig1, sig2, window_size = 6, lag_max = 3)
mock_wdtw <- wdtw(sig1, sig2, window_size = 6, lag_max = 3)

# Generate time-mapped object for testing the 'has_time' logic
mock_wcc_time <- wcc(sig1, sig2, time = time_vec, window_size = 6, lag_max = 3)

# Generate optima objects
mock_wcc_optima <- pick_optima(mock_wcc, L_size = 3, search_method = "local")
mock_wdtw_optima <- pick_optima(mock_wdtw, search_method = "global")


# -------------------------------------------------------------------------
# Structural Tests
# -------------------------------------------------------------------------

test_that("plot.wcc_res handles logical arguments and layers correctly", {

  # Default plot
  p_default <- plot(mock_wcc)
  expect_s3_class(p_default, "ggplot")

  # A default plot with zero-lag line should have 2 layers (tile + vline)
  expect_length(p_default$layers, 2)
  expect_equal(p_default$labels$x, "Lag (\u03c4) Index")
  expect_equal(p_default$labels$y, "Elapsed Time Window Index")

  # Plot without zero-lag line
  p_no_line <- plot(mock_wcc, show_zero_lag = FALSE)
  expect_length(p_no_line$layers, 1)

  # Plot with time_step scaling
  p_scaled <- plot(mock_wcc, time_step = 2)
  expect_equal(p_scaled$labels$x, "Lag (\u03c4) in Seconds")
  expect_equal(p_scaled$labels$y, "Elapsed Time (Seconds)")

  # Check if data was scaled by time_step = 2
  expect_equal(max(p_scaled$data$tau), mock_wcc$settings$lag_max * 2)
})

test_that("plot.wdtw_res respects native time mapping from the object", {

  p_native_time <- plot(mock_wcc_time)

  # Because the object was built with a time vector, the y-axis label
  # should naturally say "Elapsed Time" without needing time_step
  expect_equal(p_native_time$labels$y, "Elapsed Time")

  # The data should already reflect the time vector mapping
  expect_true(all(p_native_time$data$i %in% time_vec))
})

test_that("plot_optima_overlay dynamically updates layers and titles", {

  # Base plot + vline (2 layers) + path (1) + points (1) = 4 total layers
  p_wcc_overlay <- plot_optima_overlay(mock_wcc, mock_wcc_optima)
  expect_length(p_wcc_overlay$layers, 4)
  expect_equal(p_wcc_overlay$labels$title, "Windowed Cross-Correlation with Optima Overlay")

  p_wdtw_overlay <- plot_optima_overlay(mock_wdtw, mock_wdtw_optima)
  expect_equal(p_wdtw_overlay$labels$title, "Windowed Dynamic Time Warping with Optima Overlay")

  # Check that time_step scales both the base plot and the overlay data correctly
  p_scaled_overlay <- plot_optima_overlay(mock_wcc, mock_wcc_optima, time_step = 0.5)

  # The optimum_lag in the overlay layer data should be exactly 0.5x the original optima values
  overlay_data <- p_scaled_overlay$layers[[3]]$data

  expect_equal(
    max(overlay_data$optimum_lag, na.rm = TRUE),
    max(mock_wcc_optima$optimum_lag, na.rm = TRUE) * 0.5
  )
})


# -------------------------------------------------------------------------
# Visual Regression Tests
# -------------------------------------------------------------------------

test_that("Visual outputs remain consistent", {
  skip_if_not_installed("vdiffr")

  # Test WCC base visual
  p_wcc <- plot(mock_wcc, color_low = "red", color_high = "blue")
  vdiffr::expect_doppelganger("wcc-plot-custom-colors", p_wcc)

  # Test WDTW base visual
  p_wdtw <- plot(mock_wdtw, zero_line_color = "red")
  vdiffr::expect_doppelganger("wdtw-plot-default", p_wdtw)

  # Test WCC overlay visual
  p_overlay <- plot_optima_overlay(mock_wcc, mock_wcc_optima, point_fill = "yellow")
  vdiffr::expect_doppelganger("wcc-optima-overlay", p_overlay)
})
