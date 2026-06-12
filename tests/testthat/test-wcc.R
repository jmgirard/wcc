test_that("r_to_z handles standard and extreme bounds", {
  # Correlation of 0 should equal a Z of 0
  expect_equal(r_to_z(0), 0)

  # The function clamps 1.0 at 0.99 to prevent infinite values
  expected_max <- 0.5 * log((1 + 0.99) / (1 - 0.99))
  expect_equal(r_to_z(1), expected_max)
  expect_equal(r_to_z(-1), -expected_max)
})

test_that("wcc returns correct structure and perfect correlation at lag 0", {
  # Create two identical sine waves
  x <- sin(seq(0, 4 * pi, length.out = 100))
  y <- x

  res <- wcc(x, y, window_size = 10, lag_max = 5)

  # Check object classes and structure
  expect_s3_class(res, "wcc_res")
  expect_type(res$fisher_z, "double")
  expect_true(is.data.frame(res$results_df))

  # At lag 0 for identical series, the correlation should be exactly 1
  lag_zero_res <- res$results_df |>
    dplyr::filter(tau == 0)

  # Use > 0.99 to account for minor floating point rounding differences
  expect_true(all(lag_zero_res$wcc > 0.99))
})

test_that("wcc handles missing values without crashing", {
  x <- runif(50)
  y <- runif(50)

  # Introduce missing values
  x[10:15] <- NA

  # Test with na.rm = TRUE
  res_na_rm <- wcc(x, y, window_size = 10, lag_max = 2, na.rm = TRUE)

  # It should successfully return a valid data frame and a numeric summary
  expect_true(is.data.frame(res_na_rm$results_df))
  expect_type(res_na_rm$fisher_z, "double")
})
