test_that("wcc_surrogate returns a valid wcc_surr object", {
  # Generate simple dummy data
  t <- seq(0, 10, length.out = 100)
  x <- sin(t)
  y <- sin(t + 0.5)

  # Run with a small number of surrogates to keep the test fast
  res <- wcc_surrogate(
    x = x,
    y = y,
    window_size = 20,
    lag_max = 5,
    n_surrogates = 10
  )

  # Check class and structure
  expect_s3_class(res, "wcc_surr")
  expect_type(res, "list")

  # Check specific list elements
  expect_true(is.numeric(res$observed_z))
  expect_true(is.numeric(res$surrogate_z))
  expect_equal(length(res$surrogate_z), 10)
  expect_equal(res$n_surrogates, 10)

  # Check p-value bounds
  expect_true(res$p_value >= 0 && res$p_value <= 1)
})

test_that("wcc_surrogate throws an error if time series is too short", {
  x <- 1:15
  y <- 1:15

  # lag_max * 2 = 20, which is larger than the series length (15)
  # This should trigger the minimum shift error
  expect_error(
    wcc_surrogate(x, y, window_size = 5, lag_max = 10, n_surrogates = 5),
    "Time series is too short relative to lag_max"
  )
})

test_that("wcc_surrogate results are reproducible with set.seed", {
  x <- rnorm(100)
  y <- rnorm(100)

  set.seed(42)
  res1 <- wcc_surrogate(x, y, window_size = 20, lag_max = 10, n_surrogates = 15)

  set.seed(42)
  res2 <- wcc_surrogate(x, y, window_size = 20, lag_max = 10, n_surrogates = 15)

  # The exact same seeds should produce the exact same distribution and p-value
  expect_equal(res1$surrogate_z, res2$surrogate_z)
  expect_equal(res1$p_value, res2$p_value)
})
