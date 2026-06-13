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

test_that("wcc_surrogate warns when sampling shifts with replacement", {
  # Create a short time series where valid_shifts will be small
  x <- 1:30
  y <- 1:30

  # lag_max = 5 means min_shift = 10.
  # max_shift = 30 - 10 = 20.
  # valid_shifts are 10:20 (only 11 available unique shifts).
  # Requesting 20 surrogates forces the function to sample with replacement.

  expect_warning(
    wcc_surrogate(x, y, window_size = 5, lag_max = 5, n_surrogates = 20),
    "Limited unique shifts available. Sampling with replacement."
  )
})

test_that("print.wcc_surr evaluates all logic branches without error", {
  # Helper function to generate mock objects to quickly test the print branches
  # without needing to run the expensive wcc_surrogate() calculation
  make_mock_surr <- function(p_val, n_surr) {
    structure(
      list(
        observed_z = 0.8,
        surrogate_z = runif(n_surr, 0, 0.5),
        p_value = p_val,
        n_surrogates = n_surr
      ),
      class = c("wcc_surr", "list")
    )
  }

  # Branch Test 1: p_value == 0, p_value < 0.05, n_surrogates < 1000
  mock_sig <- make_mock_surr(p_val = 0, n_surr = 100)
  expect_no_error(capture.output(print(mock_sig)))

  # Branch Test 2: p_value != 0, p_value >= 0.05, n_surrogates >= 1000
  mock_nonsig <- make_mock_surr(p_val = 0.25, n_surr = 1000)
  expect_no_error(capture.output(print(mock_nonsig)))
})
