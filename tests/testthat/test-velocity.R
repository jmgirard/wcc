test_that("v_xyz calculates correct 3D velocity", {
  t <- 1:5
  x <- c(0, 10, 20, 30, 40) # Constant velocity of 10 in x direction
  y <- rep(0, 5)
  z <- rep(0, 5)

  v <- v_xyz(t, x, y, z)

  # The first and last elements should be NA due to lag/lead boundary limits
  expect_true(is.na(v[1]))
  expect_true(is.na(v[5]))

  # Middle elements should all exactly equal 10
  expect_equal(v[2:4], c(10, 10, 10))
})
