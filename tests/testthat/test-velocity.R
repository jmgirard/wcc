test_that("calc_velocity_1d preserves direction and handles edges", {
  t <- 1:5
  # Moves forward to 20, then backward to 0
  x <- c(0, 10, 20, 10, 0)

  # Without edge filling (central difference only)
  v_no_edges <- calc_velocity_1d(t, x, fill_edges = FALSE)
  expect_true(is.na(v_no_edges[1]))
  expect_true(is.na(v_no_edges[5]))
  expect_equal(v_no_edges[2:4], c(10, 0, -10))

  # With edge filling (should use forward at 1 and backward at 5)
  v_edges <- calc_velocity_1d(t, x, fill_edges = TRUE)
  expect_equal(v_edges, c(10, 10, 0, -10, -10))
})

test_that("calc_speed_1d calculates absolute magnitude", {
  t <- 1:5
  x <- c(0, 10, 20, 10, 0)

  s_edges <- calc_speed_1d(t, x, fill_edges = TRUE)
  # Speed should always be positive, unlike velocity
  expect_equal(s_edges, c(10, 10, 0, 10, 10))
})

test_that("calc_speed_2d calculates correct 2D hypotenuse", {
  t <- 1:5
  # Creating a 3-4-5 triangle scenario for easy math
  x <- c(0, 3, 6, 9, 12)
  y <- c(0, 4, 8, 12, 16)

  s <- calc_speed_2d(t, x, y, fill_edges = TRUE)
  # The 2D speed should be exactly 5 at all points
  expect_equal(s, rep(5, 5))
})

test_that("calc_speed_3d calculates correct 3D velocity", {
  t <- 1:5
  x <- c(0, 10, 20, 30, 40)
  y <- rep(0, 5)
  z <- rep(0, 5)

  s_no_edges <- calc_speed_3d(t, x, y, z, fill_edges = FALSE)
  expect_true(is.na(s_no_edges[1]))
  expect_true(is.na(s_no_edges[5]))
  expect_equal(s_no_edges[2:4], c(10, 10, 10))

  s_edges <- calc_speed_3d(t, x, y, z, fill_edges = TRUE)
  expect_equal(s_edges, rep(10, 5))
})
