#' Calculate 3D Velocity Magnitude (Speed)
#'
#' Calculates the magnitude of the 3D velocity vector (speed) using a central difference method.
#'
#' @param t A numeric vector representing time.
#' @param x A numeric vector of x-coordinates.
#' @param y A numeric vector of y-coordinates.
#' @param z A numeric vector of z-coordinates.
#' @param n An integer specifying the step size for the lag and lead calculations. Default is 1.
#' @return A numeric vector of velocities. The first and last `n` elements will be `NA`.
#' @export
v_xyz <- function(t, x, y, z, n = 1) {

  tm1 <- dplyr::lag(t, n)
  tp1 <- dplyr::lead(t, n)

  xm1 <- dplyr::lag(x, n)
  xp1 <- dplyr::lead(x, n)
  ym1 <- dplyr::lag(y, n)
  yp1 <- dplyr::lead(y, n)
  zm1 <- dplyr::lag(z, n)
  zp1 <- dplyr::lead(z, n)

  dt <- tp1 - tm1

  v <- sqrt(
    ((xp1 - xm1) / dt)^2 +
    ((yp1 - ym1) / dt)^2 +
    ((zp1 - zm1) / dt)^2
  )

  v
}

#' Calculate 1D Velocity Magnitude (X-Axis)
#'
#' Calculates the absolute velocity (speed) along the x-axis using a central difference method.
#'
#' @param t A numeric vector representing time.
#' @param x A numeric vector of x-coordinates.
#' @param n An integer specifying the step size for the lag and lead calculations. Default is 1.
#' @return A numeric vector of velocities. The first and last `n` elements will be `NA`.
#' @export
v_x <- function(t, x, n = 1) {

  tm1 <- dplyr::lag(t, n)
  tp1 <- dplyr::lead(t, n)

  xm1 <- dplyr::lag(x, n)
  xp1 <- dplyr::lead(x, n)

  dt <- tp1 - tm1

  v <- abs((xp1 - xm1) / dt)

  v
}

#' Calculate 1D Velocity Magnitude (Y-Axis)
#'
#' Calculates the absolute velocity (speed) along the y-axis using a central difference method.
#'
#' @param t A numeric vector representing time.
#' @param y A numeric vector of y-coordinates.
#' @param n An integer specifying the step size for the lag and lead calculations. Default is 1.
#' @return A numeric vector of velocities. The first and last `n` elements will be `NA`.
#' @export
v_y <- function(t, y, n = 1) {

  tm1 <- dplyr::lag(t, n)
  tp1 <- dplyr::lead(t, n)

  ym1 <- dplyr::lag(y, n)
  yp1 <- dplyr::lead(y, n)

  dt <- tp1 - tm1

  v <- abs((yp1 - ym1) / dt)

  v
}

#' Calculate 1D Velocity Magnitude (Z-Axis)
#'
#' Calculates the absolute velocity (speed) along the z-axis using a central difference method.
#'
#' @param t A numeric vector representing time.
#' @param z A numeric vector of z-coordinates.
#' @param n An integer specifying the step size for the lag and lead calculations. Default is 1.
#' @return A numeric vector of velocities. The first and last `n` elements will be `NA`.
#' @export
v_z <- function(t, z, n = 1) {

  tm1 <- dplyr::lag(t, n)
  tp1 <- dplyr::lead(t, n)

  zm1 <- dplyr::lag(z, n)
  zp1 <- dplyr::lead(z, n)

  dt <- tp1 - tm1

  v <- abs((zp1 - zm1) / dt)

  v
}
