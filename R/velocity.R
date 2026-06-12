#' @export
v_xyz <- function(t, x, y, z) {

  tm1 <- dplyr::lag(t, 1)
  tp1 <- dplyr::lead(t, 1)

  xm1 <- dplyr::lag(x, 1)
  xp1 <- dplyr::lead(x, 1)
  ym1 <- dplyr::lag(y, 1)
  yp1 <- dplyr::lead(y, 1)
  zm1 <- dplyr::lag(z, 1)
  zp1 <- dplyr::lead(z, 1)

  dt <- tp1 - tm1

  v <- sqrt(
    ((xp1 - xm1) / dt)^2 +
    ((yp1 - ym1) / dt)^2 +
    ((zp1 - zm1) / dt)^2
  )

  v
}

#' @export
v_y <- function(t, y) {

  tm1 <- dplyr::lag(t, 1)
  tp1 <- dplyr::lead(t, 1)

  ym1 <- dplyr::lag(y, 1)
  yp1 <- dplyr::lead(y, 1)

  dt <- tp1 - tm1

  v <- sqrt(((yp1 - ym1) / dt)^2)

  v
}

#' @export
v_x <- function(t, x) {

  tm1 <- dplyr::lag(t, 1)
  tp1 <- dplyr::lead(t, 1)

  xm1 <- dplyr::lag(x, 1)
  xp1 <- dplyr::lead(x, 1)

  dt <- tp1 - tm1

  v <- sqrt(((xp1 - xm1) / dt)^2)

  v
}

#' @export
v_z <- function(t, z) {

  tm1 <- dplyr::lag(t, 1)
  tp1 <- dplyr::lead(t, 1)

  zm1 <- dplyr::lag(z, 1)
  zp1 <- dplyr::lead(z, 1)

  dt <- tp1 - tm1

  v <- sqrt(((zp1 - zm1) / dt)^2)

  v
}
