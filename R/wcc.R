#' Make windows
#'
#' Create two windows with the requested size and lagging.
#'
#' @param x A vector containing one time series (same length as `y`).
#' @param y A vector containing another time series (same length as `x`).
#' @param i A positive integer indicating the element in `x` and `y` to center
#'   on.
#' @param tau An integer indicating the lag applied to `y`, i.e., the number of
#'   elements to offset it relative to `x`.
#' @param w_max A positive integer indicating the size of the window, i.e., the
#'   number of elements from `x` and `y` to include in the window.
#' @return A list containing the windows `Wx` and `Wy`

make_windows <- function(x, y, i, tau, w_max) {

  assertthat::assert_that(rlang::is_double(x))
  assertthat::assert_that(rlang::is_double(y))
  assertthat::assert_that(rlang::is_double(i, n = 1), i > 0)
  assertthat::assert_that(rlang::is_double(tau, n = 1))
  assertthat::assert_that(rlang::is_double(w_max, n = 1), w_max > 0)

  if (tau <= 0) {
    Wx <- x[(i):(i + w_max)]
    Wy <- y[(i + tau):(i + tau + w_max)]
  } else {
    Wx <- x[(i - tau):(i - tau + w_max)]
    Wy <- y[(i):(i + w_max)]
  }
  WxWy <- list(Wx = Wx, Wy = Wy)

  WxWy
}


#' Calculate Cross-Correlation
#'
#' Calculate the cross-correlation of a pair of windows
#'
#' @param WxWy A list containing the windows for two variables `x` and `y`.
#'   Usually created by the `make_windows()` function.
#' @param na.rm A logical indicating whether to remove missing values from the
#'   windows before calculating the cross-correlation (default = `TRUE`).
#' @return A double between -1 and 1 indicating the cross-correlation or `NA`.

calc_cc <- function(WxWy, na.rm = TRUE) {
  Wx <- windows$Wx
  Wy <- windows$Wy
  mWx <- mean(Wx, na.rm = na.rm)
  sWx <- sd(Wx, na.rm = na.rm)
  mWy <- mean(Wy, na.rm = na.rm)
  sWy <- sd(Wy, na.rm = na.rm)
  r <- mean(((Wx - mWx) * (Wy - mWy)) / (sWx * sWy), na.rm = na.rm)

  r
}


#' Create wcc matrix
#'
#' Create windowed cross-correlation matrix.
#'
#' @param x Desc
#' @param y Desc
#' @param w_max Window size
#' @param w_inc Window increment
#' @param tau_max Largest lag size
#' @param tau_inc Lag increment
#' @return A matrix

create_wcc_matrix <- function(x, y, w_max, w_inc, tau_max, tau_inc) {
  n_x <- length(x)
  n_y <- length(y)

  r_WxWy <- matrix(
    nrow = floor((n - w_max - tau_max) / w_inc),
    ncol = (tau_max * 2) + 1
  )

  for (rrow in 1:nrow(r_WxWy)) {
    i <- 1 + tau_max + (rrow - 1) * w_inc
    for (rcol in 1:ncol(r_WxWy)) {
      tau <- (-tau_max:tau_max)[[rcol]]
      WxWy <- make_windows(x, y, i, tau, w_max)
      r_WxWy[rrow, rcol] <- calc_cc(WxWy)
    }
  }

  r_WxWy
}
