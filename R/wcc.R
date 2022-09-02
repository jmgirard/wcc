
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
#' @param x A vector containing one time series (same length as `y`).
#' @param y A vector containing another time series (same length as `x`).
#' @param w_max Window size
#' @param w_inc Window increment
#' @param tau_max Largest lag size
#' @param tau_inc Lag increment
#' @return A matrix

create_wcc_matrix <- function(x, y, w_max, w_inc, tau_max, tau_inc) {
  n_x <- length(x)
  n_y <- length(y)

  lags <- base::seq(-tau_max, tau_max, by = tau_inc)

  r_WxWy <- matrix(
    nrow = floor((n - w_max - tau_max) / w_inc),
    ncol = length(lags)
  )

  for (rrow in 1:nrow(r_WxWy)) {
    i <- 1 + tau_max + (rrow - 1) * w_inc
    for (rcol in 1:ncol(r_WxWy)) {
      tau <- lags[[rcol]]
      WxWy <- make_windows(x, y, i, tau, w_max)
      r_WxWy[rrow, rcol] <- calc_cc(WxWy)
    }
  }

  r_WxWy
}
