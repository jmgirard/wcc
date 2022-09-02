
make_windows <- function(x, y, i, tau, w_max) {

  assertthat::assert_that(rlang::is_double(i, n = 1), i > 0)
  assertthat::assert_that(rlang::is_double(tau, n = 1))

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

  assertthat::assert_that(rlang::is_list(WxWy))
  assertthat::assert_that(rlang::is_logical(na.rm, n = 1))

  Wx <- windows$Wx
  Wy <- windows$Wy
  mWx <- mean(Wx, na.rm = na.rm)
  sWx <- sd(Wx, na.rm = na.rm)
  mWy <- mean(Wy, na.rm = na.rm)
  sWy <- sd(Wy, na.rm = na.rm)
  r <- mean(((Wx - mWx) * (Wy - mWy)) / (sWx * sWy), na.rm = na.rm)

  r
}

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

#' Windowed Cross-Correlation
#'
#' Conduct a windowed cross-correlation analysis
#'
#' @param x A numeric vector containing a time series (same length as `y`).
#' @param y A numeric vector containing a time series (same length as `x`).
#' @param window_size A positive integer indicating the size of each window,
#'   i.e., the number of elements in each window vector. Boker et al. suggest
#'   setting the window small enough so that the assumption can be made of
#'   little change in lead-lag relationships within the number of samples in the
#'   window but not so small that the reliability for the correlation estimate
#'   for each sample will be reduced.
#' @param lag_max A positive integer indicating the maximum lag to try between
#'   `x` and `y` windows. Boker et al. recommend selecting the greatest interval
#'   of time separating a behavior from participant `x` and a behavior from
#'   participant `y` that would be considered to be of interest.
#' @param window_increment A positive integer indicating the number of samples
#'   between successive changes in the window for the `x` vector. Can be made
#'   larger than 1 to reduce the number of rows in the output matrix. Boker et
#'   al. recommend setting the window increment as long as possible, but not so
#'   long that the relation between successive rows in the results matrix is
#'   lost. (default = `1`)
#' @param lag_increment A positive integer indicating the number of samples
#'   between successive changes in the window for the `y` vector (and thus also
#'   the interval of time separating successive columns in the results matrix).
#'   Boker et al. recommend setting the lag increment to the longest lag
#'   increment that still results in related change between successive columns.
#'   (default = `1`)
#' @return A list object of class "wcc" containing the results matrix and useful
#'   summaries of it.
wcc <- function(x, y, window_size, lag_max,
                window_increment = 1, lag_increment = 1) {

  assertthat::assert_that(rlang::is_double(x))
  assertthat::assert_that(rlang::is_double(y))
  assertthat::assert_that(rlang::is_integerish(window_size, n = 1))
  assertthat::assert_that(window_size > 0)
  assertthat::assert_that(rlang::is_integerish(lag_max, n = 1))
  assertthat::assert_that(lag_max > 0)
  assertthat::assert_that(rlang::is_integerish(window_increment, n = 1))
  assertthat::assert_that(window_increment > 0)
  assertthat::assert_that(rlang::is_integerish(lag_increment, n = 1))
  assertthat::assert_that(lag_increment > 0)

  r_WxWy <- create_wcc_matrix(
    x = x,
    y = y,
    w_max = window_size,
    w_inc = window_increment,
    tau_max = lag_max,
    tau_inc = lag_increment
  )

  out <- list(
    r_WxWy = r_WxWy
  )

  out
}
