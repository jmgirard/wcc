
# Calculate windowed cross-correlation ------------------------------------

calc_wcc <- function(i, tau, x, y, w_max, na.rm = TRUE) {

  assertthat::assert_that(rlang::is_double(i, n = 1), i > 0)
  assertthat::assert_that(rlang::is_double(tau, n = 1))

  if (tau <= 0) {
    Wx <- x[(i):(i + w_max)]
    Wy <- y[(i + tau):(i + tau + w_max)]
  } else {
    Wx <- x[(i - tau):(i - tau + w_max)]
    Wy <- y[(i):(i + w_max)]
  }

  mWx <- mean(Wx, na.rm = na.rm)
  sWx <- sd(Wx, na.rm = na.rm)
  mWy <- mean(Wy, na.rm = na.rm)
  sWy <- sd(Wy, na.rm = na.rm)

  wcc <- mean(((Wx - mWx) * (Wy - mWy)) / (sWx * sWy), na.rm = na.rm)

  wcc
}


# Create wcc results df ---------------------------------------------------

create_wcc_df <- function(x, y, settings) {

  n_x <- length(x)
  n_y <- length(y)

  assertthat::assert_that(assertthat::are_equal(n_x, n_y))

  w_max <- settings$window_size
  w_inc <- settings$window_increment
  tau_max <- settings$lag_max
  tau_inc <- settings$lag_increment
  na.rm <- settings$na.rm

  # Generate sequence of lags to use
  lags <- seq(-tau_max, tau_max, by = tau_inc)

  # Calculate the size of the results matrix
  n_r <- floor((n_x - w_max - tau_max) / w_inc)
  n_c <- length(lags)

  results_df <-
    tidyr::crossing(row = 1:n_r, col = 1:n_c) |>
    dplyr::mutate(
      i = 1 + tau_max + (row - 1) * w_inc,
      tau = lags[col],
      wcc = furrr::future_map2_dbl(.x = i, .y = tau, .f = calc_wcc,
                            x = x, y = y, w_max = w_max, na.rm = na.rm)
    )

  results_df
}



# df to matrix transformation ---------------------------------------------

df_to_matrix <- function(results_df) {

  out <- matrix(
    data = results_df$wcc,
    nrow = max(results_df$row),
    ncol = max(results_df$col),
    dimnames = list(time = unique(results_df$i), lag = unique(results_df$tau))
  )

  out
}

# r-to-z transformation ---------------------------------------------------

r_to_z <- function(r) {
  0.5 * base::log((1 + r) / (1 - r))
}


# Fisher's Z calculation --------------------------------------------------

fisher_z <- function(results_df) {
  rvec <- results_df$wcc
  zvec <- r_to_z(rvec)
  azvec <- base::abs(zvec)
  res <- base::mean(azvec)

  res
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
#' @param na.rm A logical indicating whether to remove missing values from the
#'   windows when calculating windowed cross-correlations. (default = `TRUE`)
#' @return A list object of class "wcc" containing the results matrix and useful
#'   summaries of it.
#' @export
#'
wcc <- function(x, y, window_size, lag_max,
                window_increment = 1, lag_increment = 1, na.rm = TRUE) {

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
  assertthat::assert_that(rlang::is_logical(na.rm, n = 1))

  settings <- list(
    window_size = window_size,
    window_increment = window_increment,
    lag_max = lag_max,
    lag_increment = lag_increment,
    na.rm = na.rm
  )

  results_df <- create_wcc_df(
    x = x,
    y = y,
    settings = settings
  )

  out <- list(
    results_df = results_df,
    fisher_z = fisher_z(results_df),
    settings = settings
  )

  out
}
