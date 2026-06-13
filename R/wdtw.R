# Create wdtw results df --------------------------------------------------

create_wdtw_df <- function(x, y, time = NULL, settings) {

  n_x <- length(x)
  w_max <- settings$window_size
  w_inc <- settings$window_increment
  tau_max <- settings$lag_max
  tau_inc <- settings$lag_increment

  lags <- seq(-tau_max, tau_max, by = tau_inc)

  n_r <- floor((n_x - w_max - 2 * tau_max) / w_inc)
  n_c <- length(lags)

  results_df <- base::expand.grid(row = 1:n_r, col = 1:n_c) |>
    dplyr::mutate(
      i = 1 + tau_max + (row - 1) * w_inc,
      tau = lags[col],
      dtw_dist = calc_wdtw_cpp(
        x = x,
        y = y,
        i_vals = i,
        tau_vals = tau,
        w_max = w_max
      )
    )

  if (!is.null(time)) {
    results_df$i <- time[results_df$i]
  }

  results_df
}

#' Windowed Dynamic Time Warping
#'
#' Conduct a windowed dynamic time warping (WDTW) analysis to find the optimal
#' alignment distance between sliding windows of two time series.
#'
#' @param x A numeric vector containing a time series (same length as `y`).
#' @param y A numeric vector containing a time series (same length as `x`).
#' @param time An optional numeric vector representing the timestamps for the
#'   data. Must be the same length as `x` and `y`. If provided, the rolling
#'   window indices will be mapped directly to these timestamps in the results,
#'   which is highly recommended to maintain accurate timelines if edge
#'   artifacts were trimmed prior to analysis. Default is `NULL`.
#' @param window_size A positive integer indicating the size of each window.
#' @param lag_max A positive integer indicating the maximum lag to try.
#' @param window_increment A positive integer indicating the window shift increment. (default = `1`)
#' @param lag_increment A positive integer indicating the lag shift increment. (default = `1`)
#' @param scale_data A logical indicating whether to z-score standardize both
#'   time series prior to calculation. Highly recommended for DTW. (default = `TRUE`)
#' @return A list object of class "wdtw_res".
#' @export
wdtw <- function(x, y, time = NULL, window_size, lag_max,
                 window_increment = 1, lag_increment = 1,
                 scale_data = TRUE) {

  assertthat::assert_that(is.numeric(x))
  assertthat::assert_that(is.numeric(y))

  if (!is.null(time)) {
    assertthat::assert_that(is.numeric(time))
    assertthat::assert_that(length(time) == length(x))
  }

  assertthat::assert_that(rlang::is_logical(scale_data, n = 1))

  if (scale_data) {
    # as.numeric quickly strips the matrix structure returned by base::scale
    x <- as.numeric(base::scale(x))
    y <- as.numeric(base::scale(y))
  }

  x <- as.double(x)
  y <- as.double(y)
  assertthat::assert_that(rlang::is_integerish(window_size, n = 1))
  assertthat::assert_that(window_size > 0)
  assertthat::assert_that(rlang::is_integerish(lag_max, n = 1))
  assertthat::assert_that(lag_max > 0)
  assertthat::assert_that(rlang::is_integerish(window_increment, n = 1))
  assertthat::assert_that(window_increment > 0)
  assertthat::assert_that(rlang::is_integerish(lag_increment, n = 1))
  assertthat::assert_that(lag_increment > 0)

  settings <- list(
    window_size = window_size,
    window_increment = window_increment,
    lag_max = lag_max,
    lag_increment = lag_increment,
    scale_data = scale_data,
    has_time = !is.null(time)
  )

  results_df <- create_wdtw_df(x = x, y = y, time = time, settings = settings)
  mean_dist <- base::mean(results_df$dtw_dist, na.rm = TRUE)

  out <- list(
    results_df = results_df,
    mean_distance = mean_dist,
    settings = settings
  )

  new_wdtw_res(out)
}

new_wdtw_res <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = c("wdtw_res", class(x)))
}

#' Print method for wdtw_res objects
#'
#' @param x An object of class "wdtw_res".
#' @param ... Additional arguments (not used).
#' @export
print.wdtw_res <- function(x, ...) {
  s <- x$settings
  n_windows <- length(unique(x$results_df$i))
  n_lags <- length(unique(x$results_df$tau))

  cli::cli_h1("Windowed Dynamic Time Warping Analysis")

  cli::cli_dl(c(
    "Total Windows" = "{n_windows}",
    "Total Lags Tested" = "{n_lags}",
    "Window Size" = "{s$window_size}",
    "Max Lag" = "{s$lag_max}",
    "Overall Mean Distance" = "{round(x$mean_distance, 4)}"
  ))

  invisible(x)
}
