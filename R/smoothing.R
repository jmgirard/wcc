# Main Functions ----------------------------------------------------------

#' Smooth a Time Series Signal
#'
#' Applies a smoothing filter to a numeric vector. Smoothing is highly recommended
#' prior to calculating velocity or running windowed cross-correlation (WCC) to
#' reduce high-frequency noise and prevent spurious correlations.
#'
#' @param x A numeric vector representing the signal to be smoothed.
#' @param method A character string specifying the smoothing method:
#'   "moving_average", "sgolay" (Savitzky-Golay), or "butterworth". Default is "sgolay".
#' @param window An integer specifying the window size (number of data points) for the
#'   "moving_average" and "sgolay" methods. Must be an odd number for "sgolay". Default is 5.
#' @param sg_order An integer specifying the polynomial order for the Savitzky-Golay filter.
#'   Must be less than `window`. Default is 3.
#' @param bw_cutoff A numeric value between 0 and 1 specifying the normalized cutoff
#'   frequency for the Butterworth filter. Default is 0.1.
#' @param bw_order An integer specifying the order of the Butterworth filter. Default is 2.
#' @return A numeric vector containing the smoothed signal, of the same length as `x`.
#' @export
smooth_signal <- function(x,
                          method = c("sgolay", "moving_average", "butterworth"),
                          window = 5,
                          sg_order = 3,
                          bw_cutoff = 0.1,
                          bw_order = 2) {

  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector.")
  }

  method <- match.arg(method)

  if (method == "moving_average") {
    if (!rlang::is_integerish(window, n = 1) || window <= 0) {
      cli::cli_abort("{.arg window} must be a positive integer for the moving average filter.")
    }
    # Uses a centered moving average to prevent phase shifts
    weights <- rep(1 / window, window)
    res <- as.numeric(stats::filter(x, weights, sides = 2))
    return(res)
  }

  if (method == "sgolay") {
    if (!rlang::is_integerish(window, n = 1) || window %% 2 == 0) {
      cli::cli_abort("{.arg window} must be an odd integer for the Savitzky-Golay filter.")
    }
    if (!rlang::is_integerish(sg_order, n = 1) || sg_order >= window) {
      cli::cli_abort("{.arg sg_order} must be strictly less than the {.arg window} size.")
    }
    res <- signal::sgolayfilt(x, p = sg_order, n = window)
    return(as.numeric(res))
  }

  if (method == "butterworth") {
    if (!is.numeric(bw_cutoff) || length(bw_cutoff) != 1 || bw_cutoff <= 0 || bw_cutoff >= 1) {
      cli::cli_abort("{.arg bw_cutoff} must be a single numeric value between 0 and 1.")
    }
    if (!rlang::is_integerish(bw_order, n = 1) || bw_order <= 0) {
      cli::cli_abort("{.arg bw_order} must be a positive integer.")
    }
    # Uses filtfilt for zero-phase filtering to prevent phase shifts
    bf <- signal::butter(bw_order, bw_cutoff, type = "low")
    res <- signal::filtfilt(bf, x)
    return(as.numeric(res))
  }
}

#' Aggregate Time Series Data by Time Bins
#'
#' Efficiently downsamples time series data by calculating the mean of values
#' within specified time bins. This is highly recommended for high-resolution
#' data (e.g., 30Hz OpenFace output) prior to calculating velocity or running
#' windowed cross-correlation.
#'
#' @param data A data frame containing the time series data.
#' @param time_var The unquoted name of the column containing time values.
#' @param bin_width A numeric value specifying the width of the time bins.
#'   This should be in the same units as your time variable (e.g., 0.1 for 100ms bins).
#' @param na.rm A logical indicating whether to remove missing values when
#'   calculating the mean. Default is `TRUE`.
#' @return A new data frame with the downsampled time series. The time variable
#'   is updated to represent the center of each bin, and all non-numeric columns
#'   are dropped.
#' @export
aggregate_by_time <- function(data, time_var, bin_width, na.rm = TRUE) {

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!is.numeric(bin_width) || length(bin_width) != 1 || bin_width <= 0) {
    cli::cli_abort("{.arg bin_width} must be a single positive number.")
  }
  if (!rlang::is_logical(na.rm, n = 1)) {
    cli::cli_abort("{.arg na.rm} must be a single logical value.")
  }

  data |>
    dplyr::mutate(
      .bin_center = floor({{ time_var }} / bin_width) * bin_width + (bin_width / 2)
    ) |>
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = na.rm)),
      .by = .bin_center
    ) |>
    dplyr::mutate({{ time_var }} := .bin_center) |>
    dplyr::select(-.bin_center)
}

#' Trim Edge Effects from Data
#'
#' Removes a specified number of observations from the beginning and end of a
#' vector or data frame. This is highly recommended after applying zero-phase or
#' polynomial smoothing filters (e.g., Savitzky-Golay) to remove boundary artifacts.
#'
#' @param x A numeric vector, matrix, or data frame.
#' @param trim_length An integer specifying the number of observations to remove
#'   from both ends. A standard rule of thumb is to set this equal to the window
#'   size used for smoothing.
#' @return An object of the same class as `x` with the edges removed.
#' @export
trim_edges <- function(x, trim_length) {

  if (!rlang::is_integerish(trim_length, n = 1) || trim_length <= 0) {
    cli::cli_abort("{.arg trim_length} must be a single positive integer.")
  }

  if (is.data.frame(x) || is.matrix(x)) {
    n_rows <- nrow(x)
    if (n_rows <= 2 * trim_length) {
      cli::cli_abort("{.arg trim_length} is too large; it would remove all rows from the data.")
    }
    return(x[(trim_length + 1):(n_rows - trim_length), , drop = FALSE])
  } else if (is.atomic(x) && is.vector(x)) {
    n_len <- length(x)
    if (n_len <= 2 * trim_length) {
      cli::cli_abort("{.arg trim_length} is too large; it would remove all elements from the vector.")
    }
    return(x[(trim_length + 1):(n_len - trim_length)])
  } else {
    cli::cli_abort("Input {.arg x} must be a vector, matrix, or data frame.")
  }
}
