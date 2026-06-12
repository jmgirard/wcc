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

  method <- match.arg(method)

  if (method == "moving_average") {
    # Uses a centered moving average to prevent phase shifts
    weights <- rep(1 / window, window)
    res <- as.numeric(stats::filter(x, weights, sides = 2))
    return(res)
  }

  if (method == "sgolay") {
    if (!requireNamespace("signal", quietly = TRUE)) {
      stop("The 'signal' package is required for the Savitzky-Golay filter. Please run `install.packages('signal')`.")
    }
    if (window %% 2 == 0) {
      stop("The 'window' argument must be an odd integer for the Savitzky-Golay filter.")
    }
    if (sg_order >= window) {
      stop("The 'sg_order' must be strictly less than the 'window' size.")
    }
    res <- signal::sgolayfilt(x, p = sg_order, n = window)
    return(as.numeric(res))
  }

  if (method == "butterworth") {
    if (!requireNamespace("signal", quietly = TRUE)) {
      stop("The 'signal' package is required for the Butterworth filter. Please run `install.packages('signal')`.")
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
