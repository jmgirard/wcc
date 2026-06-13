#' Calculate Surrogate Windowed Cross-Correlations
#'
#' Generates a null distribution of Windowed Cross-Correlation (WCC) values using
#' the circular shift method. This tests whether the observed synchronization is
#' significantly greater than what would be expected by chance given the inherent
#' autocorrelation of the individual time series.
#'
#' @param x A numeric vector containing a time series.
#' @param y A numeric vector containing a time series.
#' @param window_size A positive integer indicating the size of each window.
#' @param lag_max A positive integer indicating the maximum lag to try.
#' @param window_increment A positive integer indicating the window shift increment. Default is 1.
#' @param lag_increment A positive integer indicating the lag shift increment. Default is 1.
#' @param na.rm A logical indicating whether to remove missing values. Default is `TRUE`.
#' @param n_surrogates An integer specifying the number of surrogate permutations to run. Default is 100.
#' @return A list object of class "wcc_surr" containing the observed Fisher's Z,
#'   the distribution of surrogate Z scores, the empirical p-value, and settings.
#' @export
wcc_surrogate <- function(x, y, window_size, lag_max,
                          window_increment = 1, lag_increment = 1,
                          na.rm = TRUE, n_surrogates = 100) {

  # 1. Calculate the observed WCC score
  obs_wcc <- wcc(x, y, window_size, lag_max, window_increment, lag_increment, na.rm)
  obs_z <- obs_wcc$fisher_z

  n_y <- length(y)

  # Ensure the shift is large enough to break true synchrony
  min_shift <- lag_max * 2
  max_shift <- n_y - min_shift

  if (max_shift <= min_shift) {
    stop("Time series is too short relative to lag_max to perform valid circular shifts.")
  }

  valid_shifts <- min_shift:max_shift

  # Sample shifts (with replacement only if necessary)
  if (length(valid_shifts) < n_surrogates) {
    warning("Limited unique shifts available. Sampling with replacement.")
    shifts <- sample(valid_shifts, n_surrogates, replace = TRUE)
  } else {
    shifts <- sample(valid_shifts, n_surrogates, replace = FALSE)
  }

  # 2. Build the structural grid ONCE for maximum speed
  lags <- seq(-lag_max, lag_max, by = lag_increment)
  n_r <- floor((length(x) - window_size - lag_max) / window_increment)
  n_c <- length(lags)

  grid_df <- base::expand.grid(row = 1:n_r, col = 1:n_c) |>
    dplyr::mutate(
      i = 1 + lag_max + (row - 1) * window_increment,
      tau = lags[col]
    )

  i_vals <- grid_df$i
  tau_vals <- grid_df$tau
  surrogate_zs <- numeric(n_surrogates)

  # 3. Fast Loop: Only run the C++ math and the Fisher Z aggregation
  for (idx in seq_len(n_surrogates)) {
    shift <- shifts[idx]
    y_surr <- c(y[(shift + 1):n_y], y[1:shift])

    wcc_vals <- calc_wcc_cpp(
      x = x,
      y = y_surr,
      i_vals = i_vals,
      tau_vals = tau_vals,
      w_max = window_size
    )

    # Inline r-to-z transformation for speed
    wcc_vals[wcc_vals == -1] <- -0.99
    wcc_vals[wcc_vals == 1] <- 0.99
    z_vals <- 0.5 * base::log((1 + wcc_vals) / (1 - wcc_vals))

    surrogate_zs[idx] <- base::mean(base::abs(z_vals), na.rm = TRUE)
  }

  # 4. Calculate empirical p-value
  p_val <- sum(surrogate_zs >= obs_z) / n_surrogates

  out <- list(
    observed_z = obs_z,
    surrogate_z = surrogate_zs,
    p_value = p_val,
    n_surrogates = n_surrogates,
    settings = obs_wcc$settings
  )

  structure(out, class = c("wcc_surr", "list"))
}

#' Print method for wcc_surr objects
#'
#' @param x An object of class "wcc_surr".
#' @param ... Additional arguments (not used).
#' @export
print.wcc_surr <- function(x, ...) {

  cli::cli_h1("WCC Surrogate Analysis (Pseudo-Synchrony)")

  # Dynamically format the p-value based on the permutation resolution
  if (x$p_value == 0) {
    p_disp <- paste0("< ", 1 / x$n_surrogates)
  } else {
    p_disp <- as.character(round(x$p_value, 4))
  }

  cli::cli_dl(c(
    "Permutations" = "{x$n_surrogates}",
    "Observed Fisher's Z" = "{round(x$observed_z, 4)}",
    "Average Null Z" = "{round(mean(x$surrogate_z), 4)}",
    "Empirical p-value" = "{p_disp}"
  ))

  if (x$p_value < 0.05) {
    cli::cli_alert_success("Observed synchrony is significantly greater than chance.")
  } else {
    cli::cli_alert_warning("Observed synchrony is not significantly different from chance.")
  }

  if (x$n_surrogates < 1000) {
    cli::cli_alert_info("Note: {x$n_surrogates} permutations may be too few for stable p-values.\nConsider setting `n_surrogates >= 1000` for final reporting.")
  }

  invisible(x)
}
