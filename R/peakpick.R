#' Find Peak Windowed Cross-Correlations
#'
#' @param wcc_obj An object of class "wcc_res" returned by `wcc()`.
#' @param L_size An odd integer specifying the size of the local search region.
#' @param strict_monotonic Logical indicating whether to strictly enforce
#'   decreasing values on the flanks of the peak. (default = FALSE)
#' @return A data frame of class "wcc_peaks" containing the elapsed time indices,
#'   the peak lags, and the peak correlation values.
#' @export
pick_peaks <- function(wcc_obj, L_size, strict_monotonic = FALSE) {

  if (!inherits(wcc_obj, "wcc_res")) {
    stop("Input must be a wcc_res object.")
  }

  tau_max <- wcc_obj$settings$lag_max
  df <- wcc_obj$results_df

  # 1. Order data chronologically and by lag to ensure perfect vector alignment
  df <- df[order(df$i, df$tau), ]

  # 2. Split the correlation values by the time index 'i'
  # A factor guarantees the chronological order of 'i' is preserved in the split list
  i_factor <- factor(df$i, levels = unique(df$i))
  wcc_list <- split(df$wcc, i_factor)

  # 3. Extract unique 'i' values to pass as the structural backbone of the output
  i_vals <- as.numeric(names(wcc_list))

  # 4. Pass everything directly to C++ for ultra-fast processing
  out_df <- pick_peaks_cpp(
    wcc_list = wcc_list,
    i_vals = i_vals,
    tau_max = tau_max,
    L_size = L_size,
    strict_monotonic = strict_monotonic
  )

  # Return the formally constructed object
  wcc_peaks(out_df, L_size = L_size, strict_monotonic = strict_monotonic)
}

new_wcc_peaks <- function(x = data.frame(), L_size = numeric(), strict_monotonic = logical()) {
  stopifnot(is.data.frame(x))

  # Store settings as attributes so they don't clutter the data frame
  attr(x, "L_size") <- L_size
  attr(x, "strict_monotonic") <- strict_monotonic

  structure(x, class = c("wcc_peaks", class(x)))
}

wcc_peaks <- function(x = data.frame(), L_size = numeric(), strict_monotonic = logical()) {
  new_wcc_peaks(x, L_size, strict_monotonic)
}

#' Print method for wcc_peaks objects
#'
#' @param x An object of class "wcc_peaks".
#' @param n An integer specifying how many rows of peaks to print. Default is 5.
#' @param ... Additional arguments (not used).
#' @export
print.wcc_peaks <- function(x, n = 5, ...) {
  L_size <- attr(x, "L_size")
  strict <- attr(x, "strict_monotonic")
  total_peaks <- nrow(x)

  cli::cli_h1("WCC Peak Picking Results")

  cli::cli_dl(c(
    "Total Peaks Found" = "{total_peaks}",
    "Local Search Size" = "{L_size}",
    "Strict Monotonic" = "{strict}"
  ))

  if (total_peaks > 0) {
    cli::cli_text("Showing the first {min(n, total_peaks)} peak{?s}:")
    # Coerce to data frame to avoid recursive print looping
    print(head(as.data.frame(x), n), row.names = FALSE)

    if (total_peaks > n) {
      remaining <- total_peaks - n
      cli::cli_text("{cli::col_grey('# ... with ', remaining, ' more row', ifelse(remaining == 1, '', 's'))}")
    }
  } else {
    cli::cli_alert_info("No peaks found matching the criteria.")
  }

  invisible(x)
}
