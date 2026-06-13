#' Find Peak (or Valley) Windowed Cross-Correlations
#'
#' @param wcc_obj An object of class "wcc_res" or "wdtw_res".
#' @param L_size An odd integer specifying the size of the local search region.
#' @param strict_monotonic Logical indicating whether to strictly enforce
#'   monotonic flanks around the extremum. (default = FALSE)
#' @param find_min Logical indicating whether to search for local minima instead
#'   of local maxima. Required for distance metrics like DTW. (default = FALSE)
#' @return A data frame of class "wcc_peaks".
#' @export
pick_peaks <- function(wcc_obj, L_size, strict_monotonic = FALSE, find_min = FALSE) {

  if (!inherits(wcc_obj, c("wcc_res", "wdtw_res"))) {
    stop("Input must be a wcc_res or wdtw_res object.")
  }

  tau_max <- wcc_obj$settings$lag_max
  df <- wcc_obj$results_df

  # Adapt to whether this is a correlation or distance object
  metric_col <- if (inherits(wcc_obj, "wcc_res")) "wcc" else "dtw_dist"

  # 1. Order data chronologically and by lag to ensure perfect vector alignment
  df <- df[order(df$i, df$tau), ]

  # 2. Split the target metric values by the time index 'i'
  i_factor <- factor(df$i, levels = unique(df$i))
  wcc_list <- split(df[[metric_col]], i_factor)

  # 3. Extract unique 'i' values to pass as the structural backbone of the output
  i_vals <- as.numeric(names(wcc_list))

  # 4. Pass everything directly to C++ for ultra-fast processing
  out_df <- pick_peaks_cpp(
    wcc_list = wcc_list,
    i_vals = i_vals,
    tau_max = tau_max,
    L_size = L_size,
    strict_monotonic = strict_monotonic,
    find_min = find_min
  )

  # Return the formally constructed object
  wcc_peaks(out_df, L_size = L_size, strict_monotonic = strict_monotonic, find_min = find_min)
}

new_wcc_peaks <- function(x = data.frame(), L_size = numeric(), strict_monotonic = logical(), find_min = logical()) {
  stopifnot(is.data.frame(x))

  # Store settings as attributes so they don't clutter the data frame
  attr(x, "L_size") <- L_size
  attr(x, "strict_monotonic") <- strict_monotonic
  attr(x, "find_min") <- find_min

  structure(x, class = c("wcc_peaks", class(x)))
}

wcc_peaks <- function(x = data.frame(), L_size = numeric(), strict_monotonic = logical(), find_min = logical()) {
  new_wcc_peaks(x, L_size, strict_monotonic, find_min)
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
  find_min <- attr(x, "find_min")
  total_peaks <- nrow(x)

  cli::cli_h1("WCC Peak Picking Results")

  cli::cli_dl(c(
    "Total Extremes Found" = "{total_peaks}",
    "Local Search Size" = "{L_size}",
    "Strict Monotonic" = "{strict}",
    "Search Mode" = "{ifelse(find_min, 'Valleys (Minima)', 'Peaks (Maxima)')}"
  ))

  if (total_peaks > 0) {
    cli::cli_text("Showing the first {min(n, total_peaks)} result{?s}:")
    # Coerce to data frame to avoid recursive print looping
    print(utils::head(as.data.frame(x), n), row.names = FALSE)

    if (total_peaks > n) {
      remaining <- total_peaks - n
      cli::cli_text("{cli::col_grey('# ... with ', remaining, ' more row', ifelse(remaining == 1, '', 's'))}")
    }
  } else {
    cli::cli_alert_info("No extremes found matching the criteria.")
  }

  invisible(x)
}
