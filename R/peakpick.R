#' Find Peak Windowed Cross-Correlations
#'
#' @param wcc_obj An object of class "wcc_res" returned by `wcc()`.
#' @param L_size An odd integer specifying the size of the local search region.
#' @param strict_monotonic Logical indicating whether to strictly enforce
#'   decreasing values on the flanks of the peak. (default = FALSE)
#' @return A data frame containing the elapsed time indices, the peak lags,
#'   and the peak correlation values.
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

  out_df
}
