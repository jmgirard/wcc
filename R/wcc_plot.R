#' Plot wcc_res object
#'
#' @param x An object of class "wcc_res".
#' @param ... Additional arguments (not used).
#' @export
plot.wcc_res <- function(x, ...) {
  requireNamespace("RColorBrewer")
  df <- dplyr::mutate(
    x$results_df,
    wcc_bin = dplyr::case_when(
      wcc <= -0.8 ~ "[-1, -0.8]",
      wcc <= -0.6 ~ "(-0.8, -0.6]",
      wcc <= -0.4 ~ "(-0.6, -0.4]",
      wcc <= -0.2 ~ "(-0.4, -0.2]",
      wcc < 0 ~ "(-0.2, 0)",
      wcc < 0.2 ~ "[0, 0.2)",
      wcc < 0.4 ~ "[0.2, 0.4)",
      wcc < 0.6 ~ "[0.4, 0.6)",
      wcc < 0.8 ~ "[0.6, 0.8)",
      wcc <= 1.0 ~ "[0.8, 1.0]"
    ),
    wcc_bin = factor(wcc_bin, levels = c(
      "[-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]", "(-0.4, -0.2]", "(-0.2, 0)",
      "[0, 0.2)", "[0.2, 0.4)", "[0.4, 0.6)", "[0.6, 0.8)", "[0.8, 1.0]"))
  )
  p <-
    ggplot2::ggplot(data = df, ggplot2::aes(x = tau, y = i, fill = wcc_bin)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_brewer(palette = "RdBu", drop = FALSE)
  p
}

#' Plot Windowed Cross-Correlation with Peak Overlay
#'
#' @param wcc_obj An object of class "wcc_res" containing the windowed cross-correlation results.
#' @param peaks_df A data frame containing the peaks to overlay.
#' @export
plot_peaks_overlay <- function(wcc_obj, peaks_df) {

  # Base heatmap from your wcc_plot.R script
  breaks <- c(-Inf, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, Inf)
  labels <- c("[-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]", "(-0.4, -0.2]",
              "(-0.2, 0)", "[0, 0.2)", "[0.2, 0.4)", "[0.4, 0.6)",
              "[0.6, 0.8)", "[0.8, 1.0]")

  heatmap_df <- wcc_obj$results_df |>
    dplyr::mutate(
      wcc_bin = cut(wcc, breaks = breaks, labels = labels,
                    right = TRUE, include.lowest = TRUE)
    )

  ggplot2::ggplot() +
    # The underlying correlation landscape
    ggplot2::geom_tile(
      data = heatmap_df,
      ggplot2::aes(x = tau, y = i, fill = wcc_bin)
    ) +
    ggplot2::scale_fill_brewer(palette = "RdBu", drop = FALSE) +
    # The peak overlay
    ggplot2::geom_point(
      data = peaks_df,
      ggplot2::aes(x = peak_lag, y = i),
      color = "black",
      size = 1.5
    ) +
    ggplot2::geom_path(
      data = peaks_df,
      ggplot2::aes(x = peak_lag, y = i),
      color = "black",
      alpha = 0.5
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Windowed Cross-Correlation with Picked Peaks",
      x = "Lag (tau)",
      y = "Elapsed Time Window (i)"
    )
}
