#' @export plot.wcc_res
#' @export
#' @importFrom graphics plot
plot.wcc_res <- function(results_obj) {

  breaks <- c(-Inf, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, Inf)
  labels <- c("[-1, -0.8]", "(-0.8, -0.6]", "(-0.6, -0.4]", "(-0.4, -0.2]",
              "(-0.2, 0)", "[0, 0.2)", "[0.2, 0.4)", "[0.4, 0.6)",
              "[0.6, 0.8)", "[0.8, 1.0]")

  df <- results_obj$results_df |>
    dplyr::mutate(
      wcc_bin = cut(wcc, breaks = breaks, labels = labels,
                    right = TRUE, include.lowest = TRUE)
    )

  ggplot2::ggplot(data = df, ggplot2::aes(x = tau, y = i, fill = wcc_bin)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_brewer(palette = "RdBu", drop = FALSE)
}

#' Plot Windowed Cross-Correlation with Peak Overlay
#'
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
