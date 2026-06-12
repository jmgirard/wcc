#' Plot wcc_res object
#'
#' @param x An object of class "wcc_res".
#' @param time_step A numeric value specifying the duration of each index.
#'   If not 1, axes will be converted from raw indices to time units. Default is 1.
#' @param color_low Character string specifying the color for a correlation of -1. Default is "#B2182B" (Deep Red).
#' @param color_mid Character string specifying the color for a correlation of 0. Default is "#F7F7F7" (Off-white).
#' @param color_high Character string specifying the color for a correlation of 1. Default is "#2166AC" (Deep Blue).
#' @param ... Additional arguments (not used).
#' @export
plot.wcc_res <- function(x, time_step = 1,
                         color_low = "#B2182B",
                         color_mid = "#F7F7F7",
                         color_high = "#2166AC", ...) {

  df <- x$results_df

  # Scale indices to time if a step size is provided
  if (time_step != 1) {
    df$tau <- df$tau * time_step
    df$i <- df$i * time_step
    x_label <- "Lag (\u03c4) in Seconds"
    y_label <- "Elapsed Time (Seconds)"
  } else {
    x_label <- "Lag (\u03c4) Index"
    y_label <- "Elapsed Time Window Index"
  }

  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = tau, y = i, fill = wcc)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = color_low,
      mid = color_mid,
      high = color_high,
      midpoint = 0,
      limits = c(-1, 1),
      na.value = "grey80",
      name = "WCC (r)"
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    )

  p
}

#' Plot Windowed Cross-Correlation with Peak Overlay
#'
#' @param wcc_obj An object of class "wcc_res" containing the windowed cross-correlation results.
#' @param peaks_df A data frame of class "wcc_peaks" containing the peaks to overlay.
#' @param time_step A numeric value specifying the duration of each index. Default is 1.
#' @param line_color Character string specifying the color of the connecting line. Default is "black".
#' @param point_fill Character string specifying the inner fill of the peak points. Default is "black".
#' @param point_stroke Character string specifying the outer outline of the peak points. Default is "white".
#' @param color_low Character string specifying the heatmap color for a correlation of -1. Default is "#B2182B".
#' @param color_mid Character string specifying the heatmap color for a correlation of 0. Default is "#F7F7F7".
#' @param color_high Character string specifying the heatmap color for a correlation of 1. Default is "#2166AC".
#' @export
plot_peaks_overlay <- function(wcc_obj, peaks_df,
                               time_step = 1,
                               line_color = "black",
                               point_fill = "black",
                               point_stroke = "white",
                               color_low = "#B2182B",
                               color_mid = "#F7F7F7",
                               color_high = "#2166AC") {

  # Call the base plot method to generate the heatmap and handle axes and colors
  p_base <- plot(wcc_obj, time_step = time_step,
                 color_low = color_low, color_mid = color_mid, color_high = color_high)

  # Scale peak coordinates to match the base plot
  if (time_step != 1) {
    peaks_df$peak_lag <- peaks_df$peak_lag * time_step
    peaks_df$i <- peaks_df$i * time_step
  }

  p_final <- p_base +
    # The connecting line (drawn first so points sit on top)
    ggplot2::geom_path(
      data = peaks_df,
      ggplot2::aes(x = peak_lag, y = i),
      color = line_color,
      alpha = 0.6,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    # The points with an outline for contrast
    ggplot2::geom_point(
      data = peaks_df,
      ggplot2::aes(x = peak_lag, y = i),
      shape = 21,
      fill = point_fill,
      color = point_stroke,
      size = 1.5,
      stroke = 0.4,
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      title = "Windowed Cross-Correlation with Picked Peaks"
    )

  p_final
}
