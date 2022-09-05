
plot.wcc <- function(results_obj) {
  df <- dplyr::mutate(
    results_obj$results_df,
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
    ggplot2::scale_fill_brewer(palette = "RdBu")
  p
}
