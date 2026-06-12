# Plot Windowed Cross-Correlation with Peak Overlay

Plot Windowed Cross-Correlation with Peak Overlay

## Usage

``` r
plot_peaks_overlay(
  wcc_obj,
  peaks_df,
  time_step = 1,
  line_color = "black",
  point_fill = "black",
  point_stroke = "white",
  color_low = "#B2182B",
  color_mid = "#F7F7F7",
  color_high = "#2166AC",
  show_zero_lag = TRUE,
  zero_line_color = "black"
)
```

## Arguments

- wcc_obj:

  An object of class "wcc_res" containing the windowed cross-correlation
  results.

- peaks_df:

  A data frame of class "wcc_peaks" containing the peaks to overlay.

- time_step:

  A numeric value specifying the duration of each index. Default is 1.

- line_color:

  Character string specifying the color of the connecting line. Default
  is "black".

- point_fill:

  Character string specifying the inner fill of the peak points. Default
  is "black".

- point_stroke:

  Character string specifying the outer outline of the peak points.
  Default is "white".

- color_low:

  Character string specifying the heatmap color for a correlation of -1.
  Default is "#B2182B".

- color_mid:

  Character string specifying the heatmap color for a correlation of 0.
  Default is "#F7F7F7".

- color_high:

  Character string specifying the heatmap color for a correlation of 1.
  Default is "#2166AC".

- show_zero_lag:

  Logical indicating whether to draw a vertical line at lag = 0. Default
  is \`TRUE\`.

- zero_line_color:

  Character string specifying the color of the zero-lag line. Default is
  "black".
