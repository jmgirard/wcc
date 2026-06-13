# Windowed Dynamic Time Warping

Conduct a windowed dynamic time warping (WDTW) analysis to find the
optimal alignment distance between sliding windows of two time series.

## Usage

``` r
wdtw(
  x,
  y,
  time = NULL,
  window_size,
  lag_max,
  window_increment = 1,
  lag_increment = 1,
  scale_data = TRUE
)
```

## Arguments

- x:

  A numeric vector containing a time series (same length as \`y\`).

- y:

  A numeric vector containing a time series (same length as \`x\`).

- time:

  An optional numeric vector representing the timestamps for the data.
  Must be the same length as \`x\` and \`y\`. If provided, the rolling
  window indices will be mapped directly to these timestamps in the
  results, which is highly recommended to maintain accurate timelines if
  edge artifacts were trimmed prior to analysis. Default is \`NULL\`.

- window_size:

  A positive integer indicating the size of each window.

- lag_max:

  A positive integer indicating the maximum lag to try.

- window_increment:

  A positive integer indicating the window shift increment. (default =
  \`1\`)

- lag_increment:

  A positive integer indicating the lag shift increment. (default =
  \`1\`)

- scale_data:

  A logical indicating whether to z-score standardize both time series
  prior to calculation. Highly recommended for DTW. (default = \`TRUE\`)

## Value

A list object of class "wdtw_res".
