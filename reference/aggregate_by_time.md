# Aggregate Time Series Data by Time Bins

Efficiently downsamples time series data by calculating the mean of
values within specified time bins. This is highly recommended for
high-resolution data (e.g., 30Hz OpenFace output) prior to calculating
velocity or running windowed cross-correlation.

## Usage

``` r
aggregate_by_time(data, time_var, bin_width, na.rm = TRUE)
```

## Arguments

- data:

  A data frame containing the time series data.

- time_var:

  The unquoted name of the column containing time values.

- bin_width:

  A numeric value specifying the width of the time bins. This should be
  in the same units as your time variable (e.g., 0.1 for 100ms bins).

- na.rm:

  A logical indicating whether to remove missing values when calculating
  the mean. Default is \`TRUE\`.

## Value

A new data frame with the downsampled time series. The time variable is
updated to represent the center of each bin, and all non-numeric columns
are dropped.
