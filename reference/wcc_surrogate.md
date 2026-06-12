# Calculate Surrogate Windowed Cross-Correlations

Generates a null distribution of Windowed Cross-Correlation (WCC) values
using the circular shift method. This tests whether the observed
synchronization is significantly greater than what would be expected by
chance given the inherent autocorrelation of the individual time series.

## Usage

``` r
wcc_surrogate(
  x,
  y,
  window_size,
  lag_max,
  window_increment = 1,
  lag_increment = 1,
  na.rm = TRUE,
  n_surrogates = 100
)
```

## Arguments

- x:

  A numeric vector containing a time series.

- y:

  A numeric vector containing a time series.

- window_size:

  A positive integer indicating the size of each window.

- lag_max:

  A positive integer indicating the maximum lag to try.

- window_increment:

  A positive integer indicating the window shift increment. Default is
  1.

- lag_increment:

  A positive integer indicating the lag shift increment. Default is 1.

- na.rm:

  A logical indicating whether to remove missing values. Default is
  \`TRUE\`.

- n_surrogates:

  An integer specifying the number of surrogate permutations to run.
  Default is 100.

## Value

A list object of class "wcc_surr" containing the observed Fisher's Z,
the distribution of surrogate Z scores, the empirical p-value, and
settings.
