# Calculate Leadership Asymmetry Index

Computes a rolling index of leader-follower asymmetry from optimally
picked lags. A value of 1 indicates 'x' leads entirely, -1 indicates 'y'
leads entirely, and 0 indicates equal leading or simultaneous behavior.

## Usage

``` r
leadership_asymmetry(optima_obj, epoch_size = 10, min_valid = 3)
```

## Arguments

- optima_obj:

  An object of class "wcc_optima" or "wdtw_optima".

- epoch_size:

  A positive integer specifying the number of windows to group together
  to calculate the local asymmetry ratio.

- min_valid:

  A positive integer specifying the minimum number of valid (non-NA)
  optima required in an epoch to compute the index.

## Value

A data frame containing the rolling asymmetry index.
