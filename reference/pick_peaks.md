# Find Peak Windowed Cross-Correlations

Find Peak Windowed Cross-Correlations

## Usage

``` r
pick_peaks(wcc_obj, L_size, strict_monotonic = FALSE)
```

## Arguments

- wcc_obj:

  An object of class "wcc_res" returned by \`wcc()\`.

- L_size:

  An odd integer specifying the size of the local search region.

- strict_monotonic:

  Logical indicating whether to strictly enforce decreasing values on
  the flanks of the peak. (default = FALSE)

## Value

A data frame of class "wcc_peaks" containing the elapsed time indices,
the peak lags, and the peak correlation values.
