# Find Peak (or Valley) Windowed Cross-Correlations

Find Peak (or Valley) Windowed Cross-Correlations

## Usage

``` r
pick_peaks(wcc_obj, L_size, strict_monotonic = FALSE, find_min = FALSE)
```

## Arguments

- wcc_obj:

  An object of class "wcc_res" or "wdtw_res".

- L_size:

  An odd integer specifying the size of the local search region.

- strict_monotonic:

  Logical indicating whether to strictly enforce monotonic flanks around
  the extremum. (default = FALSE)

- find_min:

  Logical indicating whether to search for local minima instead of local
  maxima. Required for distance metrics like DTW. (default = FALSE)

## Value

A data frame of class "wcc_peaks".
