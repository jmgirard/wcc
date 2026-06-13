# Trim Edge Effects from Data

Removes a specified number of observations from the beginning and end of
a vector or data frame. This is highly recommended after applying
zero-phase or polynomial smoothing filters (e.g., Savitzky-Golay) to
remove boundary artifacts.

## Usage

``` r
trim_edges(x, trim_length)
```

## Arguments

- x:

  A numeric vector, matrix, or data frame.

- trim_length:

  An integer specifying the number of observations to remove from both
  ends. A standard rule of thumb is to set this equal to the window size
  used for smoothing.

## Value

An object of the same class as \`x\` with the edges removed.
