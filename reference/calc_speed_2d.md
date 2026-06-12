# Calculate 2D Speed

Calculates the magnitude of the 2D velocity vector (speed) using finite
difference methods.

## Usage

``` r
calc_speed_2d(
  t,
  x,
  y,
  n = 1,
  method = c("central", "forward", "backward"),
  fill_edges = TRUE
)
```

## Arguments

- t:

  A numeric vector representing time.

- x:

  A numeric vector of x-coordinates.

- y:

  A numeric vector of y-coordinates.

- n:

  An integer specifying the step size for the difference calculations.
  Default is 1.

- method:

  A character string specifying the method to use: "central", "forward",
  or "backward". Default is "central".

- fill_edges:

  A logical indicating whether to automatically use forward and backward
  differences to estimate speeds at the boundaries when \`method =
  "central"\`. Default is \`TRUE\`.

## Value

A numeric vector of speeds the same length as the input vectors.
