# Calculate 1D Velocity

Calculates the velocity (rate of change including direction) along a
single axis using finite difference methods.

## Usage

``` r
calc_velocity_1d(
  t,
  x,
  n = 1,
  method = c("central", "forward", "backward"),
  fill_edges = TRUE
)
```

## Arguments

- t:

  A numeric vector representing time.

- x:

  A numeric vector of coordinates.

- n:

  An integer specifying the step size for the difference calculations.
  Default is 1.

- method:

  A character string specifying the method to use: "central", "forward",
  or "backward". Default is "central".

- fill_edges:

  A logical indicating whether to automatically use forward and backward
  differences to estimate velocities at the boundaries when \`method =
  "central"\`. Default is \`TRUE\`.

## Value

A numeric vector of velocities the same length as the input vectors.
