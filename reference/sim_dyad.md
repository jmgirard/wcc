# Simulated Dyadic 3D Positional Data

A simulated dataset containing 3D positional coordinates for two
interacting individuals (Person A and Person B) over a 30-second
duration. The data is sampled at 80 Hz and features a shifting lead-lag
relationship along the z-axis. Specifically, Person A initially leads
the interaction by 0.5 seconds, and this smoothly transitions to Person
B leading by 0.5 seconds. Realistic low-level sensor jitter is applied
to all coordinates to mimic continuous behavioral measures like motion
capture or computer vision tracking.

## Usage

``` r
sim_dyad
```

## Format

A data frame with 2400 rows and 7 variables:

- time:

  Elapsed time in seconds.

- x_A:

  Simulated x-axis coordinate for Person A (noise only).

- y_A:

  Simulated y-axis coordinate for Person A (noise only).

- z_A:

  Simulated z-axis coordinate for Person A (0.5 Hz sine wave + noise).

- x_B:

  Simulated x-axis coordinate for Person B (noise only).

- y_B:

  Simulated y-axis coordinate for Person B (noise only).

- z_B:

  Simulated z-axis coordinate for Person B (0.5 Hz sine wave + shifting
  lag + noise).
