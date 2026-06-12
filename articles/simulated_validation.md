# Simulated Validation

``` r

library(bsync)

# 1. Simulate data with a known lag shift
set.seed(42)
time_steps <- 1:1000

# Base signal (e.g., a sine wave representing rhythmic movement)
signal <- sin(time_steps * 0.1)

# Create x and y with an abrupt lag shift at time step 500
x <- signal + rnorm(1000, mean = 0, sd = 0.2)

# For the first half, y lags x by 0. For the second half, y lags x by +5.
y_first_half <- signal[1:500] + rnorm(500, mean = 0, sd = 0.2)
y_second_half <- signal[(501 - 5):(1000 - 5)] + rnorm(500, mean = 0, sd = 0.2)
y <- c(y_first_half, y_second_half)

# 2. Run your package pipeline
wcc_sim <- wcc(
  x = x,
  y = y,
  window_size = 50,
  lag_max = 10,
  window_increment = 10
)

sim_peaks <- pick_peaks(wcc_sim, L_size = 5, strict_monotonic = FALSE)
```

``` r

# 3. Validate the results
sim_peaks
#> 
#> ── WCC Peak Picking Results ────────────────────────────────────────────────────
#> Total Peaks Found: 94
#> Local Search Size: 5
#> Strict Monotonic: FALSE
#> Showing the first 5 peaks:
#>   i peak_lag peak_value
#>  11       -1  0.9397980
#>  21        0  0.9152112
#>  31        0  0.9428786
#>  41       -1  0.9485425
#>  51        0  0.9061785
#> # ... with 89 more rows
```
