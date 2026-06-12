
# wcc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jmgirard/wcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmgirard/wcc/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmgirard/wcc/graph/badge.svg)](https://app.codecov.io/gh/jmgirard/wcc)
[![test-coverage](https://github.com/jmgirard/wcc/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jmgirard/wcc/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of **wcc** is to provide a modern, high-efficiency R
implementation of the windowed cross-correlation and peak-picking
algorithms.

Traditional cross-correlation assumes that the association between two
time series is stationary over time. However, in many psychological and
behavioral contexts (such as interpersonal conversation or synchronized
movement), the lead-lag relationship between individuals is highly
dynamic. This package allows researchers to quantify these nonstationary
associations by evaluating correlations across sliding windows of
elapsed time.

## Installation

You can install the development version of wcc from
[GitHub](https://github.com/jmgirard/wcc) with:

``` r
# install.packages("devtools")
devtools::install_github("jmgirard/wcc")
```

## Example Workflow

The following example demonstrates the complete `wcc` pipeline. We will
use the included `sim_dyad` dataset, which contains 30 seconds of
simulated 3D motion tracking data for two individuals. In this
simulation, Person A begins by leading a rhythmic movement, they
synchronize in the middle, and Person B takes the lead by the end.

First, we load the package and prepare the data. We highly recommend
smoothing positional data before calculating velocities to prevent
high-frequency noise from amplifying and skewing the cross-correlations.
Here we apply a Savitzky-Golay filter and then calculate the 1D
directional velocity on the Z-axis.

``` r
library(wcc)
library(dplyr)

data("sim_dyad")

# Step 1: Smooth the raw position data and calculate 1D velocity
df <- sim_dyad |>
  mutate(
    z_A_smooth = smooth_signal(z_A, method = "sgolay", window = 5),
    z_B_smooth = smooth_signal(z_B, method = "sgolay", window = 5),
    vel_A = calc_velocity_1d(time, z_A_smooth, fill_edges = TRUE),
    vel_B = calc_velocity_1d(time, z_B_smooth, fill_edges = TRUE)
  )
```

Next, we calculate the windowed cross-correlations. Calling `summary()`
on the resulting object provides a clean, formatted overview of the
analysis settings and the distribution of correlation values.

``` r
# Step 2: Calculate Windowed Cross-Correlations
wcc_results <- wcc(
  x = df$vel_A,
  y = df$vel_B,
  window_size = 150,
  lag_max = 75,
  window_increment = 25,
  lag_increment = 1,
  na.rm = TRUE
)

# View the summary
summary(wcc_results)
#> 
#> ── Windowed Cross-Correlation Analysis ─────────────────────────────────────────
#> Total Windows: 87
#> Total Lags Tested: 151
#> Window Size: 150
#> Max Lag: 75
#> Overall Fisher's Z: 1.1007
#> 
#> ── Cross-Correlation Value Distribution ──
#> 
#>      0%     25%     50%     75%    100% 
#> -0.9988 -0.6828  0.0474  0.7220  0.9987
#> ! 78 missing values (NA) detected.
```

Once the cross-correlations are calculated, we extract the specific lags
that represent the peak association within each time window. Printing
the peak object displays the peak-picking metadata alongside the first
few results.

``` r
# Step 3: Extract the Peaks
peaks <- pick_peaks(
  wcc_obj = wcc_results,
  L_size = 5,
  strict_monotonic = FALSE
)

# View the peak results
peaks
#> 
#> ── WCC Peak Picking Results ────────────────────────────────────────────────────
#> Total Peaks Found: 87
#> Local Search Size: 5
#> Strict Monotonic: FALSE
#> Showing the first 5 peaks:
#>    i peak_lag peak_value
#>   76       34  0.9985194
#>  101       34  0.9981981
#>  126       32  0.9971209
#>  151       31  0.9979207
#>  176       31  0.9982943
#> # ... with 82 more rows
```

Finally, we visualize the resulting correlation landscape. The
underlying heatmap represents the cross-correlation values at each lag
and elapsed time window. The overlaid black dots and lines demonstrate
the algorithm successfully tracking the shifting lag of maximum
association over the course of the interaction.

``` r
# Step 4: Plot the correlation landscape and overlay the shifting peaks
plot_peaks_overlay(wcc_results, peaks)
```

<img src="man/figures/README-example-plot-1.png" alt="" width="100%" />

## Citation

If you use **wcc** in your research, please cite the package:

``` r
citation("wcc")
```

This package builds upon the original windowed cross-correlation
methodology proposed in the following paper:

- Boker, S. M., Rotondo, J. L., Xu, M., & King, K. (2002). Windowed
  cross-correlation and peak picking for the analysis of variability in
  the association between behavioral time series. *Psychological
  Methods*, *7*(3), 338.
