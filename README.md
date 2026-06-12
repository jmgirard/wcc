
# wcc

[![R-CMD-check](https://github.com/jmgirard/wcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmgirard/wcc/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

First, we load the package and calculate the 1D directional velocity for
the rhythmic movements on the Z-axis.

``` r
library(wcc)
library(dplyr)

data("sim_dyad")

# Step 1: Calculate 1D directional velocity for both individuals
df <- sim_dyad |>
  mutate(
    vel_A = v_z(time, z_A),
    vel_B = v_z(time, z_B)
  )
```

Next, we calculate the windowed cross-correlations and extract the
shifting peaks.

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

# Step 3: Extract the Peaks
peaks <- pick_peaks(
  wcc_obj = wcc_results,
  L_size = 5,
  strict_monotonic = FALSE
)
```

Finally, we can visualize the resulting correlation landscape. The black
dots and lines represent the algorithm successfully tracking the
shifting lag of maximum association over the course of the interaction.

``` r
# Step 4: Plot the landscape and overlay the shifting peaks
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
