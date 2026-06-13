# WCC Workflow

## Analyzing Interpersonal Synchrony with Windowed Cross-Correlation

This vignette walks through a complete Windowed Cross-Correlation (WCC)
analysis using the `bsync` package. WCC is highly effective for
quantifying interpersonal synchrony because it accommodates
non-stationary relationships. Unlike global cross-correlation, WCC
captures how synchronization ebbs and flows over time.

We will cover data simulation, calculating WCC, surrogate testing for
statistical significance, optima extraction, and visualization. For
guidance on selecting the appropriate window sizes and lag parameters,
please see the
[`suggest_wcc_params()`](https://jmgirard.github.io/bsync/reference/suggest_wcc_params.md)
vignette.

### 1. Simulating and Preparing Realistic Data

To demonstrate the workflow, we will simulate a realistic interaction
between two participants (Person A and Person B) captured at 30 Hz. We
will generate smooth continuous data to simulate bodily motion and then
add random measurement noise.

In this scenario, Person A leads the interaction by 15 frames (0.5
seconds) at the start. Over the course of the 60 seconds, the dynamic
smoothly transitions until Person B leads by 15 frames at the end.

``` r

library(bsync)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

set.seed(2026)

# Simulation parameters
fs <- 30
n_frames <- 1800 # 60 seconds of data

# Generate a smooth base signal using a 30-frame moving average.
# This provides enough structure to form a distinct peak within a 90-frame WCC window.
raw_noise <- rnorm(n_frames + 300)
base_signal <- stats::filter(raw_noise, rep(1/30, 30), circular = TRUE)
base_signal <- as.numeric(base_signal)

person_A_raw <- numeric(n_frames)
person_B_raw <- numeric(n_frames)

# Continuous lag shift: Person A leads by 15 frames, smoothly transitioning to B leading
lag_shifts <- round(seq(15, -15, length.out = n_frames))

for (i in 1:n_frames) {
  idx_A <- 150 + i
  idx_B <- 150 + i - lag_shifts[i]

  # Add slight independent noise to mimic realistic measurement error
  person_A_raw[i] <- base_signal[idx_A] + rnorm(1, sd = 0.05)
  person_B_raw[i] <- base_signal[idx_B] + rnorm(1, sd = 0.05)
}
```

#### 1.1 Smoothing and Edge Trimming

Raw kinematic data almost always requires smoothing before analysis.
Here, we apply a Savitzky-Golay filter to iron out the high-frequency
measurement noise.

However, polynomial smoothing algorithms require surrounding data to
calculate an accurate average, which causes mathematical distortion at
the extreme beginning and end of the recording. We use
[`trim_edges()`](https://jmgirard.github.io/bsync/reference/trim_edges.md)
to drop these edge artifacts, ensuring our analysis is built strictly on
stable data. A standard rule of thumb is to trim a length equal to your
smoothing window.

``` r

dyad_data <- data.frame(
  time = seq(0, by = 1/fs, length.out = n_frames),
  person_A_raw = person_A_raw,
  person_B_raw = person_B_raw
) |>
  mutate(
    person_A = smooth_signal(person_A_raw, method = "sgolay", window = 15),
    person_B = smooth_signal(person_B_raw, method = "sgolay", window = 15)
  ) |>
  trim_edges(trim_length = 15)
```

### 2. Calculating Windowed Cross-Correlation

With our data ready, we can run the primary
[`wcc()`](https://jmgirard.github.io/bsync/reference/wcc.md) function.
This function slides a window across the time series and calculates the
cross-correlation at various lags within each window.

We will use a window size of 90 frames (3 seconds) and a maximum lag of
45 frames (1.5 seconds). We will also increment the window by 30 frames
(1 second) to allow for overlap and smooth transitions.

``` r

wcc_results <- wcc(
  x = dyad_data$person_A,
  y = dyad_data$person_B,
  window_size = 90,
  lag_max = 45,
  window_increment = 30,
  lag_increment = 1
)

# View a summary of the results
summary(wcc_results)
#> 
#> ── Windowed Cross-Correlation Analysis ─────────────────────────────────────────
#> Total Windows: 54
#> Total Lags Tested: 91
#> Window Size: 90
#> Max Lag: 45
#> Overall Fisher's Z: 0.5352
#> 
#> ── Cross-Correlation Value Distribution ──
#> 
#>      0%     25%     50%     75%    100% 
#> -0.9269 -0.2582  0.0906  0.4734  0.9960
#> ! 1 missing value (NA) detected.
```

The [`wcc()`](https://jmgirard.github.io/bsync/reference/wcc.md)
function returns a list object of class `wcc_res` containing the results
data frame, the overall Fisher’s Z score, and the input settings. The
overall Fisher’s Z score indicates a positive global correlation, and
the quantile distribution gives us a quick look at the spread of the
correlation values across the entire interaction.

### 3. Surrogate Testing for Significance

Time series data are inherently autocorrelated. Because of this, high
cross-correlation values can sometimes occur purely by chance. To test
if the synchronization we observed is meaningful, we generate a null
distribution using the circular shift method.

``` r

surrogate_results <- wcc_surrogate(
  x = dyad_data$person_A,
  y = dyad_data$person_B,
  window_size = 90,
  lag_max = 45,
  window_increment = 30,
  lag_increment = 1,
  n_surrogates = 100
)

print(surrogate_results)
#> 
#> ── WCC Surrogate Analysis (Pseudo-Synchrony) ───────────────────────────────────
#> Permutations: 100
#> Observed Fisher's Z: 0.5352
#> Average Null Z: 0.4088
#> Empirical p-value: < 0.01
#> ✔ Observed synchrony is significantly greater than chance.
#> ℹ Note: 100 permutations may be too few for stable p-values.
#> Consider setting `n_surrogates >= 1000` for final reporting.
```

The output gives us an empirical p-value by calculating the proportion
of surrogate Fisher’s Z scores that meet or exceed our observed Fisher’s
Z. Because the observed value was higher than all 100 permutations, the
empirical p-value is reported as \< 0.01. **Note:** We use
`n_surrogates = 100` here for speed during exploratory analysis, but to
achieve reliable p-values for publication, it is highly recommended to
run at least 1,000 to 10,000 permutations.

### 4. Optima Extraction

While the heatmap is visually informative, we often want to extract the
precise lags where coordination is strongest within each time window.
The
[`pick_optima()`](https://jmgirard.github.io/bsync/reference/pick_optima.md)
function identifies local maximums within the WCC grid.

``` r

# Extract optima using a local search size of 9
wcc_optima_df <- pick_optima(wcc_results, L_size = 9)

print(wcc_optima_df)
#> 
#> ── WCC Optima Results ──────────────────────────────────────────────────────────
#> Total Optima Found: 54
#> Search Method: local
#> Search Mode: Peaks (Maxima)
#> Local Search Size: 9
#> Strict Monotonic: FALSE
#> Showing the first 5 results:
#>    i optimum_lag optimum_value
#>   46         -10   -0.15961961
#>   76          -2    0.68482860
#>  106          -9    0.01674598
#>  136          -7    0.11919598
#>  166          11    0.97551258
#> # ... with 49 more rows
```

#### 4.1 Tuning the Local Search Window (`L_size`)

Choosing the right `L_size` is crucial for a successful local search.
`L_size` must be an odd integer, and it defines the width of the
neighborhood (in frames) that the algorithm evaluates to confirm a local
maximum.

There is a delicate balance to strike:

- **If `L_size` is too small (e.g., 3):** The algorithm might get
  trapped by tiny noise ripples near lag zero and completely miss the
  true interactive response.
- **If `L_size` is too large (e.g., 45):** The algorithm might skip over
  the fast, immediate interactive response and lock onto a delayed,
  mathematically stronger, but theoretically irrelevant rhythm.

A good rule of thumb is to set `L_size` to capture roughly 0.1 to 0.5
seconds of data. At 30 Hz, an `L_size` between 5 and 15 frames is
usually a great starting point. Because
[`pick_optima()`](https://jmgirard.github.io/bsync/reference/pick_optima.md)
is computationally lightweight, we recommend experimenting with several
values and visually comparing the plots.

### 5. Visualizing the Results

Finally, we can visualize the shifting synchronization landscape. By
passing the `time_step` argument, the axes are automatically converted
from raw frame indices to seconds.

``` r

plot_optima_overlay(
  surface_obj = wcc_results,
  optima_df = wcc_optima_df,
  time_step = 1 / fs,
  show_zero_lag = TRUE
)
```

![](wcc-workflow_files/figure-html/visualization-1.png)

In the resulting plot, you should clearly see a diagonal track of optima
securely tracing the shifting peak lag over time, cleanly avoiding edge
artifacts thanks to our preprocessing.
