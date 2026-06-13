# WDTW Workflow

## Analyzing Interpersonal Synchrony with Windowed Dynamic Time Warping

This vignette walks through a complete Windowed Dynamic Time Warping
(WDTW) analysis using the `bsync` package. While Windowed
Cross-Correlation (WCC) is excellent for capturing linear relationships,
WDTW excels at finding similarities in signals that might be distorted,
stretched, or compressed in time.

It is important to remember that WDTW calculates a distance metric
rather than a correlation. This means that lower values indicate
stronger synchronization (a smaller distance between the signals).

### 1. Simulating Realistic Dyadic Data

To demonstrate the workflow, we will simulate an interaction between two
participants (Person A and Person B) captured at 30 Hz. We will generate
smooth continuous data to simulate bodily motion.

In this scenario, Person A leads the interaction by 15 frames (0.5
seconds) at the start. Over the course of the 60 seconds, the dynamic
smoothly transitions until Person B leads by 15 frames at the end.

``` r

library(bsync)

set.seed(2026)

# Simulation parameters
fs <- 30
n_frames <- 1800 # 60 seconds of data

# Generate a smooth base signal using a 30-frame moving average.
raw_noise <- rnorm(n_frames + 300)
base_signal <- stats::filter(raw_noise, rep(1/30, 30), circular = TRUE)
base_signal <- as.numeric(base_signal)

person_A <- numeric(n_frames)
person_B <- numeric(n_frames)

# Continuous lag shift: Person A leads by 15 frames, smoothly transitioning to B leading
lag_shifts <- round(seq(15, -15, length.out = n_frames))

for (i in 1:n_frames) {
  idx_A <- 150 + i
  idx_B <- 150 + i - lag_shifts[i]

  # Add slight independent noise to mimic realistic measurement error
  person_A[i] <- base_signal[idx_A] + rnorm(1, sd = 0.05)
  person_B[i] <- base_signal[idx_B] + rnorm(1, sd = 0.05)
}

dyad_data <- data.frame(
  time = seq(0, by = 1/fs, length.out = n_frames),
  person_A = person_A,
  person_B = person_B
)
```

### 2. Calculating Windowed Dynamic Time Warping

With our data ready, we can run the primary
[`wdtw()`](https://jmgirard.github.io/bsync/reference/wdtw.md) function.
This function slides a window across the time series and calculates the
DTW alignment distance at various lags within each window.

We strongly recommend leaving `scale_data = TRUE` (the default). DTW
distances are highly sensitive to the scale of the input variables, and
z-score standardizing the data ensures that the resulting cost matrix is
driven by the structural shape of the behaviors rather than arbitrary
measurement units.

``` r

wdtw_results <- wdtw(
  x = dyad_data$person_A,
  y = dyad_data$person_B,
  window_size = 90,
  lag_max = 45,
  window_increment = 30,
  lag_increment = 1,
  scale_data = TRUE
)

# View a summary of the results
print(wdtw_results)
#> 
#> ── Windowed Dynamic Time Warping Analysis ──────────────────────────────────────
#> Total Windows: 55
#> Total Lags Tested: 91
#> Window Size: 90
#> Max Lag: 45
#> Overall Mean Distance: 36.6055
```

The [`wdtw()`](https://jmgirard.github.io/bsync/reference/wdtw.md)
function returns a list object of class `wdtw_res` containing the
results data frame, the overall mean distance, and the input settings.

### 3. Valley Picking (Extracting the Minima)

While the full distance matrix is informative, we often want to extract
the precise lags where the alignment is optimal within each time window.
For correlation (WCC), we look for peaks. For distance metrics (WDTW),
we look for valleys.

The
[`pick_peaks()`](https://jmgirard.github.io/bsync/reference/pick_peaks.md)
function handles this by setting the `find_min` argument to `TRUE`.

``` r

# Extract optimal alignment lags (valleys) using a local search size of 5
wdtw_valleys_df <- pick_peaks(wdtw_results, L_size = 5, find_min = TRUE)

print(wdtw_valleys_df)
#> 
#> ── WCC Peak Picking Results ────────────────────────────────────────────────────
#> Total Extremes Found: 55
#> Local Search Size: 5
#> Strict Monotonic: FALSE
#> Search Mode: Valleys (Minima)
#> Showing the first 5 results:
#>    i peak_lag peak_value
#>   46       -5   22.53230
#>   76       -8   26.66298
#>  106       -3   28.71353
#>  136        1   23.02804
#>  166       -6   25.28022
#> # ... with 50 more rows
```

This returns a `wcc_peaks` data frame containing the elapsed time
indices, the optimal lags, and the corresponding DTW distance values.
The console output confirms that the search mode was successfully set to
“Valleys (Minima)”.

### 4. Visualizing the Results

Finally, we can visualize the shifting synchronization landscape. The
[`plot_peaks_overlay()`](https://jmgirard.github.io/bsync/reference/plot_peaks_overlay.md)
function detects that it is working with a `wdtw_res` object and
automatically applies a sequential color palette to map the distances.

By passing the `time_step` argument, the axes are automatically
converted from raw frame indices to seconds.

``` r

plot_peaks_overlay(
  wcc_obj = wdtw_results,
  peaks_df = wdtw_valleys_df,
  time_step = 1 / fs,
  show_zero_lag = TRUE
)
```

![](wdtw-workflow_files/figure-html/visualization-1.png)

In the resulting plot, darker colors represent smaller distances
(stronger synchrony). You will see a clear, dark diagonal track
corresponding to the simulated shift in the dyad’s interaction. The
overlaid points map exactly to the lowest alignment costs, smoothly
tracing the transition from Person A leading to Person B leading.
