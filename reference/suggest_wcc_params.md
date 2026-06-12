# Suggest WCC Hyperparameters

Calculates principled starting values for Windowed Cross-Correlation
parameters based on the sampling rate of the data and the theoretical
timing of the behaviors.

## Usage

``` r
suggest_wcc_params(
  sample_rate,
  event_duration_sec = 2,
  max_delay_sec = 3,
  overlap_pct = 0.5
)
```

## Arguments

- sample_rate:

  A numeric value indicating the sampling rate in Hertz (frames per
  second).

- event_duration_sec:

  The expected duration of a single behavioral event in seconds. Default
  is 2 (typical for brief conversational gestures).

- max_delay_sec:

  The maximum plausible reaction time between participants in seconds.
  Default is 3.

- overlap_pct:

  The desired percentage of overlap between consecutive time windows.
  Default is 0.5 (50 percent overlap).

## Value

A list of recommended parameters ready to be passed to \`wcc()\`.
