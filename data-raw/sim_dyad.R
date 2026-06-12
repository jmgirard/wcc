set.seed(2026)

# Simulation parameters
fs <- 50                   # Sampling frequency (50 Hz)
duration <- 30             # Duration in seconds
N <- fs * duration         # Total number of observations
t <- seq(0, duration, length.out = N)

# Base rhythm: e.g., head nodding at 0.5 Hz
freq <- 0.5
base_signal <- sin(2 * pi * freq * t)

# Create a shifting lag (in seconds)
# Starts with A leading by 0.6s, shifts to B leading by 0.6s
lag_shift_sec <- seq(0.6, -0.6, length.out = N)

# Simulate 3D coordinates
# X and Y are mostly stationary noise
x_A <- rnorm(N, mean = 0, sd = 0.05)
y_A <- rnorm(N, mean = 0, sd = 0.05)
x_B <- rnorm(N, mean = 0, sd = 0.05)
y_B <- rnorm(N, mean = 0, sd = 0.05)

# Z contains the main cyclic movement with the shifting lag
z_A <- base_signal + rnorm(N, mean = 0, sd = 0.1)
z_B <- sin(2 * pi * freq * (t - lag_shift_sec)) + rnorm(N, mean = 0, sd = 0.1)

# Combine into a clean data frame
sim_dyad <- data.frame(
  time = t,
  x_A = x_A, y_A = y_A, z_A = z_A,
  x_B = x_B, y_B = y_B, z_B = z_B
)

usethis::use_data(sim_dyad, overwrite = TRUE)
