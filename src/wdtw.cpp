#include <Rcpp.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// -----------------------------------------------------------------------------
// Calculate Windowed Dynamic Time Warping Core
// -----------------------------------------------------------------------------

// [[Rcpp::export]]
NumericVector calc_wdtw_cpp(NumericVector x, NumericVector y,
                            IntegerVector i_vals, IntegerVector tau_vals,
                            int w_max, bool use_l2, bool local_scale) {

  int n_calcs = i_vals.size();
  NumericVector results(n_calcs, NA_REAL);
  int w_len = w_max + 1;

  // Space-optimized Dynamic Programming
  std::vector<double> prev_row(w_len + 1);
  std::vector<double> curr_row(w_len + 1);

  for(int k = 0; k < n_calcs; k++) {
    int i = i_vals[k] - 1; // 0-based indexing
    int tau = tau_vals[k];

    // Safety check to ensure windows do not go out of bounds
    if (i < 0 || i + w_max >= x.size() || i + tau < 0 || i + tau + w_max >= y.size()) {
      continue;
    }

    bool has_na = false;
    double mean_x = 0.0, sd_x = 1.0;
    double mean_y = 0.0, sd_y = 1.0;

    // Pre-calculate local window statistics if local scaling is requested
    if (local_scale) {
      double sum_x = 0.0, sum_x2 = 0.0;
      double sum_y = 0.0, sum_y2 = 0.0;
      int valid_n = 0;

      for (int w = 0; w <= w_max; w++) {
        double vx = x[i + w];
        double vy = y[i + tau + w];

        if (NumericVector::is_na(vx) || NumericVector::is_na(vy)) {
          has_na = true;
          break;
        }

        sum_x += vx;
        sum_x2 += vx * vx;
        sum_y += vy;
        sum_y2 += vy * vy;
        valid_n++;
      }

      if (has_na) continue; // Skip to next window if NAs are found

      if (valid_n > 1) {
        mean_x = sum_x / valid_n;
        double var_x = (sum_x2 - (sum_x * sum_x) / valid_n) / (valid_n - 1);
        sd_x = var_x > 0 ? std::sqrt(var_x) : 1.0;

        mean_y = sum_y / valid_n;
        double var_y = (sum_y2 - (sum_y * sum_y) / valid_n) / (valid_n - 1);
        sd_y = var_y > 0 ? std::sqrt(var_y) : 1.0;
      }
    }

    // Initialize the first DP row with infinity, except for the origin
    std::fill(prev_row.begin(), prev_row.end(), R_PosInf);
    prev_row[0] = 0.0;

    // Build the DTW cost matrix row by row
    for (int w_x = 1; w_x <= w_len; w_x++) {
      std::fill(curr_row.begin(), curr_row.end(), R_PosInf);

      double val_x = x[i + w_x - 1];
      if (NumericVector::is_na(val_x)) {
        has_na = true;
        break;
      }
      if (local_scale) {
        val_x = (val_x - mean_x) / sd_x;
      }

      for (int w_y = 1; w_y <= w_len; w_y++) {
        double val_y = y[i + tau + w_y - 1];
        if (NumericVector::is_na(val_y)) {
          has_na = true;
          break;
        }
        if (local_scale) {
          val_y = (val_y - mean_y) / sd_y;
        }

        // Apply L1 or L2 distance metric
        double cost = use_l2 ? ((val_x - val_y) * (val_x - val_y)) : std::abs(val_x - val_y);

        double min_prev = std::min({curr_row[w_y - 1], prev_row[w_y], prev_row[w_y - 1]});
        curr_row[w_y] = cost + min_prev;
      }

      prev_row = curr_row;
    }

    if (!has_na) {
      results[k] = prev_row[w_len];
    }
  }

  return results;
}
