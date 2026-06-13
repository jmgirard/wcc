#include <Rcpp.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_wdtw_cpp(NumericVector x, NumericVector y,
                            IntegerVector i_vals, IntegerVector tau_vals,
                            int w_max) {

  int n_calcs = i_vals.size();
  NumericVector results(n_calcs, NA_REAL);

  // w_len is the number of elements in the window (w_max + 1)
  int w_len = w_max + 1;

  // Space-optimized Dynamic Programming:
  // We only need the previous row and current row to calculate DTW
  std::vector<double> prev_row(w_len + 1);
  std::vector<double> curr_row(w_len + 1);

  for(int k = 0; k < n_calcs; k++) {
    int i = i_vals[k] - 1; // 0-based indexing for C++
    int tau = tau_vals[k];

    // Safety check to ensure windows do not go out of bounds
    if (i < 0 || i + w_max >= x.size() || i + tau < 0 || i + tau + w_max >= y.size()) {
      continue;
    }

    // Initialize the first DP row with infinity, except for the origin [0]
    std::fill(prev_row.begin(), prev_row.end(), R_PosInf);
    prev_row[0] = 0.0;

    bool has_na = false;

    // Build the DTW cost matrix row by row
    for (int w_x = 1; w_x <= w_len; w_x++) {
      std::fill(curr_row.begin(), curr_row.end(), R_PosInf);

      double val_x = x[i + w_x - 1];
      if (NumericVector::is_na(val_x)) {
        has_na = true;
        break;
      }

      for (int w_y = 1; w_y <= w_len; w_y++) {
        double val_y = y[i + tau + w_y - 1];
        if (NumericVector::is_na(val_y)) {
          has_na = true;
          break;
        }

        // Calculate absolute distance (Euclidean 1D cost)
        double cost = std::abs(val_x - val_y);

        // Find the minimum cost from the previous steps (insertion, deletion, match)
        double min_prev = std::min({curr_row[w_y - 1], prev_row[w_y], prev_row[w_y - 1]});
        curr_row[w_y] = cost + min_prev;
      }

      // The current row becomes the previous row for the next iteration
      prev_row = curr_row;
    }

    // If no missing values were encountered, store the final accumulated distance
    if (!has_na) {
      results[k] = prev_row[w_len];
    }
  }

  return results;
}
