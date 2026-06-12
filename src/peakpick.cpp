#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pick_peaks_cpp(List wcc_list, NumericVector i_vals, int tau_max, int L_size, bool strict_monotonic = false) {

  if (L_size % 2 == 0) {
    stop("L_size must be an odd integer to possess a true center element.");
  }

  int n = wcc_list.size();
  NumericVector out_lag(n, NA_REAL);
  NumericVector out_val(n, NA_REAL);

  int c = tau_max; // 0-based C++ index representing lag 0
  int half_L = (L_size - 1) / 2;
  int expected_length = 2 * tau_max + 1;

  // The absolute furthest lag from the center we can verify with flanks
  int max_distance = tau_max - half_L;

  for (int k = 0; k < n; ++k) {
    NumericVector rvec = wcc_list[k];

    if (rvec.size() != expected_length) {
      stop("A correlation vector does not match the expected length for tau_max.");
    }

    // Helper lambda to evaluate a local region of length L_size
    auto check_window = [&](int start_idx) -> bool {
      double max_val = R_NegInf;
      int max_idx = -1;

      // Find the maximum value and its index, ignoring NAs
      for (int i = 0; i < L_size; ++i) {
        double val = rvec[start_idx + i];
        if (!NumericVector::is_na(val)) {
          if (max_idx == -1 || val > max_val) {
            max_val = val;
            max_idx = i;
          }
        }
      }

      // The peak must be exactly at the center of the local window
      if (max_idx != half_L) return false;

      // Monotonic flank check
      if (strict_monotonic) {
        // Left flank must be strictly increasing
        for (int i = 1; i <= half_L; ++i) {
          double prev = rvec[start_idx + i - 1];
          double curr = rvec[start_idx + i];
          if (NumericVector::is_na(prev) || NumericVector::is_na(curr) || curr <= prev) return false;
        }
        // Right flank must be strictly decreasing
        for (int i = half_L + 1; i < L_size; ++i) {
          double prev = rvec[start_idx + i - 1];
          double curr = rvec[start_idx + i];
          if (NumericVector::is_na(prev) || NumericVector::is_na(curr) || curr >= prev) return false;
        }
      }

      return true;
    };

    // Search symmetrically outward from lag 0
    for (int dist = 0; dist <= max_distance; ++dist) {

      // Check Negative Lag (L1 equivalent)
      int left_center = c - dist;
      if (check_window(left_center - half_L)) {
        out_lag[k] = left_center - c; // -dist
        out_val[k] = rvec[left_center];
        break;
      }

      // Check Positive Lag (L2 equivalent)
      if (dist > 0) {
        int right_center = c + dist;
        if (check_window(right_center - half_L)) {
          out_lag[k] = right_center - c; // +dist
          out_val[k] = rvec[right_center];
          break;
        }
      }
    }
  }

  return DataFrame::create(Named("i") = i_vals,
                           Named("peak_lag") = out_lag,
                           Named("peak_value") = out_val);
}
