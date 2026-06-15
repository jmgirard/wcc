#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// -----------------------------------------------------------------------------
// Calculate Windowed Granger Causality Core
// -----------------------------------------------------------------------------

// [[Rcpp::export]]
DataFrame calc_wgranger_cpp(NumericVector x, NumericVector y,
                            IntegerVector i_vals, int w_max, int p) {

  int n_calcs = i_vals.size();
  NumericVector f_xy(n_calcs, NA_REAL);
  NumericVector p_xy(n_calcs, NA_REAL);
  NumericVector f_yx(n_calcs, NA_REAL);
  NumericVector p_yx(n_calcs, NA_REAL);

  int w_len = w_max + 1;
  int n_eff = w_len - p;
  int df1 = p;
  int df2 = n_eff - 2 * p - 1;

  if (df2 <= 0) {
    warning("Window size is too small for the requested AR order to maintain degrees of freedom.");
    return DataFrame::create(Named("f_xy") = f_xy, Named("p_xy") = p_xy,
                             Named("f_yx") = f_yx, Named("p_yx") = p_yx);
  }

  for (int k = 0; k < n_calcs; k++) {
    int i = i_vals[k] - 1; // 0-based index

    // Safety bounds check
    if (i < 0 || i + w_max >= x.size() || i + w_max >= y.size()) {
      continue;
    }

    // Check for NAs within the entire window
    bool has_na = false;
    for (int w = 0; w < w_len; w++) {
      if (NumericVector::is_na(x[i + w]) || NumericVector::is_na(y[i + w])) {
        has_na = true;
        break;
      }
    }
    if (has_na) continue;

    // Initialize matrices
    arma::mat X_U(n_eff, 1 + 2 * p, arma::fill::ones); // Unrestricted matrix
    arma::mat X_R_Y(n_eff, 1 + p, arma::fill::ones);   // Restricted matrix for predicting Y
    arma::mat X_R_X(n_eff, 1 + p, arma::fill::ones);   // Restricted matrix for predicting X
    arma::vec Y_vec(n_eff);                            // Target vector Y
    arma::vec X_vec(n_eff);                            // Target vector X

    // Populate matrices element by element for the current window
    for (int row = 0; row < n_eff; row++) {
      int t = i + p + row;
      Y_vec(row) = y[t];
      X_vec(row) = x[t];

      for (int lag = 1; lag <= p; lag++) {
        double x_lag_val = x[t - lag];
        double y_lag_val = y[t - lag];

        // Fill restricted Y model (constant + Y lags)
        X_R_Y(row, lag) = y_lag_val;

        // Fill restricted X model (constant + X lags)
        X_R_X(row, lag) = x_lag_val;

        // Fill unrestricted model (constant + Y lags + X lags)
        X_U(row, lag) = y_lag_val;
        X_U(row, p + lag) = x_lag_val;
      }
    }

    arma::vec beta_U, beta_R;

    // -------------------------------------------------------------------------
    // 1. Does X predict Y?
    // -------------------------------------------------------------------------
    if (arma::solve(beta_U, X_U, Y_vec) && arma::solve(beta_R, X_R_Y, Y_vec)) {
      arma::vec res_U = Y_vec - X_U * beta_U;
      double rss_U = arma::dot(res_U, res_U);

      arma::vec res_R = Y_vec - X_R_Y * beta_R;
      double rss_R = arma::dot(res_R, res_R);

      if (rss_U > 0) {
        double f_stat = ((rss_R - rss_U) / df1) / (rss_U / df2);
        if (f_stat < 0) f_stat = 0; // Float precision safety

        f_xy[k] = f_stat;
        p_xy[k] = R::pf(f_stat, df1, df2, 0, 0); // 0 = upper tail, 0 = no log
      }
    }

    // -------------------------------------------------------------------------
    // 2. Does Y predict X?
    // -------------------------------------------------------------------------
    if (arma::solve(beta_U, X_U, X_vec) && arma::solve(beta_R, X_R_X, X_vec)) {
      arma::vec res_U = X_vec - X_U * beta_U;
      double rss_U = arma::dot(res_U, res_U);

      arma::vec res_R = X_vec - X_R_X * beta_R;
      double rss_R = arma::dot(res_R, res_R);

      if (rss_U > 0) {
        double f_stat = ((rss_R - rss_U) / df1) / (rss_U / df2);
        if (f_stat < 0) f_stat = 0;

        f_yx[k] = f_stat;
        p_yx[k] = R::pf(f_stat, df1, df2, 0, 0);
      }
    }
  }

  return DataFrame::create(
    Named("f_xy") = f_xy, Named("p_xy") = p_xy,
    Named("f_yx") = f_yx, Named("p_yx") = p_yx
  );
}
