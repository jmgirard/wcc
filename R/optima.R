#' Find Optimum (Peak or Valley) in Windowed Analyses
#'
#' @param obj An object of class "wcc_res" or "wdtw_res".
#' @param L_size An odd integer specifying the size of the local search region.
#'   Ignored if `search_method = "global"`. Default is `NULL`.
#' @param strict_monotonic Logical indicating whether to strictly enforce
#'   monotonic flanks around the extremum. Ignored if `search_method = "global"`. Default is FALSE.
#' @param find_min Logical indicating whether to search for local minima instead
#'   of local maxima. If `NULL` (the default), the function automatically
#'   searches for maxima (`FALSE`) for cross-correlation ("wcc_res") and
#'   minima (`TRUE`) for distance metrics ("wdtw_res").
#' @param search_method Character string specifying "local" or "global" search.
#'   "local" searches symmetrically outward from lag 0. "global" searches the
#'   entire window for the absolute extremum. If `NULL`, defaults to "local"
#'   for "wcc_res" and "global" for "wdtw_res".
#' @return A data frame of class "wcc_optima" or "wdtw_optima".
#' @export
pick_optima <- function(obj, L_size = NULL, strict_monotonic = FALSE, find_min = NULL, search_method = NULL) {

  # 1. Dynamically determine metric, class, and default search parameters based on input
  if (inherits(obj, "wcc_res")) {
    metric_col <- "wcc"
    out_class <- "wcc_optima"
    if (is.null(find_min)) find_min <- FALSE
    if (is.null(search_method)) search_method <- "local"
  } else if (inherits(obj, "wdtw_res")) {
    metric_col <- "dtw_dist"
    out_class <- "wdtw_optima"
    if (is.null(find_min)) find_min <- TRUE
    if (is.null(search_method)) search_method <- "global"
  } else {
    stop("Input must be a wcc_res or wdtw_res object.")
  }

  tau_max <- obj$settings$lag_max
  df <- obj$results_df

  # 2. Order data chronologically and by lag
  df <- df[order(df$i, df$tau), ]

  # 3. Branch routing based on chosen search method
  if (search_method == "local") {

    if (is.null(L_size)) {
      stop("An L_size must be provided when using search_method = 'local'.")
    }

    i_factor <- factor(df$i, levels = unique(df$i))
    metric_list <- split(df[[metric_col]], i_factor)
    i_vals <- as.numeric(names(metric_list))

    out_df <- pick_optima_cpp(
      metric_list = metric_list,
      i_vals = i_vals,
      tau_max = tau_max,
      L_size = L_size,
      strict_monotonic = strict_monotonic,
      find_min = find_min
    )

  } else if (search_method == "global") {

    out_df <- df |>
      dplyr::filter(!is.na(.data[[metric_col]]))

    if (find_min) {
      out_df <- out_df |>
        dplyr::slice_min(order_by = .data[[metric_col]], n = 1, with_ties = FALSE, by = i)
    } else {
      out_df <- out_df |>
        dplyr::slice_max(order_by = .data[[metric_col]], n = 1, with_ties = FALSE, by = i)
    }

    out_df <- out_df |>
      dplyr::select(i, optimum_lag = tau, optimum_value = dplyr::all_of(metric_col)) |>
      as.data.frame()

  } else {
    stop("search_method must be either 'local' or 'global'.")
  }

  # 4. Apply attributes and dynamic class
  attr(out_df, "search_method") <- search_method
  attr(out_df, "find_min") <- find_min

  if (search_method == "local") {
    attr(out_df, "L_size") <- L_size
    attr(out_df, "strict_monotonic") <- strict_monotonic
  }

  structure(out_df, class = c(out_class, "data.frame"))
}

# Generic print helper to prevent code duplication
print_optima <- function(x, n, title) {
  search_method <- attr(x, "search_method")
  find_min <- attr(x, "find_min")
  total_optima <- nrow(x)

  cli::cli_h1(title)

  cli::cli_dl(c(
    "Total Optima Found" = "{total_optima}",
    "Search Method" = "{search_method}",
    "Search Mode" = "{ifelse(find_min, 'Valleys (Minima)', 'Peaks (Maxima)')}"
  ))

  if (search_method == "local") {
    cli::cli_dl(c(
      "Local Search Size" = "{attr(x, 'L_size')}",
      "Strict Monotonic" = "{attr(x, 'strict_monotonic')}"
    ))
  }

  if (total_optima > 0) {
    cli::cli_text("Showing the first {min(n, total_optima)} result{?s}:")
    print(utils::head(as.data.frame(x), n), row.names = FALSE)

    if (total_optima > n) {
      remaining <- total_optima - n
      cli::cli_text("{cli::col_grey('# ... with ', remaining, ' more row', ifelse(remaining == 1, '', 's'))}")
    }
  } else {
    cli::cli_alert_info("No optima found matching the criteria.")
  }

  invisible(x)
}

#' Print method for wcc_optima objects
#'
#' @param x An object of class "wcc_optima".
#' @param n An integer specifying how many rows to print. Default is 5.
#' @param ... Additional arguments (not used).
#' @export
print.wcc_optima <- function(x, n = 5, ...) {
  print_optima(x, n, title = "WCC Optima Results")
}

#' Print method for wdtw_optima objects
#'
#' @param x An object of class "wdtw_optima".
#' @param n An integer specifying how many rows to print. Default is 5.
#' @param ... Additional arguments (not used).
#' @export
print.wdtw_optima <- function(x, n = 5, ...) {
  print_optima(x, n, title = "WDTW Optima Results")
}
