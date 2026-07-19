#' Tidy comparison table for opt_mix_result objects
#'
#' Returns a tidy tibble comparing current vs. optimal allocation per channel
#' with absolute and percentage deltas for spend, KPI, cost-per, and shares.
#'
#' @param x An `opt_mix_result` object returned by [opt_mix()].
#' @param ... Additional arguments (ignored).
#' @return A tibble with one row per channel (plus a TOTAL row) comparing
#'   current and optimal performance, including spend/KPI/units deltas
#'   (absolute and percent), cost-per change, and share shifts.
#' @export

opt_table <- function(x, ...) {

  if (!inherits(x, "opt_mix_result")) {
    stop("`x` must be an opt_mix_result object returned by opt_mix().")
  }

  sol <- x$solution
  n_weeks <- x$budget_info$n_weeks

  has_units <- !all(is.na(sol$weekly_units))

  result <- tibble::tibble(
    channel = sol$channel,

    # Spend
    current_spend = sol$current_weekly_spend,
    optimal_spend = sol$weekly_spend,
    spend_delta = sol$weekly_spend - sol$current_weekly_spend,
    spend_delta_pct = (sol$weekly_spend / sol$current_weekly_spend) - 1,

    # KPI
    current_kpi = sol$current_weekly_kpi,
    optimal_kpi = sol$weekly_kpi,
    kpi_delta = sol$weekly_kpi - sol$current_weekly_kpi,
    kpi_delta_pct = (sol$weekly_kpi / sol$current_weekly_kpi) - 1,

    # Cost per
    current_cp = sol$current_cost_per,
    optimal_cp = sol$cost_per,
    cp_delta = sol$cost_per - sol$current_cost_per,

    # Shares
    current_spend_share = sol$current_spend_share,
    optimal_spend_share = sol$spend_share,
    spend_share_shift = sol$spend_share - sol$current_spend_share,

    current_kpi_share = sol$current_kpi_share,
    optimal_kpi_share = sol$kpi_share,
    kpi_share_shift = sol$kpi_share - sol$current_kpi_share
  )

  # Units (only when available)
  if (has_units) {
    result$current_units <- sol$current_weekly_units
    result$optimal_units <- sol$weekly_units
    result$units_delta <- sol$weekly_units - sol$current_weekly_units
    result$units_delta_pct <- (sol$weekly_units / sol$current_weekly_units) - 1

    result$current_rr <- sol$current_rr
    result$optimal_rr <- sol$rr
    result$rr_delta <- sol$rr - sol$current_rr
  }

  # Add totals row
  totals <- tibble::tibble(
    channel = "TOTAL",
    current_spend = sum(sol$current_weekly_spend),
    optimal_spend = sum(sol$weekly_spend),
    spend_delta = sum(sol$weekly_spend) - sum(sol$current_weekly_spend),
    spend_delta_pct = (sum(sol$weekly_spend) / sum(sol$current_weekly_spend)) - 1,
    current_kpi = sum(sol$current_weekly_kpi),
    optimal_kpi = sum(sol$weekly_kpi),
    kpi_delta = sum(sol$weekly_kpi) - sum(sol$current_weekly_kpi),
    kpi_delta_pct = (sum(sol$weekly_kpi) / sum(sol$current_weekly_kpi)) - 1,
    current_cp = sum(sol$current_weekly_spend) / sum(sol$current_weekly_kpi),
    optimal_cp = sum(sol$weekly_spend) / sum(sol$weekly_kpi),
    cp_delta = (sum(sol$weekly_spend) / sum(sol$weekly_kpi)) -
               (sum(sol$current_weekly_spend) / sum(sol$current_weekly_kpi)),
    current_spend_share = 1,
    optimal_spend_share = 1,
    spend_share_shift = 0,
    current_kpi_share = 1,
    optimal_kpi_share = 1,
    kpi_share_shift = 0
  )

  if (has_units) {
    totals$current_units <- sum(sol$current_weekly_units, na.rm = TRUE)
    totals$optimal_units <- sum(sol$weekly_units, na.rm = TRUE)
    totals$units_delta <- totals$optimal_units - totals$current_units
    totals$units_delta_pct <- (totals$optimal_units / totals$current_units) - 1
    totals$current_rr <- totals$current_kpi / totals$current_units
    totals$optimal_rr <- totals$optimal_kpi / totals$optimal_units
    totals$rr_delta <- totals$optimal_rr - totals$current_rr
  }

  result <- dplyr::bind_rows(result, totals)

  # Store metadata as attributes
  attr(result, "method") <- x$method
  attr(result, "objective") <- x$objective %||% "max_kpi"
  attr(result, "n_weeks") <- n_weeks
  attr(result, "n_draws") <- x$n_draws

  result
}
