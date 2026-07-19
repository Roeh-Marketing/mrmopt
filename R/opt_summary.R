#' Print a formatted summary of an opt_mix_result object
#'
#' Displays a formatted summary of the optimization result, including budget
#' info, optimal allocation per channel, and totals vs. current performance.
#'
#' @param x An `opt_mix_result` object returned by [opt_mix()].
#' @param ... Additional arguments (ignored).
#' @return The object `x`, invisibly. Called for its side effect of printing
#'   formatted output. Use [opt_table()] to retrieve a tidy tibble.
#' @export

opt_summary <- function(x, ...) {

  if (!inherits(x, "opt_mix_result")) {
    stop("`x` must be an opt_mix_result object returned by opt_mix().")
  }

  dollar <- function(v) paste0("$", formatC(v, format = "f", big.mark = ",", digits = 0))
  comma  <- function(v) formatC(v, format = "f", big.mark = ",", digits = 0)
  pct    <- function(v) paste0(ifelse(v >= 0, "+", ""), round(v * 100, 1), "%")

  sol <- x$solution
  bi  <- x$budget_info
  is_posterior <- x$method == "posterior"

  objective <- x$objective %||% "max_kpi"
  flexible_budget <- objective %in% c("target_roi", "target_mroi",
                                      "target_cpk", "target_mcpk")

  # --- Header ---
  method_label <- if (is_posterior) {
    paste0("posterior, ", x$n_draws, " draws")
  } else {
    "point"
  }

  obj_label <- switch(
    objective,
    max_kpi     = "",
    target_roi  = paste0(", target ROI \u2265 ", round(x$target_roi, 4)),
    target_mroi = paste0(", target mROI = ", round(x$target_mroi, 6)),
    target_cpk  = paste0(", target CPK \u2264 ", dollar(x$target_cpk)),
    target_mcpk = paste0(", target mCPK \u2264 ", dollar(x$target_mcpk))
  )
  cat(cli_rule(paste0("Optimization Result (", method_label, obj_label, ")")), "\n")

  budget_word <- if (flexible_budget) "Optimal budget" else "Budget"
  cat(budget_word, ": ", dollar(bi$weekly_budget), "/week", sep = "")
  if (bi$n_weeks > 1) {
    cat("  |  ", dollar(bi$total_budget), " over ", bi$n_weeks, " weeks", sep = "")
  }
  cat("  |  Channels: ", nrow(sol), "\n", sep = "")

  # --- Allocation table ---
  cat(cli_rule("Optimal Allocation"), "\n")

  max_name <- max(nchar(sol$channel), nchar("Channel"))
  name_w <- max_name + 2

  if (is_posterior) {
    header <- sprintf(
      "  %-*s  %14s  %24s  %10s  %7s",
      name_w, "Channel", "Weekly Spend", "[95% CI]", "CP", "Share"
    )
    cat(header, "\n")
    for (i in seq_len(nrow(sol))) {
      r <- sol[i, ]
      ci <- paste0("[", dollar(r$weekly_spend_lower), " \u2013 ", dollar(r$weekly_spend_upper), "]")
      cat(sprintf(
        "  %-*s  %14s  %24s  %10s  %6s\n",
        name_w, r$channel,
        dollar(r$weekly_spend), ci,
        dollar(r$cost_per),
        pct(r$spend_share - 1 + 1)
      ))
    }
  } else {
    header <- sprintf(
      "  %-*s  %14s  %12s  %10s  %7s",
      name_w, "Channel", "Weekly Spend", "Weekly KPI", "CP", "Share"
    )
    cat(header, "\n")
    for (i in seq_len(nrow(sol))) {
      r <- sol[i, ]
      cat(sprintf(
        "  %-*s  %14s  %12s  %10s  %5.1f%%\n",
        name_w, r$channel,
        dollar(r$weekly_spend), comma(r$weekly_kpi),
        dollar(r$cost_per),
        r$spend_share * 100
      ))
    }
  }

  # --- Totals ---
  cat(cli_rule("Totals"), "\n")

  total_opt_spend <- sum(sol$weekly_spend)
  total_opt_kpi   <- sum(sol$weekly_kpi)
  total_cur_spend <- sum(sol$current_weekly_spend)
  total_cur_kpi   <- sum(sol$current_weekly_kpi)
  avg_cp_opt <- total_opt_spend / total_opt_kpi
  avg_cp_cur <- total_cur_spend / total_cur_kpi

  cat("  Optimal:  Spend ", dollar(total_opt_spend),
      "  |  KPI ", comma(total_opt_kpi),
      "  |  Avg CP ", dollar(avg_cp_opt), "\n", sep = "")
  cat("  Current:  Spend ", dollar(total_cur_spend),
      "  |  KPI ", comma(total_cur_kpi),
      "  |  Avg CP ", dollar(avg_cp_cur), "\n", sep = "")

  kpi_change <- (total_opt_kpi / total_cur_kpi) - 1
  cp_change  <- avg_cp_opt - avg_cp_cur

  cat("  Change:   KPI ", pct(kpi_change),
      "  |  CP ", ifelse(cp_change >= 0, "+", ""),
      dollar(abs(cp_change)), "\n", sep = "")

  if (flexible_budget) {
    spend_change <- total_opt_spend - total_cur_spend
    cat("  Budget \u0394: ", ifelse(spend_change >= 0, "+", ""),
        dollar(abs(spend_change)), " (", pct(spend_change / total_cur_spend),
        ")\n", sep = "")
  }

  # Objective-specific reporting
  if (objective == "target_roi" && !is.null(x$achieved_roi)) {
    cat("  Achieved ROI: ", round(x$achieved_roi, 4), "\n", sep = "")
  }
  if (objective == "target_cpk" && !is.null(x$achieved_cpk)) {
    cat("  Achieved CPK: ", dollar(x$achieved_cpk), "\n", sep = "")
  }
  if (objective == "target_mroi" && !is.null(x$channel_mroi)) {
    cat("  Per-channel mROI at solution:\n")
    for (ch in names(x$channel_mroi)) {
      cat("    ", ch, ": ", round(x$channel_mroi[ch], 6), "\n", sep = "")
    }
  }
  if (objective == "target_mcpk" && !is.null(x$channel_mcpk)) {
    cat("  Per-channel marginal CPK at solution:\n")
    for (ch in names(x$channel_mcpk)) {
      cat("    ", ch, ": ", dollar(x$channel_mcpk[ch]), "\n", sep = "")
    }
  }

  if (bi$n_weeks > 1) {
    cat("\n  Period (", bi$n_weeks, " weeks): ",
        dollar(total_opt_spend * bi$n_weeks), " spend  |  ",
        comma(total_opt_kpi * bi$n_weeks), " KPI\n", sep = "")
  }

  invisible(x)
}
