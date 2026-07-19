# =============================================================================
# Target-mROI optimisation: point-estimate and posterior paths
# =============================================================================
#
# Set each channel's spend to the point where its marginal return (dy/dx)
# equals a target threshold.  This is a per-channel root-finding problem --
# no multi-channel optimiser is needed.
#
# Budget is an *output* (sum of per-channel solutions).
# =============================================================================


# -- Point-estimate path -----------------------------------------------------

opt_mix_target_mroi_point <- function(
    mrms, constr, target_mroi, budget_info, verbose
) {

  channels  <- names(mrms)
  n_weeks   <- budget_info$n_weeks

  response_funs <- purrr::map(mrms, ~mrm_response_function(.x, location = "center"))

  optimal_spend  <- numeric(length(channels))
  channel_mroi   <- numeric(length(channels))
  names(optimal_spend) <- channels
  names(channel_mroi)  <- channels

  for (i in seq_along(channels)) {
    mrm <- mrms[[i]]
    p   <- hlpr_params(mrm, scaled = TRUE)[["center"]]
    fn  <- rm_dispatch(mrm$rc_type)

    optimal_spend[i] <- hlpr_find_mroi_spend(
      curve_fn  = fn,
      b         = p$b,
      c_param   = p$c,
      d         = p$d,
      e         = p$e,
      target_mr = target_mroi,
      lb        = constr$lb[i],
      ub        = constr$ub[i]
    )

    # Achieved mROI at solution
    channel_mroi[i] <- hlpr_numerical_mr(
      fn, optimal_spend[i], p$b, p$c, p$d, p$e
    )
  }

  optimal_kpi   <- purrr::map2_dbl(response_funs, optimal_spend, ~.x(.y))
  weekly_budget <- sum(optimal_spend)

  budget_info$weekly_budget <- weekly_budget
  budget_info$total_budget  <- weekly_budget * n_weeks

  solution <- hlpr_build_solution(
    channels     = channels,
    mrms         = mrms,
    weekly_spend = optimal_spend,
    weekly_kpi   = optimal_kpi,
    n_weeks      = n_weeks
  )

  constraints_df <- tibble::tibble(
    channel = channels,
    lb      = constr$lb,
    ub      = constr$ub,
    x0      = budget_info$current_weekly
  )

  if (verbose) {
    cat("Per-channel mROI root-finding complete.\n")
    cat("Discovered weekly budget:",
        format(round(weekly_budget), big.mark = ","), "\n")
    cat("Total weekly KPI:",
        format(round(sum(optimal_kpi)), big.mark = ","), "\n")
    cat("Per-channel achieved mROI:\n")
    for (ch in channels) {
      cat("  ", ch, ": ", round(channel_mroi[ch], 6), "\n", sep = "")
    }
    if (n_weeks > 1) {
      cat("Total period KPI:",
          format(round(sum(optimal_kpi) * n_weeks), big.mark = ","), "\n")
    }
  }

  list(
    solution       = solution,
    constraints    = constraints_df,
    budget_info    = budget_info,
    channel_mroi   = channel_mroi,
    nloptr_result  = NULL,
    response_funs  = response_funs,
    draws_matrix   = NULL,
    kpi_matrix     = NULL,
    solution_draws = NULL,
    n_draws        = NULL,
    draw_ids       = NULL
  )
}


# -- Posterior-sampling path --------------------------------------------------

opt_mix_target_mroi_posterior <- function(
    mrms, constr, target_mroi, budget_info,
    n_draws, seed, verbose
) {

  channels   <- names(mrms)
  n_channels <- length(mrms)
  n_weeks    <- budget_info$n_weeks

  draws_list <- hlpr_extract_draws(mrms)

  max_available <- min(purrr::map_dbl(draws_list, ~.x$n_draws))
  if (n_draws > max_available) {
    warning("Requested ", n_draws, " draws but only ", max_available,
            " available. Using ", max_available, ".")
    n_draws <- max_available
  }

  if (!is.null(seed)) set.seed(seed)
  draw_ids <- sample(seq_len(max_available), n_draws)

  if (verbose) cat("Per-channel mROI root-finding across", n_draws,
                    "posterior draws...\n")

  # Result matrices: n_draws x n_channels
  draws_matrix <- matrix(NA_real_, nrow = n_draws, ncol = n_channels)
  colnames(draws_matrix) <- channels
  kpi_matrix <- matrix(NA_real_, nrow = n_draws, ncol = n_channels)
  colnames(kpi_matrix) <- channels

  if (verbose) pb <- utils::txtProgressBar(min = 0, max = n_draws, style = 3)

  for (j in seq_len(n_draws)) {
    did <- draw_ids[j]
    for (i in seq_along(draws_list)) {
      dl <- draws_list[[i]]

      # Per-channel root-find for this draw
      draws_matrix[j, i] <- hlpr_find_mroi_spend(
        curve_fn  = dl$curve_fn,
        b         = dl$b[did],
        c_param   = dl$c[did],
        d         = dl$d[did],
        e         = dl$e[did],
        target_mr = target_mroi,
        lb        = constr$lb[i],
        ub        = constr$ub[i]
      )

      # KPI at that spend with this draw's curve
      kpi_matrix[j, i] <- dl$curve_fn(
        draws_matrix[j, i],
        b = dl$b[did], c = dl$c[did],
        d = dl$d[did], e = dl$e[did]
      )
    }
    if (verbose) utils::setTxtProgressBar(pb, j)
  }

  if (verbose) { close(pb); cat("\n") }

  total_kpi <- rowSums(kpi_matrix)

  # Medians and CIs
  spend_median <- apply(draws_matrix, 2, stats::median)
  spend_lower  <- apply(draws_matrix, 2, stats::quantile, 0.025)
  spend_upper  <- apply(draws_matrix, 2, stats::quantile, 0.975)
  kpi_median   <- apply(kpi_matrix, 2, stats::median)
  kpi_lower    <- apply(kpi_matrix, 2, stats::quantile, 0.025)
  kpi_upper    <- apply(kpi_matrix, 2, stats::quantile, 0.975)

  weekly_budget <- sum(spend_median)

  budget_info$weekly_budget <- weekly_budget
  budget_info$total_budget  <- weekly_budget * n_weeks

  # Per-channel mROI at median solution (point-estimate for reporting)
  channel_mroi <- numeric(n_channels)
  names(channel_mroi) <- channels
  for (i in seq_along(draws_list)) {
    dl <- draws_list[[i]]
    # Use overall median draw params for a representative mROI value
    med_idx <- draw_ids[which.min(abs(draws_matrix[, i] - spend_median[i]))]
    channel_mroi[i] <- hlpr_numerical_mr(
      dl$curve_fn, spend_median[i],
      dl$b[med_idx], dl$c[med_idx], dl$d[med_idx], dl$e[med_idx]
    )
  }

  solution <- hlpr_build_solution(
    channels           = channels,
    mrms               = mrms,
    weekly_spend       = spend_median,
    weekly_kpi         = kpi_median,
    weekly_spend_lower = spend_lower,
    weekly_spend_upper = spend_upper,
    weekly_kpi_lower   = kpi_lower,
    weekly_kpi_upper   = kpi_upper,
    n_weeks            = n_weeks
  )

  total_spend_vec <- rowSums(draws_matrix)
  solution_draws <- tibble::as_tibble(as.data.frame(draws_matrix)) |>
    dplyr::mutate(
      draw        = dplyr::row_number(),
      total_kpi   = total_kpi,
      total_spend = total_spend_vec,
      cost_per    = total_spend_vec / total_kpi
    )

  if (verbose) {
    cat("Posterior mROI optimization complete.\n")
    cat("Median discovered weekly budget:",
        format(round(weekly_budget), big.mark = ","), "\n")
    cat("Median total weekly KPI:",
        format(round(sum(kpi_median)), big.mark = ","), "\n")
  }

  list(
    solution       = solution,
    constraints    = tibble::tibble(
      channel = channels, lb = constr$lb, ub = constr$ub,
      x0 = budget_info$current_weekly
    ),
    budget_info    = budget_info,
    channel_mroi   = channel_mroi,
    nloptr_result  = NULL,
    response_funs  = NULL,
    draws_matrix   = draws_matrix,
    kpi_matrix     = kpi_matrix,
    solution_draws = solution_draws,
    n_draws        = n_draws,
    draw_ids       = draw_ids
  )
}
