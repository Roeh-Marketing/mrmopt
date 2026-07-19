# =============================================================================
# Target-ROI optimisation: point-estimate and posterior paths
# =============================================================================
#
# Maximise total (incremental) KPI subject to portfolio ROI >= target.
# Budget is free -- the equality constraint from max_kpi is replaced by an
# inequality constraint on ROI.
#
# ROI  = sum(f_i(x_i) - f_i(0)) / sum(x_i)  >=  target_roi
# <=>  target_roi * sum(x) - sum(f(x) - f(0))  <=  0   (nloptr g_ineq form)
# =============================================================================


# -- Point-estimate path -----------------------------------------------------

opt_mix_target_roi_point <- function(
    mrms, constr, target_roi, budget_info,
    xtol_rel, maxeval, verbose
) {

  channels  <- names(mrms)
  n_weeks   <- budget_info$n_weeks

  # Response functions from posterior median
  response_funs <- purrr::map(mrms, ~mrm_response_function(.x, location = "center"))

  # Baseline KPI per channel: f(0)
  baselines <- purrr::map2_dbl(response_funs, mrms, function(fn, mrm) {
    p <- hlpr_params(mrm, scaled = TRUE)[["center"]]
    hlpr_baseline_kpi(rm_dispatch(mrm$rc_type), p$b, p$c, p$d, p$e,
                      mrm$rc_type)
  })

  # Objective: maximise total KPI  (nloptr minimises, so negate)
  eval_f <- function(x) -sum(purrr::map2_dbl(response_funs, x, ~.x(.y)))

  # Inequality constraint: target_roi * sum(x) - sum(incr_kpi) <= 0
  eval_g_ineq <- function(x) {
    kpi_vals <- purrr::map2_dbl(response_funs, x, ~.x(.y))
    incr_kpi <- sum(kpi_vals - baselines)
    target_roi * sum(x) - incr_kpi
  }

  # Starting point: current weekly spend (clamped to bounds)
  x0 <- pmin(pmax(budget_info$current_weekly, constr$lb), constr$ub)

  res <- hlpr_opt_solve(
    eval_f      = eval_f,
    x0          = x0,
    lb          = constr$lb,
    ub          = constr$ub,
    eval_g_eq   = NULL,
    eval_g_ineq = eval_g_ineq,
    xtol_rel    = xtol_rel,
    maxeval     = maxeval
  )

  optimal_spend <- res$solution
  optimal_kpi   <- purrr::map2_dbl(response_funs, optimal_spend, ~.x(.y))
  weekly_budget <- sum(optimal_spend)

  # Achieved ROI at solution
  incr_kpi     <- sum(optimal_kpi - baselines)
  achieved_roi <- if (weekly_budget > 0) incr_kpi / weekly_budget else NA_real_

  # Update budget info with discovered budget
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
    x0      = x0
  )

  if (verbose) {
    cat("Optimization converged (status:", res$status, ")\n")
    cat("Discovered weekly budget:",
        format(round(weekly_budget), big.mark = ","), "\n")
    cat("Achieved ROI:", round(achieved_roi, 4), "\n")
    cat("Total weekly KPI:",
        format(round(sum(optimal_kpi)), big.mark = ","), "\n")
    if (n_weeks > 1) {
      cat("Total period KPI:",
          format(round(sum(optimal_kpi) * n_weeks), big.mark = ","), "\n")
    }
  }

  list(
    solution       = solution,
    constraints    = constraints_df,
    budget_info    = budget_info,
    achieved_roi   = achieved_roi,
    nloptr_result  = res,
    response_funs  = response_funs,
    draws_matrix   = NULL,
    kpi_matrix     = NULL,
    solution_draws = NULL,
    n_draws        = NULL,
    draw_ids       = NULL
  )
}


# -- Posterior-sampling path --------------------------------------------------

opt_mix_target_roi_posterior <- function(
    mrms, constr, target_roi, budget_info,
    n_draws, seed, parallel,
    xtol_rel, maxeval, verbose
) {

  channels   <- names(mrms)
  n_channels <- length(mrms)
  n_weeks    <- budget_info$n_weeks

  # Pre-extract and unscale all draws

  draws_list <- hlpr_extract_draws(mrms)

  max_available <- min(purrr::map_dbl(draws_list, ~.x$n_draws))
  if (n_draws > max_available) {
    warning("Requested ", n_draws, " draws but only ", max_available,
            " available. Using ", max_available, ".")
    n_draws <- max_available
  }

  if (!is.null(seed)) set.seed(seed)
  draw_ids <- sample(seq_len(max_available), n_draws)

  # Pre-compute per-channel baseline vectors (f(0) per draw)
  baseline_vecs <- purrr::map(seq_along(draws_list), function(i) {
    dl <- draws_list[[i]]
    rc_type <- mrms[[i]]$rc_type
    hlpr_baseline_kpi_vec(dl$curve_fn, dl$b, dl$c, dl$d, dl$e, rc_type)
  })

  # Starting point: current spend clamped to bounds
  x0 <- pmin(pmax(budget_info$current_weekly, constr$lb), constr$ub)

  # Build objective + ROI constraint for a single draw
  make_draw_fns <- function(draw_id) {
    eval_f <- function(x) {
      total <- 0
      for (i in seq_along(draws_list)) {
        dl <- draws_list[[i]]
        total <- total + dl$curve_fn(
          x[i],
          b = dl$b[draw_id], c = dl$c[draw_id],
          d = dl$d[draw_id], e = dl$e[draw_id]
        )
      }
      -total
    }

    eval_g_ineq <- function(x) {
      incr <- 0
      for (i in seq_along(draws_list)) {
        dl <- draws_list[[i]]
        kpi_i <- dl$curve_fn(
          x[i],
          b = dl$b[draw_id], c = dl$c[draw_id],
          d = dl$d[draw_id], e = dl$e[draw_id]
        )
        incr <- incr + (kpi_i - baseline_vecs[[i]][draw_id])
      }
      target_roi * sum(x) - incr
    }

    list(eval_f = eval_f, eval_g_ineq = eval_g_ineq)
  }

  solve_one <- function(j) {
    fns <- make_draw_fns(draw_ids[j])
    res <- hlpr_opt_solve(
      eval_f      = fns$eval_f,
      x0          = x0,
      lb          = constr$lb,
      ub          = constr$ub,
      eval_g_eq   = NULL,
      eval_g_ineq = fns$eval_g_ineq,
      xtol_rel    = xtol_rel,
      maxeval     = maxeval
    )
    res$solution
  }

  # Run across draws
  if (verbose) cat("Optimizing across", n_draws, "posterior draws (target ROI)...\n")

  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    results_list <- future.apply::future_lapply(
      seq_len(n_draws), solve_one, future.seed = TRUE
    )
  } else {
    if (parallel) warning("future.apply not installed; falling back to sequential.")
    if (verbose) pb <- utils::txtProgressBar(min = 0, max = n_draws, style = 3)
    results_list <- vector("list", n_draws)
    for (j in seq_len(n_draws)) {
      results_list[[j]] <- solve_one(j)
      if (verbose) utils::setTxtProgressBar(pb, j)
    }
    if (verbose) { close(pb); cat("\n") }
  }

  # Assemble results
  draws_matrix <- do.call(rbind, results_list)
  colnames(draws_matrix) <- channels

  # Compute KPI + ROI for each draw
  kpi_matrix <- matrix(NA_real_, nrow = n_draws, ncol = n_channels)
  colnames(kpi_matrix) <- channels
  total_kpi <- numeric(n_draws)
  roi_vec   <- numeric(n_draws)

  for (j in seq_len(n_draws)) {
    incr <- 0
    for (i in seq_along(draws_list)) {
      dl <- draws_list[[i]]
      kpi_matrix[j, i] <- dl$curve_fn(
        draws_matrix[j, i],
        b = dl$b[draw_ids[j]], c = dl$c[draw_ids[j]],
        d = dl$d[draw_ids[j]], e = dl$e[draw_ids[j]]
      )
      incr <- incr + (kpi_matrix[j, i] - baseline_vecs[[i]][draw_ids[j]])
    }
    total_kpi[j] <- sum(kpi_matrix[j, ])
    roi_vec[j]   <- incr / sum(draws_matrix[j, ])
  }

  # Medians and CIs
  spend_median <- apply(draws_matrix, 2, stats::median)
  spend_lower  <- apply(draws_matrix, 2, stats::quantile, 0.025)
  spend_upper  <- apply(draws_matrix, 2, stats::quantile, 0.975)
  kpi_median   <- apply(kpi_matrix, 2, stats::median)
  kpi_lower    <- apply(kpi_matrix, 2, stats::quantile, 0.025)
  kpi_upper    <- apply(kpi_matrix, 2, stats::quantile, 0.975)

  weekly_budget <- sum(spend_median)
  achieved_roi  <- stats::median(roi_vec)

  budget_info$weekly_budget <- weekly_budget
  budget_info$total_budget  <- weekly_budget * n_weeks

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
      cost_per    = total_spend_vec / total_kpi,
      roi         = roi_vec
    )

  if (verbose) {
    cat("Posterior optimization complete.\n")
    cat("Median discovered weekly budget:",
        format(round(weekly_budget), big.mark = ","), "\n")
    cat("Median achieved ROI:", round(achieved_roi, 4), "\n")
    cat("Median total weekly KPI:",
        format(round(sum(kpi_median)), big.mark = ","), "\n")
  }

  list(
    solution       = solution,
    constraints    = tibble::tibble(
      channel = channels, lb = constr$lb, ub = constr$ub, x0 = x0
    ),
    budget_info    = budget_info,
    achieved_roi   = achieved_roi,
    nloptr_result  = NULL,
    response_funs  = NULL,
    draws_matrix   = draws_matrix,
    kpi_matrix     = kpi_matrix,
    solution_draws = solution_draws,
    n_draws        = n_draws,
    draw_ids       = draw_ids
  )
}
