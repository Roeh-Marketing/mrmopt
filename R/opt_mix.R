#' Optimize media mix allocation across channels
#'
#' Given a set of fitted response curve models, find the optimal spend
#' allocation. Supports point-estimate optimization (fast, single solution)
#' and posterior-sampling optimization (slower, returns a distribution of
#' solutions reflecting Bayesian uncertainty).
#'
#' Five objectives are available (the last two are cost-per-KPI
#' convenience wrappers around the ROI objectives):
#' \describe{
#'   \item{`"max_kpi"` (default)}{Maximize total KPI given a fixed budget
#'     constraint.}
#'   \item{`"target_roi"`}{Maximize incremental KPI while keeping portfolio
#'     ROI \eqn{\ge} `target_roi`.
#'     The budget is an *output*, not an input.
#'     ROI is defined as total incremental KPI (f(x) - f(0)) divided by
#'     total spend.}
#'   \item{`"target_mroi"`}{Set each channel's spend to the point where its
#'     marginal return (dy/dx) equals `target_mroi`.
#'     The budget is an *output* — the sum of per-channel solutions.}
#'   \item{`"target_cpk"`}{Same optimization as `"target_roi"` but framed
#'     as a cost-per-KPI ceiling: keep average CPK \eqn{\le} `target_cpk`.
#'     Internally converts to `target_roi = 1 / target_cpk`. Useful when the
#'     KPI is a non-revenue metric (leads, visits, etc.).}
#'   \item{`"target_mcpk"`}{Same optimization as `"target_mroi"` but framed
#'     as a marginal cost-per-KPI threshold: stop spending on a channel once
#'     the marginal cost of the next KPI unit exceeds `target_mcpk`.
#'     Internally converts to `target_mroi = 1 / target_mcpk`.}
#' }
#'
#' @param mrms A named list of `mrmfit` objects (one per channel).
#' @param method One of `"point"` (default) or `"posterior"`. Point uses
#'   posterior median parameters; posterior optimizes over multiple MCMC draws.
#' @param objective One of `"max_kpi"` (default), `"target_roi"`,
#'   `"target_mroi"`, `"target_cpk"`, or `"target_mcpk"`.
#'   See **Details**.
#' @param budget Total budget for the period. Required for `"max_kpi"`;
#'   ignored (with a message) for flexible-budget objectives. If `NULL`
#'   (default), inferred from total current weekly spend.
#' @param target_roi Numeric; minimum acceptable portfolio ROI
#'   (incremental KPI / spend). Required when `objective = "target_roi"`.
#' @param target_mroi Numeric; target marginal return per dollar. Required
#'   when `objective = "target_mroi"`.
#' @param target_cpk Numeric; maximum acceptable average cost per KPI unit
#'   (spend / incremental KPI). Required when `objective = "target_cpk"`.
#' @param target_mcpk Numeric; maximum acceptable marginal cost per KPI unit.
#'   Required when `objective = "target_mcpk"`.
#' @param n_weeks Number of weeks the budget covers. Used to convert a period
#'   budget to weekly optimization. Default `1` (budget is already weekly).
#'   Convenience values: use `52` for annual, `13` for quarterly, `4` for
#'   monthly.
#' @param constraints A data frame with per-channel bounds. Must contain
#'   columns `channel`, `min_spend`, `max_spend`. If `NULL` (default),
#'   constraints are auto-generated from model return rate ranges. Note:
#'   share-based constraints (`min_share`/`max_share`) are only supported
#'   for `objective = "max_kpi"`.
#' @param bounds_multiplier When `constraints` is `NULL`, multiplier applied
#'   to auto-detected spend ranges. Default `3`.
#' @param n_draws Number of posterior draws to optimize over when
#'   `method = "posterior"`. Default `200`.
#' @param seed Random seed for draw sampling. Default `NULL`.
#' @param parallel Logical; use `future.apply` for parallel posterior
#'   optimization. Default `FALSE`. Requires a `future::plan()` to be set.
#'   Ignored for `"target_mroi"` (per-channel root-finding is fast).
#' @param xtol_rel Relative tolerance for nloptr. Default `1e-8`.
#'   Ignored for `"target_mroi"`.
#' @param maxeval Maximum nloptr evaluations per solve. Default `1000`.
#'   Ignored for `"target_mroi"`.
#' @param verbose Print progress information. Default `TRUE`.
#'
#' @return An `opt_mix_result` S3 object. All objectives return the same
#'   top-level structure:
#'   \itemize{
#'     \item `$solution` — tibble with one row per channel containing current
#'       and optimal spend, KPI, units, cost-per, response rate, and share
#'       columns. Posterior results include `_lower`/`_upper` CI columns.
#'     \item `$constraints` — tibble: channel, lb, ub, x0.
#'     \item `$budget_info` — list: total_budget, weekly_budget, n_weeks,
#'       current_weekly. For flexible-budget objectives (`target_roi`,
#'       `target_mroi`), `weekly_budget` and `total_budget` are *outputs*
#'       computed from the optimal solution.
#'     \item `$method` — `"point"` or `"posterior"`.
#'     \item `$objective` — the objective used.
#'     \item `$mrms` — the named list of `mrmfit` models.
#'   }
#'   Objective-specific fields:
#'   \itemize{
#'     \item `$target_roi` / `$achieved_roi` — for `"target_roi"` objective.
#'     \item `$target_mroi` / `$channel_mroi` — for `"target_mroi"` objective.
#'     \item `$target_cpk` / `$achieved_cpk` — for `"target_cpk"` objective.
#'       The underlying `$target_roi` / `$achieved_roi` are also available.
#'     \item `$target_mcpk` / `$channel_mcpk` — for `"target_mcpk"` objective.
#'       The underlying `$target_mroi` / `$channel_mroi` are also available.
#'   }
#'   Point-only fields: `$nloptr_result`, `$response_funs`. \cr
#'   Posterior-only fields: `$draws_matrix`, `$kpi_matrix`,
#'   `$solution_draws`, `$n_draws`, `$draw_ids`.
#'
#'   Use [print()] or [summary()] for a formatted console summary,
#'   [opt_table()] for a tidy comparison tibble, and [plot()] or the
#'   standalone `opt_plot_*` functions for visualizations.
#'
#' @import nloptr
#' @importFrom purrr map map_dbl map2_dbl
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

opt_mix <- function(
    mrms,
    method = c("point", "posterior"),
    objective = c("max_kpi", "target_roi", "target_mroi",
                  "target_cpk", "target_mcpk"),
    budget = NULL,
    target_roi = NULL,
    target_mroi = NULL,
    target_cpk = NULL,
    target_mcpk = NULL,
    n_weeks = 1,
    constraints = NULL,
    bounds_multiplier = 3,
    n_draws = 200,
    seed = NULL,
    parallel = FALSE,
    xtol_rel = 1e-8,
    maxeval = 1000,
    verbose = TRUE
) {

  method    <- match.arg(method)
  objective <- match.arg(objective)

  # opt_mix is an intended consumer of as_mrmfit_list() output, so suppress the
  # per-unit "view" warnings emitted by the functions it calls internally.
  .uv <- options(mrmopt.unit_view_depth =
                   getOption("mrmopt.unit_view_depth", 0L) + 1L)
  on.exit(options(.uv), add = TRUE)

  # --- CPK / mCPK conversion (reciprocal wrappers) ---
  # Store the original objective for reporting, then map to the underlying
  # ROI/mROI solver.
  cpk_mode <- objective %in% c("target_cpk", "target_mcpk")

  if (objective == "target_cpk") {
    if (is.null(target_cpk) || !is.numeric(target_cpk) || target_cpk <= 0) {
      stop("`target_cpk` must be a positive number when objective = 'target_cpk'.")
    }
    target_roi <- 1 / target_cpk
  }

  if (objective == "target_mcpk") {
    if (is.null(target_mcpk) || !is.numeric(target_mcpk) || target_mcpk <= 0) {
      stop("`target_mcpk` must be a positive number when objective = 'target_mcpk'.")
    }
    target_mroi <- 1 / target_mcpk
  }

  # Map CPK objectives to their underlying ROI solver
  solver_objective <- switch(
    objective,
    target_cpk  = "target_roi",
    target_mcpk = "target_mroi",
    objective
  )

  flexible_budget <- solver_objective %in% c("target_roi", "target_mroi")

  # --- Validation ---
  if (!is.list(mrms) || !all(sapply(mrms, inherits, "mrmfit"))) {
    stop("`mrms` must be a named list of mrmfit objects.")
  }
  if (is.null(names(mrms)) || any(names(mrms) == "")) {
    stop("`mrms` must be a named list. Use channel names as list names.")
  }

  # Objective-specific validation (after CPK conversion)

  if (solver_objective == "target_roi") {
    if (is.null(target_roi) || !is.numeric(target_roi) || target_roi <= 0) {
      stop("`target_roi` must be a positive number when objective = 'target_roi'.")
    }
    if (!is.null(budget)) {
      message("Note: `budget` is ignored for objective = '", objective,
              "' (budget is an output).")
    }
  }

  if (solver_objective == "target_mroi") {
    if (is.null(target_mroi) || !is.numeric(target_mroi) || target_mroi <= 0) {
      stop("`target_mroi` must be a positive number when objective = 'target_mroi'.")
    }
    if (!is.null(budget)) {
      message("Note: `budget` is ignored for objective = '", objective,
              "' (budget is an output).")
    }
  }

  # Warn about share constraints with flexible budget
  if (flexible_budget && !is.null(constraints)) {
    has_shares <- any(c("min_share", "max_share") %in% names(constraints))
    if (has_shares) {
      warning("Share-based constraints (min_share/max_share) are ignored for ",
              "flexible-budget objectives ('", objective, "'). ",
              "Only absolute bounds (min_spend/max_spend) are used.")
    }
  }

  channels <- names(mrms)
  n_channels <- length(mrms)

  # --- Budget ---
  current_weekly <- map_dbl(mrms, hlpr_get_weekly_spend)

  if (flexible_budget) {
    # Budget is an output; use current spend as reference for constraint parsing
    weekly_budget <- sum(current_weekly)
  } else if (is.null(budget)) {
    weekly_budget <- sum(current_weekly)
    if (verbose) {
      cat("No budget supplied; using total current weekly spend:",
          format(round(weekly_budget), big.mark = ","), "\n")
    }
  } else {
    weekly_budget <- budget / n_weeks
  }

  budget_info <- list(
    total_budget = weekly_budget * n_weeks,
    weekly_budget = weekly_budget,
    n_weeks = n_weeks,
    current_weekly = current_weekly
  )

  # --- Constraints ---
  if (is.null(constraints)) {
    constr <- hlpr_auto_constraints(mrms, weekly_budget, bounds_multiplier)
  } else {
    if (flexible_budget) {
      # Strip share columns for flexible-budget objectives
      share_cols <- intersect(c("min_share", "max_share"), names(constraints))
      if (length(share_cols) > 0) {
        constraints <- constraints[, !names(constraints) %in% share_cols,
                                   drop = FALSE]
      }
    }
    constr <- hlpr_parse_constraints(constraints, channels, weekly_budget)
  }

  if (verbose) {
    dollar_fmt <- function(v) paste0("$", formatC(v, format = "f",
                                                   big.mark = ",", digits = 0))
    obj_label <- switch(
      objective,
      max_kpi     = "max_kpi",
      target_roi  = paste0("target_roi >= ", target_roi),
      target_mroi = paste0("target_mroi = ", target_mroi),
      target_cpk  = paste0("target_cpk <= ", dollar_fmt(target_cpk)),
      target_mcpk = paste0("target_mcpk <= ", dollar_fmt(target_mcpk))
    )
    cat("\nOptimization setup:\n")
    cat("  Channels:      ", n_channels, "\n")
    cat("  Method:        ", method, "\n")
    cat("  Objective:     ", obj_label, "\n")
    if (!flexible_budget) {
      cat("  Weekly budget: ", format(round(weekly_budget), big.mark = ","), "\n")
      if (n_weeks > 1) {
        cat("  Period budget: ", format(round(weekly_budget * n_weeks), big.mark = ","),
            " (", n_weeks, " weeks)\n")
      }
    }
    cat("\n")
  }

  # --- Dispatch (CPK objectives map to their ROI solver) ---
  if (solver_objective == "max_kpi") {

    if (method == "point") {
      result <- opt_mix_point(
        mrms, constr, weekly_budget, budget_info,
        xtol_rel, maxeval, verbose
      )
    } else {
      result <- opt_mix_posterior(
        mrms, constr, weekly_budget, budget_info,
        n_draws, seed, parallel,
        xtol_rel, maxeval, verbose
      )
    }

  } else if (solver_objective == "target_roi") {

    if (method == "point") {
      result <- opt_mix_target_roi_point(
        mrms, constr, target_roi, budget_info,
        xtol_rel, maxeval, verbose
      )
    } else {
      result <- opt_mix_target_roi_posterior(
        mrms, constr, target_roi, budget_info,
        n_draws, seed, parallel,
        xtol_rel, maxeval, verbose
      )
    }

  } else if (solver_objective == "target_mroi") {

    if (method == "point") {
      result <- opt_mix_target_mroi_point(
        mrms, constr, target_mroi, budget_info, verbose
      )
    } else {
      result <- opt_mix_target_mroi_posterior(
        mrms, constr, target_mroi, budget_info,
        n_draws, seed, verbose
      )
    }

  }

  result$method    <- method
  result$objective <- objective
  result$mrms      <- mrms

  # Objective-specific metadata (NULL when not applicable)
  # ROI-family: target_roi and target_cpk both run the ROI solver
  if (solver_objective == "target_roi") {
    result$target_roi <- target_roi
    # achieved_roi is set by the internal function
  } else {
    result$target_roi   <- NULL
    result$achieved_roi <- NULL
  }

  # mROI-family: target_mroi and target_mcpk both run the mROI solver
  if (solver_objective == "target_mroi") {
    result$target_mroi <- target_mroi
    # channel_mroi is set by the internal function
  } else {
    result$target_mroi  <- NULL
    result$channel_mroi <- NULL
  }

  # CPK convenience fields (reciprocals of ROI values)
  if (objective == "target_cpk") {
    result$target_cpk   <- target_cpk
    result$achieved_cpk <- if (!is.null(result$achieved_roi) &&
                               result$achieved_roi > 0) {
      1 / result$achieved_roi
    } else {
      NA_real_
    }
  } else {
    result$target_cpk   <- NULL
    result$achieved_cpk <- NULL
  }

  if (objective == "target_mcpk") {
    result$target_mcpk  <- target_mcpk
    result$channel_mcpk <- if (!is.null(result$channel_mroi)) {
      1 / result$channel_mroi
    } else {
      NULL
    }
  } else {
    result$target_mcpk  <- NULL
    result$channel_mcpk <- NULL
  }

  class(result) <- "opt_mix_result"
  result
}


# =============================================================================
# Point-estimate path
# =============================================================================

opt_mix_point <- function(
    mrms, constr, weekly_budget, budget_info,
    xtol_rel, maxeval, verbose
) {

  channels <- names(mrms)
  n_weeks <- budget_info$n_weeks

  # Build objective from posterior median response functions
  response_funs <- map(mrms, ~mrm_response_function(.x, location = "center"))
  eval_f <- function(x) -sum(map2_dbl(response_funs, x, ~.x(.y)))

  # Budget equality constraint: sum(x) - budget = 0
  eval_g_eq <- function(x) sum(x) - weekly_budget

  res <- hlpr_opt_solve(
    eval_f = eval_f,
    x0 = constr$x0,
    lb = constr$lb,
    ub = constr$ub,
    eval_g_eq = eval_g_eq,
    xtol_rel = xtol_rel,
    maxeval = maxeval
  )

  # Evaluate response at optimal spend
  optimal_kpi <- map2_dbl(response_funs, res$solution, ~.x(.y))

  solution <- hlpr_build_solution(
    channels = channels,
    mrms = mrms,
    weekly_spend = res$solution,
    weekly_kpi = optimal_kpi,
    n_weeks = n_weeks
  )

  constraints_df <- tibble::tibble(
    channel = channels,
    lb = constr$lb,
    ub = constr$ub,
    x0 = constr$x0
  )

  if (verbose) {
    cat("Optimization converged (status:", res$status, ")\n")
    cat("Total weekly KPI:", format(round(sum(optimal_kpi)), big.mark = ","), "\n")
    if (n_weeks > 1) {
      cat("Total period KPI:", format(round(sum(optimal_kpi) * n_weeks), big.mark = ","), "\n")
    }
  }

  list(
    solution = solution,
    constraints = constraints_df,
    budget_info = budget_info,
    nloptr_result = res,
    response_funs = response_funs,
    draws_matrix = NULL,
    kpi_matrix = NULL,
    solution_draws = NULL,
    n_draws = NULL,
    draw_ids = NULL
  )
}


# =============================================================================
# Posterior-sampling path
# =============================================================================

opt_mix_posterior <- function(
    mrms, constr, weekly_budget, budget_info,
    n_draws, seed, parallel,
    xtol_rel, maxeval, verbose
) {

  channels <- names(mrms)
  n_channels <- length(mrms)
  n_weeks <- budget_info$n_weeks

  # Pre-extract and unscale all draws (the fast path)
  draws_list <- hlpr_extract_draws(mrms)

  # Determine available draws (use min across models)
  max_available <- min(map_dbl(draws_list, ~.x$n_draws))
  if (n_draws > max_available) {
    warning("Requested ", n_draws, " draws but only ", max_available,
            " available. Using ", max_available, ".")
    n_draws <- max_available
  }

  if (!is.null(seed)) set.seed(seed)
  draw_ids <- sample(seq_len(max_available), n_draws)

  # Budget equality constraint
  eval_g_eq <- function(x) sum(x) - weekly_budget

  # Build objective function for a single draw
  make_draw_objective <- function(draw_id) {
    function(x) {
      total <- 0
      for (i in seq_along(draws_list)) {
        dl <- draws_list[[i]]
        total <- total + dl$curve_fn(
          x[i],
          b = dl$b[draw_id],
          c = dl$c[draw_id],
          d = dl$d[draw_id],
          e = dl$e[draw_id]
        )
      }
      -total
    }
  }

  # Single-draw solver
  solve_one <- function(j) {
    eval_f <- make_draw_objective(draw_ids[j])
    res <- hlpr_opt_solve(
      eval_f = eval_f,
      x0 = constr$x0,
      lb = constr$lb,
      ub = constr$ub,
      eval_g_eq = eval_g_eq,
      xtol_rel = xtol_rel,
      maxeval = maxeval
    )
    res$solution
  }

  # Run optimization across draws
  if (verbose) cat("Optimizing across", n_draws, "posterior draws...\n")

  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    results_list <- future.apply::future_lapply(
      seq_len(n_draws), solve_one, future.seed = TRUE
    )
  } else {
    if (parallel) {
      warning("future.apply not installed; falling back to sequential.")
    }
    if (verbose) pb <- txtProgressBar(min = 0, max = n_draws, style = 3)
    results_list <- vector("list", n_draws)
    for (j in seq_len(n_draws)) {
      results_list[[j]] <- solve_one(j)
      if (verbose) setTxtProgressBar(pb, j)
    }
    if (verbose) {
      close(pb)
      cat("\n")
    }
  }

  # Assemble results matrix (n_draws x n_channels)
  draws_matrix <- do.call(rbind, results_list)
  colnames(draws_matrix) <- channels

  # Compute KPI for each draw's solution (using that draw's own curve)
  kpi_matrix <- matrix(NA_real_, nrow = n_draws, ncol = n_channels)
  colnames(kpi_matrix) <- channels
  total_kpi <- numeric(n_draws)

  for (j in seq_len(n_draws)) {
    for (i in seq_along(draws_list)) {
      dl <- draws_list[[i]]
      kpi_matrix[j, i] <- dl$curve_fn(
        draws_matrix[j, i],
        b = dl$b[draw_ids[j]],
        c = dl$c[draw_ids[j]],
        d = dl$d[draw_ids[j]],
        e = dl$e[draw_ids[j]]
      )
    }
    total_kpi[j] <- sum(kpi_matrix[j, ])
  }

  # Compute medians and CIs
  spend_median <- apply(draws_matrix, 2, stats::median)
  spend_lower  <- apply(draws_matrix, 2, stats::quantile, 0.025)
  spend_upper  <- apply(draws_matrix, 2, stats::quantile, 0.975)
  kpi_median   <- apply(kpi_matrix, 2, stats::median)
  kpi_lower    <- apply(kpi_matrix, 2, stats::quantile, 0.025)
  kpi_upper    <- apply(kpi_matrix, 2, stats::quantile, 0.975)

  solution <- hlpr_build_solution(
    channels = channels,
    mrms = mrms,
    weekly_spend = spend_median,
    weekly_kpi = kpi_median,
    weekly_spend_lower = spend_lower,
    weekly_spend_upper = spend_upper,
    weekly_kpi_lower = kpi_lower,
    weekly_kpi_upper = kpi_upper,
    n_weeks = n_weeks
  )

  # Long-form draws for plotting
  total_spend_vec <- rowSums(draws_matrix)
  solution_draws <- tibble::as_tibble(as.data.frame(draws_matrix)) |>
    dplyr::mutate(
      draw = dplyr::row_number(),
      total_kpi = total_kpi,
      total_spend = total_spend_vec,
      cost_per = total_spend_vec / total_kpi
    )

  if (verbose) {
    cat("Posterior optimization complete.\n")
    cat("Median total weekly KPI:",
        format(round(sum(kpi_median)), big.mark = ","), "\n")
  }

  list(
    solution = solution,
    constraints = tibble::tibble(
      channel = channels, lb = constr$lb, ub = constr$ub, x0 = constr$x0
    ),
    budget_info = budget_info,
    nloptr_result = NULL,
    response_funs = NULL,
    draws_matrix = draws_matrix,
    kpi_matrix = kpi_matrix,
    solution_draws = solution_draws,
    n_draws = n_draws,
    draw_ids = draw_ids
  )
}


# =============================================================================
# Constraint helpers (internal)
# =============================================================================

#' Auto-generate constraints from model return rate ranges
#' @keywords internal
hlpr_auto_constraints <- function(mrms, weekly_budget, bounds_multiplier = 3) {
  channels <- names(mrms)

  lb <- map_dbl(mrms, ~{
    min_spend <- .x$summary$range_min_spend
    max(min_spend / bounds_multiplier, 0)
  })

  ub <- map_dbl(mrms, ~{
    max_spend <- .x$summary$range_max_spend
    max_spend * bounds_multiplier
  })

  x0 <- map_dbl(mrms, ~{
    min_spend <- .x$summary$range_min_spend
    max_spend <- .x$summary$range_max_spend
    (min_spend + max_spend) / 2
  })

  # Ensure feasibility
  x0 <- pmin(pmax(x0, lb), ub)

  list(lb = lb, ub = ub, x0 = x0)
}

#' Parse user-supplied constraints data frame
#'
#' Supports absolute bounds (`min_spend`, `max_spend`), share-based bounds
#' (`min_share`, `max_share` as fractions of budget), and fixed channels
#' (`fixed = TRUE` locks spend at `min_spend`). When both absolute and
#' share-based bounds are present, the tighter constraint wins.
#' @keywords internal
hlpr_parse_constraints <- function(constraints, channels, weekly_budget) {
  required_cols <- c("channel", "min_spend", "max_spend")
  missing <- setdiff(required_cols, names(constraints))
  if (length(missing) > 0) {
    stop("constraints must contain columns: ", paste(missing, collapse = ", "))
  }

  # Match to model order
  constr_ordered <- constraints[match(channels, constraints$channel), ]

  unmatched <- channels[is.na(match(channels, constraints$channel))]
  if (length(unmatched) > 0) {
    stop("No constraints found for channels: ", paste(unmatched, collapse = ", "))
  }

  lb <- constr_ordered$min_spend
  ub <- constr_ordered$max_spend

  # --- Share-based bounds (tighter constraint wins) ---
  if ("min_share" %in% names(constr_ordered)) {
    ms <- constr_ordered$min_share
    if (any(ms < 0 | ms > 1, na.rm = TRUE)) {
      stop("min_share values must be between 0 and 1.")
    }
    if (sum(ms, na.rm = TRUE) > 1) {
      stop("Sum of min_share exceeds 1 \u2014 constraints are infeasible.")
    }
    share_lb <- ms * weekly_budget
    lb <- pmax(lb, share_lb, na.rm = TRUE)
  }

  if ("max_share" %in% names(constr_ordered)) {
    ms <- constr_ordered$max_share
    if (any(ms < 0 | ms > 1, na.rm = TRUE)) {
      stop("max_share values must be between 0 and 1.")
    }
    share_ub <- ms * weekly_budget
    ub <- pmin(ub, share_ub, na.rm = TRUE)
  }

  # Validate share ordering
  if (all(c("min_share", "max_share") %in% names(constr_ordered))) {
    bad <- constr_ordered$min_share > constr_ordered$max_share
    if (any(bad, na.rm = TRUE)) {
      stop("min_share must be <= max_share for all channels.")
    }
  }

  # --- Fixed channels ---
  if ("fixed" %in% names(constr_ordered)) {
    fixed_mask <- !is.na(constr_ordered$fixed) & constr_ordered$fixed
    lb[fixed_mask] <- constr_ordered$min_spend[fixed_mask]
    ub[fixed_mask] <- constr_ordered$min_spend[fixed_mask]
  }

  x0 <- (lb + ub) / 2

  list(lb = lb, ub = ub, x0 = x0)
}
