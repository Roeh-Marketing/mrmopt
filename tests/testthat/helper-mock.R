# -------------------------------------------------------------------------
# Mock mrmfit fixture
# -------------------------------------------------------------------------

make_mock_response_df <- function(type = "gompertz",
                                  x_min   = 1e4,
                                  x_max   = 1e6,
                                  n       = 200) {

  log_forms <- c("log_logistic", "weibull", "reflected_weibull")
  is_log    <- type %in% log_forms

  x_start <- if (is_log) x_min else 0
  x_seq   <- seq(x_start, 2 * x_max, length.out = n)

  # Params designed to produce a proper saturation curve in [0,2] scaled x space
  params <- list(b = -5, c = 0, d = 1000, e = 0.5)

  if (is_log) {
    x_scaled <- x_seq / x_max          # ratio-scaled: positive, in (0, 2]
  } else {
    x_scaled <- x_seq / x_max          # simple scaling: [0, 2]
  }

  center <- response(x_scaled, params, type = type)
  lower  <- pmax(center * 0.9, 0)
  upper  <- center * 1.1

  # Column name matches the sanitized name brms would store in mrm$data
  x_col <- "spendchannel"

  df <- data.frame(
    x      = x_seq,
    center = center,
    lower  = lower,
    upper  = upper,
    stringsAsFactors = FALSE
  )
  names(df)[1] <- x_col

  y_col <- "center"
  df$ar <- (df[[y_col]] - min(df[[y_col]])) / df[[x_col]]
  df$mr <- c(NA_real_, diff(df[[y_col]]) / diff(df[[x_col]]))
  denom_cp <- df[[y_col]] - min(df[[y_col]])
  df$cp <- ifelse(
    denom_cp > 0,
    df[[x_col]] / denom_cp * (sum(denom_cp) / sum(df[[y_col]])),
    NA_real_
  )
  df$cp_lower <- ifelse(denom_cp > 0, df[[x_col]] / denom_cp, NA_real_)
  df$cp_upper <- df$cp_lower * 1.1

  y_l <- "lower"
  df$ar_lower <- (df[[y_l]] - min(df[[y_l]])) / df[[x_col]]
  df$mr_lower <- c(NA_real_, diff(df[[y_l]]) / diff(df[[x_col]]))

  y_u <- "upper"
  df$ar_upper <- (df[[y_u]] - min(df[[y_u]])) / df[[x_col]]
  df$mr_upper <- c(NA_real_, diff(df[[y_u]]) / diff(df[[x_col]]))

  df$type      <- type
  df$resp_var  <- "opps"
  df$input_var <- x_col

  df
}


make_mock_mrmfit <- function(type        = "gompertz",
                             with_units  = FALSE,
                             x_min       = 1e4,
                             x_max       = 1e6,
                             n_obs       = 52) {

  log_forms  <- c("log_logistic", "weibull", "reflected_weibull")
  is_log     <- type %in% log_forms

  scale_values <- list(
    x_min    = 0,
    x_max    = x_max,
    x_offset = 0,
    y_min    = 0,
    y_max    = 1000
  )

  set.seed(4291)
  x_raw   <- sort(runif(n_obs, x_min, x_max))
  x_sc    <- x_raw / x_max
  y_sc    <- runif(n_obs, 0.3, 0.9)

  # brms stores data with response (y) first, then x
  mock_data <- data.frame(
    opps         = y_sc,
    spendchannel = x_sc,
    stringsAsFactors = FALSE
  )

  rdf <- make_mock_response_df(type = type, x_min = x_min, x_max = x_max)

  weekly_spend_val <- {
    mean(x_sc) * x_max
  }
  rdf_x   <- rdf[["spendchannel"]]

  # Identify range points from response_df
  peak_mr_idx   <- which.max(rdf$mr)
  peak_ar_idx   <- which.max(rdf$ar)
  range_min_s   <- rdf_x[peak_mr_idx]
  range_peak_s  <- rdf_x[peak_ar_idx]
  mr_threshold  <- 0.7 * rdf$mr[peak_mr_idx]
  tail_idx      <- peak_mr_idx:nrow(rdf)
  decay_pos     <- which(rdf$mr[tail_idx] <= mr_threshold)[1]
  range_max_s   <- if (is.na(decay_pos)) max(rdf_x) else rdf_x[tail_idx[decay_pos]]

  interp <- function(col, at) stats::approx(rdf_x, rdf[[col]], xout = at, rule = 2)$y

  mock_summary <- list(
    channel         = "channel",
    rc_type         = type,
    weekly_spend    = weekly_spend_val,
    weekly_units    = if (with_units) weekly_spend_val / 0.05 else NA_real_,
    kpi_at_current  = interp("center", weekly_spend_val),
    ar_at_current   = interp("ar",     weekly_spend_val),
    mr_at_current   = interp("mr",     weekly_spend_val),
    cp_at_current   = interp("cp",     weekly_spend_val),
    rr_at_current   = NA_real_,
    # RC parameters (from summary.mock_brmsfit values)
    b = -5.0, c = 0.0, d = 1000.0, e = 500000.0,
    # Range points
    range_min_spend  = range_min_s,
    range_min_units  = if (with_units) range_min_s  / 0.05 else NA_real_,
    range_min_kpi    = interp("center", range_min_s),
    range_min_cp     = interp("cp",     range_min_s),
    range_min_ar     = interp("ar",     range_min_s),
    range_min_mr     = interp("mr",     range_min_s),
    range_min_rr     = NA_real_,
    range_peak_spend = range_peak_s,
    range_peak_units = if (with_units) range_peak_s / 0.05 else NA_real_,
    range_peak_kpi   = interp("center", range_peak_s),
    range_peak_cp    = interp("cp",     range_peak_s),
    range_peak_ar    = interp("ar",     range_peak_s),
    range_peak_mr    = interp("mr",     range_peak_s),
    range_peak_rr    = NA_real_,
    range_max_spend  = range_max_s,
    range_max_units  = if (with_units) range_max_s  / 0.05 else NA_real_,
    range_max_kpi    = interp("center", range_max_s),
    range_max_cp     = interp("cp",     range_max_s),
    range_max_ar     = interp("ar",     range_max_s),
    range_max_mr     = interp("mr",     range_max_s),
    range_max_rr     = NA_real_,
    # Week distribution
    n_weeks         = n_obs,
    pct_weeks_below = 30,
    pct_weeks_in    = 50,
    pct_weeks_above = 20
  )
  attr(mock_summary, "log_curve_no_peak") <- is_log
  attr(mock_summary, "R2") <- tibble::tibble(
    Estimate = 0.85, Est.Error = 0.02, Q2.5 = 0.80, Q97.5 = 0.90
  )
  class(mock_summary) <- c("mrm_summary", "tbl_df", "tbl", "data.frame")

  mock_r2 <- tibble::tibble(
    Estimate  = 0.85,
    Est.Error = 0.02,
    Q2.5      = 0.80,
    Q97.5     = 0.90
  )

  mock_formula <- list(resp = "opps")
  class(mock_formula) <- "brmsformula"

  # Build params_hier_unit so hlpr_params() can work on mock objects
  # (avoids calling brms::summary() which fails on mocks).
 # Raw scaled center values matching the mock draws means
  raw_center <- c(b = -5, c = 0.1, d = 0.9, e = 0.5)
  raw_lower  <- c(b = -6, c = 0.06, d = 0.8, e = 0.4)
  raw_upper  <- c(b = -4, c = 0.14, d = 1.0, e = 0.6)

  unscaled_center <- hlpr_unscale_params(raw_center, scale_values, type)
  unscaled_lower  <- hlpr_unscale_params(raw_lower,  scale_values, type)
  unscaled_upper  <- hlpr_unscale_params(raw_upper,  scale_values, type)

  mock_params_hier_unit <- list(
    center = as.list(unscaled_center),
    lower  = as.list(unscaled_lower),
    upper  = as.list(unscaled_upper)
  )

  mock <- list(
    data         = mock_data,
    formula      = mock_formula,
    rc_type      = type,
    spend_col    = "spend_channel",
    kpi_col      = "opps",
    date_col     = "date",
    scale_values = scale_values,
    scale_method = "min_max",
    cost_per_unit = if (with_units) 0.05 else NULL,
    units_col    = if (with_units) "units_channel" else NULL,
    date_range   = as.Date(c("2023-01-01", "2023-12-31")),
    response_df  = rdf,
    R2           = mock_r2,
    summary      = mock_summary,
    returnes_ranges = NULL,
    params_summary  = NULL,
    params_hier_unit = mock_params_hier_unit
  )

  class(mock) <- c("mrmfit", "mock_brmsfit", "brmsfit")
  mock
}


# -------------------------------------------------------------------------
# Mock opt_mix_result fixture
# -------------------------------------------------------------------------

make_mock_opt_result <- function(method = "point", n_channels = 2) {
  m1 <- make_mock_mrmfit("gompertz", with_units = TRUE)
  m2 <- make_mock_mrmfit("logistic", with_units = TRUE)
  mrms <- list(ch_a = m1, ch_b = m2)[seq_len(n_channels)]
  channels <- names(mrms)

  spend <- c(200000, 500000)[seq_len(n_channels)]
  kpi <- c(5000, 10000)[seq_len(n_channels)]

  if (method == "posterior") {
    sol <- hlpr_build_solution(
      channels = channels, mrms = mrms,
      weekly_spend = spend, weekly_kpi = kpi,
      weekly_spend_lower = spend * 0.8, weekly_spend_upper = spend * 1.2,
      weekly_kpi_lower = kpi * 0.9, weekly_kpi_upper = kpi * 1.1
    )
    n_draws <- 20
    set.seed(6734)
    dm <- matrix(
      runif(n_draws * n_channels, min = 100000, max = 600000),
      nrow = n_draws, ncol = n_channels
    )
    colnames(dm) <- channels
  } else {
    sol <- hlpr_build_solution(
      channels = channels, mrms = mrms,
      weekly_spend = spend, weekly_kpi = kpi
    )
    dm <- NULL
    n_draws <- NULL
  }

  result <- list(
    solution = sol,
    constraints = tibble::tibble(
      channel = channels, lb = spend * 0.5, ub = spend * 2, x0 = spend
    ),
    budget_info = list(
      total_budget = sum(spend), weekly_budget = sum(spend),
      n_weeks = 1, current_weekly = spend
    ),
    method = method,
    mrms = mrms,
    nloptr_result = NULL,
    response_funs = NULL,
    draws_matrix = dm,
    kpi_matrix = NULL,
    solution_draws = NULL,
    n_draws = n_draws,
    draw_ids = NULL
  )
  class(result) <- "opt_mix_result"
  result
}


# -------------------------------------------------------------------------
# Mock brms draws
# -------------------------------------------------------------------------

# S3 method so hlpr_extract_draws can call as_draws_df on mocks
# Register in posterior namespace so S3 dispatch finds it
as_draws_df.mock_brmsfit <- function(x, ...) {
  n <- 50L
  set.seed(8142)
  df <- data.frame(
    b_b_Intercept = rnorm(n, -5, 0.5),
    b_c_Intercept = rnorm(n, 0.1, 0.02),
    b_d_Intercept = rnorm(n, 0.9, 0.05),
    b_e_Intercept = rnorm(n, 0.5, 0.05),
    sigma = rep(0.1, n),
    lprior = rep(-10, n),
    lp__ = rep(-50, n),
    .chain = rep(1:2, each = n / 2),
    .iteration = rep(seq_len(n / 2), 2),
    .draw = seq_len(n)
  )
  class(df) <- c("draws_df", "draws", "tbl_df", "tbl", "data.frame")
  df
}

# Register so posterior::as_draws_df dispatch finds it
if (requireNamespace("posterior", quietly = TRUE)) {
  registerS3method("as_draws_df", "mock_brmsfit",
                    as_draws_df.mock_brmsfit,
                    envir = asNamespace("posterior"))
}
