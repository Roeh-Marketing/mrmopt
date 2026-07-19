#' Fit a within-channel hierarchical response curve model using brms
#'
#' Fits a single hierarchical response curve model for one channel where the
#' curve parameters are partially pooled across sub-channel groupings. The
#' channel-level (population) effects describe the mean curve; group-level
#' (random) effects let sparse sub-channel units borrow strength from the
#' channel mean. Shape parameters (\code{b}, \code{e}) are pooled by default,
#' while the scale parameter (\code{d}) is allowed to vary more freely to
#' reflect size differences across units.
#'
#' This is the hierarchical counterpart to \code{\link{fit_response}} and
#' follows the same conventions: global data scaling, the \code{b}/\code{c}/
#' \code{d}/\code{e} parameterization, and the same prior-specification tiers.
#'
#' @inheritParams fit_response
#' @param group A character vector of one or more grouping column names,
#'   ordered from the outermost (broadest) to the innermost (finest) level of
#'   the hierarchy. For example \code{c("subtype", "station")} fits a nested
#'   structure \code{(1 | subtype) + (1 | subtype:station)} where stations are
#'   pooled within subtypes, which are in turn pooled toward the channel mean.
#' @param type The response curve type: \code{"gompertz"}, \code{"logistic"},
#'   \code{"reflected_gompertz"}, \code{"log_logistic"}, \code{"weibull"}, or
#'   \code{"reflected_weibull"}. For the log-based forms the midpoint is
#'   reparameterized internally on the log scale for sampling stability (see
#'   Details); results are still reported in the usual \code{b}/\code{c}/\code{d}/
#'   \code{e} units. Default \code{"gompertz"}.
#' @param pool A character vector naming which of \code{b}, \code{c}, \code{d},
#'   \code{e} receive group-level effects. Default \code{c("b", "e", "d")}.
#' @param group_sd_prior An optional single \code{brms} distribution string
#'   (e.g. \code{"exponential(1)"}) applied to every partial-pooling SD. When
#'   \code{NULL} (default), weakly-informative per-parameter defaults are used.
#' @param min_obs Minimum number of observations a unit (innermost group level)
#'   must have to be retained. Units with fewer observations are dropped with a
#'   warning, since they cannot identify even a pooled curve. Default \code{5}.
#' @param control A list of sampler control parameters. Default
#'   \code{list(adapt_delta = 0.95, max_treedepth = 12)}.
#' @param backend Character string specifying the Stan backend for
#'   \code{\link[brms]{brm}}: \code{"cmdstanr"} (default) or \code{"rstan"}.
#'   See \code{\link{fit_response}} for details.
#'
#' @return A fitted model object of class \code{mrmfit_hier} (extending
#'   \code{brmsfit}).
#' @details
#' Prior specification follows the same three tiers as \code{\link{fit_response}}
#' (automatic, simplified via \code{midpoint_range}/\code{ceiling_max}/
#' \code{floor_min}, or a manual raw \code{brmsprior}). The population priors are
#' identical to the single-fit case; partial-pooling SD priors are added
#' automatically (see \code{\link{hlpr_resolve_prior_hier}}).
#'
#' For the log-based forms (\code{"log_logistic"}, \code{"weibull"},
#' \code{"reflected_weibull"}) the midpoint enters as \code{log(e)}, which
#' requires \code{e > 0}. Group-level deviations on \code{e} can violate that
#' and produce \code{NaN} during sampling, so the midpoint is reparameterized
#' internally on the log scale (an unconstrained parameter \code{le = log(e)}
#' replaces \code{log(e)} in the model). This is transparent: \code{le} is
#' translated back to \code{e} during extraction, and all outputs
#' (\code{params_hier}, \code{response_df}, summaries, plots) report \code{e} in
#' original spend units.
#'
#' @seealso \code{\link{fit_response}}, \code{\link{hlpr_define_response_form_hier}}
#' @export

fit_response_hier <- function(data,
                              spend = NULL,
                              kpi = NULL,
                              date = NULL,
                              units = NULL,
                              group = NULL,
                              auto = TRUE,
                              type = "log_logistic",
                              pool = c("b", "e", "d"),
                              scale_data = TRUE,
                              scale_method = "min_max",
                              midpoint_range = NULL,
                              ceiling_max = NULL,
                              floor_min = NULL,
                              group_sd_prior = NULL,
                              min_obs = 5,
                              prior = NULL,
                              chains = 4,
                              iter = 4000,
                              warmup = 1000,
                              control = list(adapt_delta = 0.95,
                                             max_treedepth = 12),
                              infer_xrange = NULL,
                              infer_length = 1000,
                              anchor_strength = NULL,
                              anchor_zero = NULL,
                              backend = "cmdstanr",
                              refresh = 500,
                              ...) {

  # --- Resolve backend (cmdstanr preferred, rstan fallback) ---
  backend <- hlpr_resolve_backend(backend)

  if (is.null(group) || length(group) < 1) {
    stop("'group' must specify at least one grouping column.", call. = FALSE)
  }

  # --- Guard: warmup must be less than iter (matches fit_response) ---
  if (warmup >= iter) {
    new_warmup <- floor(0.5 * iter)
    warning(
      "`warmup` (", warmup, ") must be less than `iter` (", iter, "). ",
      "Setting `warmup = ", new_warmup, "`.",
      call. = FALSE
    )
    warmup <- new_warmup
  }

  # --- Map domain names to internal x/y ---
  x <- spend
  y <- kpi

  # --- Validate required columns ---
  if (is.null(x) || is.null(y) || is.null(date)) {
    stop("'spend', 'kpi', and 'date' must all be specified.", call. = FALSE)
  }

  missing_cols <- setdiff(c(x, y, date, group), names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # --- Validate and compute cost_per_unit from units (matches fit_response) ---
  cost_per_unit <- 1.0
  if (!is.null(units)) {
    if (!(units %in% names(data))) {
      stop("Units column '", units, "' not found in data.", call. = FALSE)
    }
    if (any(is.na(data[[units]]))) {
      stop("Units column '", units, "' contains NA values.", call. = FALSE)
    }
    if (any(data[[units]] == 0)) {
      stop("Units column '", units, "' contains zero values; cannot compute cost per unit.",
           call. = FALSE)
    }
    cost_per_unit <- sum(data[[x]], na.rm = TRUE) / sum(data[[units]], na.rm = TRUE)
  }

  # --- Deprecation: anchor_zero (matches fit_response) ---
  if (!is.null(anchor_zero)) {
    warning(
      "`anchor_zero` is deprecated. Floor anchoring is now handled via ",
      "`anchor_strength` in mrm_prior(). See ?mrmopt_prior for details.",
      call. = FALSE
    )
  }

  # --- Detect whether user provided simplified prior args ---
  has_simple_prior <- !is.null(midpoint_range) || !is.null(ceiling_max) ||
    !is.null(floor_min) || !is.null(anchor_strength)
  has_raw_prior <- !is.null(prior) && inherits(prior, "brmsprior")

  if (has_simple_prior && has_raw_prior) {
    stop(
      "Cannot specify both a raw `brmsprior` and simplified prior arguments ",
      "(`midpoint_range`, `ceiling_max`, `floor_min`, `anchor_strength`). ",
      "Use one or the other.",
      call. = FALSE
    )
  }

  if (auto) {
    scale_data <- TRUE
    if (has_raw_prior) {
      warning(
        "In auto mode, raw `prior` is ignored. ",
        "Set `auto = FALSE` to use a custom brmsprior.",
        call. = FALSE
      )
      prior <- NULL
    }
  }

  # --- Capture date range before subsetting (stored as metadata) ---
  date_range <- range(data[[date]], na.rm = TRUE)

  # --- Subset to model columns: x, y, and the grouping column(s) ---
  # date and units are not used in fitting; group columns are required for the
  # hierarchical structure.
  data <- data[, c(x, y, group), drop = FALSE]

  # --- Validate non-negative y (matches fit_response) ---
  if (any(data[[y]] < 0, na.rm = TRUE)) {
    stop(
      "Response variable '", y, "' contains negative values. ",
      "Response curve models require non-negative response data.",
      call. = FALSE
    )
  }

  # --- Drop units with too few observations ---
  # Keyed on the innermost group combination (all grouping columns), so a
  # sparse subtype:station combination is dropped even when its parent is dense.
  combo_key <- if (length(group) == 1) {
    as.character(data[[group[1]]])
  } else {
    apply(data[, group, drop = FALSE], 1,
          function(r) paste(as.character(r), collapse = "_"))
  }
  obs_counts <- table(combo_key)
  sparse_units <- names(obs_counts)[obs_counts < min_obs]
  if (length(sparse_units) > 0) {
    warning(
      length(sparse_units), " unit(s) have fewer than ", min_obs,
      " observations and were dropped: ",
      paste(sparse_units, collapse = ", "),
      call. = FALSE
    )
    data <- data[!(combo_key %in% sparse_units), , drop = FALSE]
  }
  if (nrow(data) == 0) {
    stop("No observations remain after dropping sparse units. ",
         "Lower `min_obs` or check the data.", call. = FALSE)
  }

  # --- Scaling (global; reuses hlpr_scale_data, group columns pass through) ---
  if (scale_data) {

    scaled_data_list <- hlpr_scale_data(data, x, y, scale_method, type = type)

    data <- scaled_data_list$scaled_data
    scale_values <- scaled_data_list$scale_values
    infer_xrange <- scaled_data_list$scaled_xrange

    # --- Prior resolution ---
    if (has_raw_prior) {
      prior <- prior
    } else {
      mrm_prior_args <- list()
      if (!is.null(midpoint_range)) mrm_prior_args$midpoint_range <- midpoint_range
      if (!is.null(ceiling_max)) mrm_prior_args$ceiling_max <- ceiling_max
      if (!is.null(floor_min)) mrm_prior_args$floor_min <- floor_min
      if (!is.null(anchor_strength)) mrm_prior_args$anchor_strength <- anchor_strength

      user_mrm_prior <- do.call(mrmopt_prior, mrm_prior_args)

      prior <- hlpr_resolve_prior_hier(
        mrm_prior = user_mrm_prior,
        scaled_data = data,
        x = x, y = y,
        scale_method = scale_method,
        scale_values = scale_values,
        type = type,
        group = group,
        pool = pool,
        group_sd_prior = group_sd_prior
      )
    }

  } else {
    # No scaling — raw prior is required
    if (is.null(prior)) {
      stop("If scale_data is FALSE, a `prior` (brmsprior object) must be provided.",
           call. = FALSE)
    }
    if (!inherits(prior, "brmsprior")) {
      stop("The provided prior is not of class 'brmsprior'. Please provide a valid prior.",
           call. = FALSE)
    }
    if (!all(c("b", "c", "d", "e") %in% prior$nlpar)) {
      stop("The provided prior does not contain all required parameters (b, c, d, e).",
           call. = FALSE)
    }
    scale_values <- NULL
  }

  # --- Sanitize x/y column names (matches fit_response) ---
  # Strip "_" and "." from the spend/kpi names so they form valid formula
  # tokens. Group column names are sanitized in parallel so the formula and the
  # data stay in sync. Group *values* (factor levels) are left untouched.
  x_clean     <- gsub("[_.]", "", x)
  y_clean     <- gsub("[_.]", "", y)
  group_clean <- gsub("[_.]", "", group)

  names(data)[match(x, names(data))]     <- x_clean
  names(data)[match(y, names(data))]     <- y_clean
  names(data)[match(group, names(data))] <- group_clean

  # --- Ensure grouping columns are factors ---
  for (g in group_clean) {
    data[[g]] <- factor(data[[g]])
  }

  # --- Build the hierarchical formula ---
  rc_formula <- hlpr_define_response_form_hier(
    type  = type,
    x     = x_clean,
    y     = y_clean,
    group = group_clean,
    pool  = pool
  )
  print(rc_formula)

  fit <- brms::brm(
    rc_formula,
    data = data,
    prior = prior,
    chains = chains,
    iter = iter,
    warmup = warmup,
    control = control,
    backend = backend,
    refresh = refresh,
    ...
  )

  # --- Attach metadata (mirrors fit_response, plus hierarchy fields) ---
  if (scale_data) {
    fit$scale_values <- scale_values
    fit$scale_method <- scale_method
  } else {
    fit$scale_values <- NULL
    fit$scale_method <- NULL
  }

  fit$date_range   <- date_range
  fit$rc_type      <- type
  fit$spend_col    <- x
  fit$kpi_col      <- y
  fit$cost_per_unit <- if (!is.null(units)) cost_per_unit else NULL
  fit$date_col     <- date
  fit$units_col    <- units

  # Hierarchy-specific metadata
  fit$group       <- group_clean
  fit$group_orig  <- group
  fit$pool        <- intersect(pool, c("b", "c", "d", "e"))
  fit$levels      <- lapply(group_clean, function(g) levels(data[[g]]))
  names(fit$levels) <- group_clean

  # Assign class. mrmfit_hier extends brmsfit but deliberately does NOT inherit
  # "mrmfit": the single-curve mrm_* methods assume one curve and would
  # mis-process a hierarchical fit. Per-level post-processing arrives in Phase 2.
  class(fit) <- c("mrmfit_hier", class(fit))

  # Bayes R2 (cheap, works on any brmsfit)
  fit$R2 <- tryCatch(
    tibble::as_tibble(brms::bayes_R2(fit)),
    error = function(e) NULL
  )

  # --- Post-processing: per-unit + channel params, inference, summaries ---
  fit$params_hier <- hlpr_params_hier(fit, scaled = scale_data)
  fit$response_df <- mrm_infer_hier(
    fit,
    xrange = infer_xrange,
    length.out = infer_length,
    scaled = scale_data
  )
  fit$summary <- mrm_summary_hier(fit)

  return(fit)
}
