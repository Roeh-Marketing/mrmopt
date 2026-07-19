#' Fit a response curve model using brms
#'
#' This function fits a response curve model using the brms package.
#'
#' @param data A data frame containing the data to be fitted.
#' @param spend The name of the spend (independent) variable. This is always
#'   the x-axis predictor.
#' @param kpi The name of the KPI (dependent/response) variable.
#' @param date The name of the date column in the data.
#' @param units Optional name of a units column (e.g., impressions, GRPs). When
#'   supplied, cost per unit (CPU) is computed as
#'   \code{sum(spend) / sum(units)} and a \code{units} column is added to the
#'   inference output from \code{\link{mrm_infer}}.
#' @param type The type of response curve model to fit. Options are "gompertz",
#'   "logistic", "log_logistic", "reflected_gompertz", "weibull", or
#'   "reflected_weibull".
#' @param auto Logical indicating whether to automatically scale the data and
#'   set priors. Default is TRUE.
#' @param scale_data Logical indicating whether to scale the data before fitting.
#'   Default is TRUE.
#' @param scale_method The method used for scaling. Either "min_max" or "std".
#' @param midpoint_range A two-element numeric vector specifying the midpoint
#'   bounds as fractions of the x-axis range (e.g., \code{c(0.1, 0.9)}).
#'   See \code{\link{mrmopt_prior}} for details.
#' @param ceiling_max A multiplier on the observed max of y for the ceiling
#'   upper bound (e.g., \code{3} means ceiling can be up to 3x observed max).
#'   See \code{\link{mrmopt_prior}} for details.
#' @param floor_min A scalar lower bound for the floor in original data units.
#'   Default is 0. See \code{\link{mrmopt_prior}} for details.
#' @param prior An optional \code{brmsprior} object for full manual control
#'   over priors. If provided, \code{midpoint_range}, \code{ceiling_max}, and
#'   \code{floor_min} are ignored. When \code{auto = FALSE} and
#'   \code{scale_data = FALSE}, this is required.
#' @param chains Number of Markov chains. Default is 4.
#' @param iter Total number of iterations per chain. Default is 4000.
#' @param warmup Number of warmup iterations per chain. Default is 1000.
#' @param control A list of control parameters for the sampler. Default is
#'   \code{list(adapt_delta = 0.95)}.
#' @param infer_xrange Optional range of x values for inference. If NULL, uses
#'   the range of x in the data.
#' @param infer_length The number of points to generate for inference.
#'   Default is 1000.
#' @param anchor_strength A single positive numeric value controlling how
#'   tightly the floor parameter (`c`) is constrained around `floor_min`.
#'   See \code{\link{mrmopt_prior}} for details. If supplied, overrides the
#'   `anchor_strength` in `mrmopt_prior()`. Default is `NULL` (use `mrmopt_prior`
#'   default of `0.05`).
#' @param anchor_zero \[Deprecated\] Previously controlled injection of a
#'   synthetic (0, 0) data point. Floor anchoring is now handled via
#'   `anchor_strength` in \code{\link{mrmopt_prior}}. If set, a deprecation
#'   warning is emitted and the argument is ignored.
#' @param backend Character string specifying the Stan backend for
#'   \code{\link[brms]{brm}}: \code{"cmdstanr"} (default) or \code{"rstan"}.
#'   The \code{cmdstanr} backend is approximately 9x faster for typical
#'   response-curve models. If \code{cmdstanr} or CmdStan is not installed, the
#'   function falls back to \code{rstan} with an informative message.
#' @param refresh How often Stan reports sampling progress (in iterations).
#'   Default is 500. Set to 0 for silent sampling.
#' @param ... Additional arguments to be passed to the \code{\link[brms]{brm}}
#'   function.
#' @return A fitted model object.
#' @details The function fits a response curve model using the specified type
#'   and returns the fitted model object. Prior specification can be done in
#'   three ways:
#'   \enumerate{
#'     \item \strong{Automatic} (\code{auto = TRUE}): Data is scaled and
#'       default priors are set automatically. You can still customize via
#'       \code{midpoint_range}, \code{ceiling_max}, and \code{floor_min}.
#'     \item \strong{Simplified} (\code{auto = FALSE}): Use
#'       \code{midpoint_range}, \code{ceiling_max}, and/or \code{floor_min}
#'       to set intuitive, scale-invariant priors.
#'     \item \strong{Manual}: Pass a raw \code{\link[brms]{prior}} object
#'       for full control.
#'   }
#' @export


fit_response = function(data,
                        spend = NULL,
                        kpi = NULL,
                        date = NULL,
                        units = NULL,
                        auto = TRUE,
                        type = "gompertz",
                        scale_data = TRUE,
                        scale_method = "min_max",
                        midpoint_range = NULL,
                        ceiling_max = NULL,
                        floor_min = NULL,
                        prior = NULL,
                        chains = 4,
                        iter = 4000,
                        warmup = 1000,
                        control = list(adapt_delta = 0.95),
                        infer_xrange = NULL,
                        infer_length = 1000,
                        anchor_strength = NULL,
                        anchor_zero = NULL,
                        backend = "cmdstanr",
                        refresh = 500,
                        ...){

  # --- Resolve backend (cmdstanr preferred, rstan fallback) ---
  backend <- hlpr_resolve_backend(backend)

  # --- Guard: warmup must be less than iter ---
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

  missing_cols <- setdiff(c(x, y, date), names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # --- Validate and compute cost_per_unit from units ---
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

  # --- Deprecation: anchor_zero ---
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

  if(auto){
    scale_data = TRUE
    if (has_raw_prior) {
      warning(
        "In auto mode, raw `prior` is ignored. ",
        "Set `auto = FALSE` to use a custom brmsprior.",
        call. = FALSE
      )
      prior <- NULL
    }
  }

  # Capture date range before subsetting (stored as metadata on the fit object)
  date_range <- range(data[[date]], na.rm = TRUE)

  # Subset to model columns only (date and units are not used in fitting)
  data <- data[,c(x,y)]

  # --- Validate non-negative y ---
  if (any(data[[y]] < 0, na.rm = TRUE)) {
    stop(
      "Response variable '", y, "' contains negative values. ",
      "Response curve models require non-negative response data.",
      call. = FALSE
    )
  }

  # --- Scaling ---
  if(scale_data){

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

      prior <- hlpr_resolve_prior(
        mrm_prior = user_mrm_prior,
        scaled_data = data,
        x = x, y = y,
        scale_method = scale_method,
        scale_values = scale_values,
        type = type
      )
    }

  }else{
    # No scaling — raw prior is required
    if(is.null(prior)){
      stop("If scale_data is FALSE, a `prior` (brmsprior object) must be provided.",
           call. = FALSE)
    }
    if(!inherits(prior, "brmsprior")){
      stop("The provided prior is not of class 'brmsprior'. Please provide a valid prior.",
           call. = FALSE)
    }
    if(!all(c("b", "c", "d", "e") %in% prior$nlpar)){
      stop("The provided prior does not contain all required parameters (b, c, d, e).",
           call. = FALSE)
    }
  }

  #rename the columns of data by removing any _ or . in the column names
  colnames(data) <- gsub("[_.]", "", colnames(data))

  rc_formula = hlpr_define_response_form(type, colnames(data)[1], colnames(data)[2])
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

  if(scale_data){
    fit$scale_values = scale_values
    fit$scale_method = scale_method
  }else{
    fit$scale_values = NULL
    fit$scale_method = NULL
  }

  fit$date_range = date_range
  fit$rc_type = type
  fit$spend_col = x
  fit$kpi_col = y
  fit$cost_per_unit = if (!is.null(units)) cost_per_unit else NULL
  fit$date_col = date
  fit$units_col = units

  # Assign class before post-processing so mrm_* functions pass inherits() checks
  class(fit) <- c("mrmfit", class(fit))

  fit$response_df =
    mrm_infer(
      fit,
      xrange = infer_xrange,
      length.out = infer_length,
      scaled = scale_data
      )
  fit$R2 = tibble::as_tibble(brms::bayes_R2(fit))
  fit$summary = mrm_summary(fit)
  fit$params_summary = mrm_params(fit)

  return(fit)
}
