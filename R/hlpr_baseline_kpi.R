#' Compute baseline KPI (response at zero spend) for a channel
#'
#' For ROI calculations, "incremental KPI" = f(x) - f(0). This helper
#' computes f(0) for each of the 6 response curve types.
#'
#' @param curve_fn Response curve function (from [rm_dispatch()]).
#' @param b,c_param,d,e Curve parameters (scalars).
#' @param rc_type Character string identifying the curve type.
#' @return Scalar: the baseline KPI at zero spend.
#'
#' @details
#' - **Log forms** (log_logistic, weibull, reflected_weibull): `f(0+) = c`
#'   analytically, so `c_param` is returned directly (avoids `log(0)`).
#' - **Standard forms** (logistic, gompertz, reflected_gompertz): evaluates
#'   `curve_fn(0, b, c_param, d, e)` directly.
#'
#' @keywords internal

hlpr_baseline_kpi <- function(curve_fn, b, c_param, d, e, rc_type) {
  log_forms <- c("log_logistic", "weibull", "reflected_weibull")
  if (rc_type %in% log_forms) {
    return(c_param)
  }
  curve_fn(0, b, c_param, d, e)
}


#' Vectorised baseline KPI for posterior draws
#'
#' Computes f(0) for each posterior draw. Used by the target-ROI posterior
#' path to build per-draw ROI constraints.
#'
#' @param curve_fn Response curve function (from [rm_dispatch()]).
#' @param b_vec,c_vec,d_vec,e_vec Numeric vectors of parameter draws.
#' @param rc_type Character string identifying the curve type.
#' @return Numeric vector (same length as inputs): baseline KPI per draw.
#' @keywords internal

hlpr_baseline_kpi_vec <- function(curve_fn, b_vec, c_vec, d_vec, e_vec,
                                  rc_type) {
  log_forms <- c("log_logistic", "weibull", "reflected_weibull")
  if (rc_type %in% log_forms) {
    return(c_vec)
  }
  vapply(seq_along(b_vec), function(j) {
    curve_fn(0, b = b_vec[j], c = c_vec[j], d = d_vec[j], e = e_vec[j])
  }, numeric(1))
}
