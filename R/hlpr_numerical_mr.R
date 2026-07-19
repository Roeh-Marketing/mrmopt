#' Compute numerical marginal return at a given spend level
#'
#' Uses central finite differences to approximate dy/dx. Handles edge cases
#' near zero for log-form curves by clamping the lower evaluation point.
#'
#' @param curve_fn Response curve function (from [rm_dispatch()]).
#' @param x Scalar: spend level to evaluate MR at.
#' @param b,c_param,d,e Curve parameters (scalars).
#' @param h Step size for central difference. Default uses
#'   `max(1, abs(x) * 1e-6)` — 1 dollar or a relative step, whichever is
#'   larger.
#' @return Scalar: estimated dy/dx (marginal return per dollar of spend).
#' @keywords internal

hlpr_numerical_mr <- function(curve_fn, x, b, c_param, d, e, h = NULL) {
  if (is.null(h)) {
    h <- max(1, abs(x) * 1e-6)
  }

  x_lo <- max(x - h, 1e-10)
  x_hi <- x + h
  actual_h <- x_hi - x_lo

  y_lo <- curve_fn(x_lo, b = b, c = c_param, d = d, e = e)
  y_hi <- curve_fn(x_hi, b = b, c = c_param, d = d, e = e)

  (y_hi - y_lo) / actual_h
}


#' Find spend level where marginal return equals a target
#'
#' Searches for the rightmost spend level in `[lb, ub]` where the marginal
#' return equals `target_mr`. Uses a grid search to find the bracket, then
#' refines with [stats::uniroot()].
#'
#' @param curve_fn Response curve function.
#' @param b,c_param,d,e Curve parameters (scalars).
#' @param target_mr Target marginal return.
#' @param lb,ub Lower and upper bounds for the spend search.
#' @param n_grid Number of grid points for initial bracket search.
#' @return Scalar: spend level where MR ≈ target_mr, clamped to `[lb, ub]`.
#' @keywords internal

hlpr_find_mroi_spend <- function(curve_fn, b, c_param, d, e,
                                 target_mr, lb, ub, n_grid = 200) {
  # Ensure positive bounds for evaluation

  lb <- max(lb, 1e-10)

  grid_x <- seq(lb, ub, length.out = n_grid)
  grid_g <- vapply(grid_x, function(xi) {
    hlpr_numerical_mr(curve_fn, xi, b, c_param, d, e) - target_mr
  }, numeric(1))

  # Find sign changes
  sign_changes <- which(diff(sign(grid_g)) != 0)

  if (length(sign_changes) == 0) {
    # No crossing found
    if (all(grid_g >= 0)) return(ub)   # MR always above target
    if (all(grid_g <= 0)) return(lb)   # MR always below target
    return(lb)
  }

  # Take the rightmost crossing (decreasing-MR side)
  idx <- sign_changes[length(sign_changes)]

  g_fn <- function(x) {
    hlpr_numerical_mr(curve_fn, x, b, c_param, d, e) - target_mr
  }

  result <- tryCatch(
    stats::uniroot(g_fn, interval = c(grid_x[idx], grid_x[idx + 1]),
                   tol = .Machine$double.eps^0.5),
    error = function(e) list(root = (grid_x[idx] + grid_x[idx + 1]) / 2)
  )

  # Clamp to bounds
  min(max(result$root, lb), ub)
}
