# Find spend level where marginal return equals a target

Searches for the rightmost spend level in \`\[lb, ub\]\` where the
marginal return equals \`target_mr\`. Uses a grid search to find the
bracket, then refines with \[stats::uniroot()\].

## Usage

``` r
hlpr_find_mroi_spend(
  curve_fn,
  b,
  c_param,
  d,
  e,
  target_mr,
  lb,
  ub,
  n_grid = 200
)
```

## Arguments

- curve_fn:

  Response curve function.

- b, c_param, d, e:

  Curve parameters (scalars).

- target_mr:

  Target marginal return.

- lb, ub:

  Lower and upper bounds for the spend search.

- n_grid:

  Number of grid points for initial bracket search.

## Value

Scalar: spend level where MR ≈ target_mr, clamped to \`\[lb, ub\]\`.
