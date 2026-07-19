# Compute numerical marginal return at a given spend level

Uses central finite differences to approximate dy/dx. Handles edge cases
near zero for log-form curves by clamping the lower evaluation point.

## Usage

``` r
hlpr_numerical_mr(curve_fn, x, b, c_param, d, e, h = NULL)
```

## Arguments

- curve_fn:

  Response curve function (from \[rm_dispatch()\]).

- x:

  Scalar: spend level to evaluate MR at.

- b, c_param, d, e:

  Curve parameters (scalars).

- h:

  Step size for central difference. Default uses \`max(1, abs(x) \*
  1e-6)\` — 1 dollar or a relative step, whichever is larger.

## Value

Scalar: estimated dy/dx (marginal return per dollar of spend).
