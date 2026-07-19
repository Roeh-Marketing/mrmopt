# Vectorised baseline KPI for posterior draws

Computes f(0) for each posterior draw. Used by the target-ROI posterior
path to build per-draw ROI constraints.

## Usage

``` r
hlpr_baseline_kpi_vec(curve_fn, b_vec, c_vec, d_vec, e_vec, rc_type)
```

## Arguments

- curve_fn:

  Response curve function (from \[rm_dispatch()\]).

- b_vec, c_vec, d_vec, e_vec:

  Numeric vectors of parameter draws.

- rc_type:

  Character string identifying the curve type.

## Value

Numeric vector (same length as inputs): baseline KPI per draw.
