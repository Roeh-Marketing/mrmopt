# Compute baseline KPI (response at zero spend) for a channel

For ROI calculations, "incremental KPI" = f(x) - f(0). This helper
computes f(0) for each of the 6 response curve types.

## Usage

``` r
hlpr_baseline_kpi(curve_fn, b, c_param, d, e, rc_type)
```

## Arguments

- curve_fn:

  Response curve function (from \[rm_dispatch()\]).

- b, c_param, d, e:

  Curve parameters (scalars).

- rc_type:

  Character string identifying the curve type.

## Value

Scalar: the baseline KPI at zero spend.

## Details

\- \*\*Log forms\*\* (log_logistic, weibull, reflected_weibull): \`f(0+)
= c\` analytically, so \`c_param\` is returned directly (avoids
\`log(0)\`). - \*\*Standard forms\*\* (logistic, gompertz,
reflected_gompertz): evaluates \`curve_fn(0, b, c_param, d, e)\`
directly.
