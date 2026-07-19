# Optimize media mix allocation across channels

Given a set of fitted response curve models, find the optimal spend
allocation. Supports point-estimate optimization (fast, single solution)
and posterior-sampling optimization (slower, returns a distribution of
solutions reflecting Bayesian uncertainty).

## Usage

``` r
opt_mix(
  mrms,
  method = c("point", "posterior"),
  objective = c("max_kpi", "target_roi", "target_mroi", "target_cpk", "target_mcpk"),
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
  xtol_rel = 1e-08,
  maxeval = 1000,
  verbose = TRUE
)
```

## Arguments

- mrms:

  A named list of \`mrmfit\` objects (one per channel).

- method:

  One of \`"point"\` (default) or \`"posterior"\`. Point uses posterior
  median parameters; posterior optimizes over multiple MCMC draws.

- objective:

  One of \`"max_kpi"\` (default), \`"target_roi"\`, \`"target_mroi"\`,
  \`"target_cpk"\`, or \`"target_mcpk"\`. See \*\*Details\*\*.

- budget:

  Total budget for the period. Required for \`"max_kpi"\`; ignored (with
  a message) for flexible-budget objectives. If \`NULL\` (default),
  inferred from total current weekly spend.

- target_roi:

  Numeric; minimum acceptable portfolio ROI (incremental KPI / spend).
  Required when \`objective = "target_roi"\`.

- target_mroi:

  Numeric; target marginal return per dollar. Required when \`objective
  = "target_mroi"\`.

- target_cpk:

  Numeric; maximum acceptable average cost per KPI unit (spend /
  incremental KPI). Required when \`objective = "target_cpk"\`.

- target_mcpk:

  Numeric; maximum acceptable marginal cost per KPI unit. Required when
  \`objective = "target_mcpk"\`.

- n_weeks:

  Number of weeks the budget covers. Used to convert a period budget to
  weekly optimization. Default \`1\` (budget is already weekly).
  Convenience values: use \`52\` for annual, \`13\` for quarterly, \`4\`
  for monthly.

- constraints:

  A data frame with per-channel bounds. Must contain columns
  \`channel\`, \`min_spend\`, \`max_spend\`. If \`NULL\` (default),
  constraints are auto-generated from model return rate ranges. Note:
  share-based constraints (\`min_share\`/\`max_share\`) are only
  supported for \`objective = "max_kpi"\`.

- bounds_multiplier:

  When \`constraints\` is \`NULL\`, multiplier applied to auto-detected
  spend ranges. Default \`3\`.

- n_draws:

  Number of posterior draws to optimize over when \`method =
  "posterior"\`. Default \`200\`.

- seed:

  Random seed for draw sampling. Default \`NULL\`.

- parallel:

  Logical; use \`future.apply\` for parallel posterior optimization.
  Default \`FALSE\`. Requires a \`future::plan()\` to be set. Ignored
  for \`"target_mroi"\` (per-channel root-finding is fast).

- xtol_rel:

  Relative tolerance for nloptr. Default \`1e-8\`. Ignored for
  \`"target_mroi"\`.

- maxeval:

  Maximum nloptr evaluations per solve. Default \`1000\`. Ignored for
  \`"target_mroi"\`.

- verbose:

  Print progress information. Default \`TRUE\`.

## Value

An \`opt_mix_result\` S3 object. All objectives return the same
top-level structure:

- \`\$solution\` — tibble with one row per channel containing current
  and optimal spend, KPI, units, cost-per, response rate, and share
  columns. Posterior results include \`\_lower\`/\`\_upper\` CI columns.

- \`\$constraints\` — tibble: channel, lb, ub, x0.

- \`\$budget_info\` — list: total_budget, weekly_budget, n_weeks,
  current_weekly. For flexible-budget objectives (\`target_roi\`,
  \`target_mroi\`), \`weekly_budget\` and \`total_budget\` are
  \*outputs\* computed from the optimal solution.

- \`\$method\` — \`"point"\` or \`"posterior"\`.

- \`\$objective\` — the objective used.

- \`\$mrms\` — the named list of \`mrmfit\` models.

Objective-specific fields:

- \`\$target_roi\` / \`\$achieved_roi\` — for \`"target_roi"\`
  objective.

- \`\$target_mroi\` / \`\$channel_mroi\` — for \`"target_mroi"\`
  objective.

- \`\$target_cpk\` / \`\$achieved_cpk\` — for \`"target_cpk"\`
  objective. The underlying \`\$target_roi\` / \`\$achieved_roi\` are
  also available.

- \`\$target_mcpk\` / \`\$channel_mcpk\` — for \`"target_mcpk"\`
  objective. The underlying \`\$target_mroi\` / \`\$channel_mroi\` are
  also available.

Point-only fields: \`\$nloptr_result\`, \`\$response_funs\`.  
Posterior-only fields: \`\$draws_matrix\`, \`\$kpi_matrix\`,
\`\$solution_draws\`, \`\$n_draws\`, \`\$draw_ids\`.

Use \[print()\] or \[summary()\] for a formatted console summary,
\[opt_table()\] for a tidy comparison tibble, and \[plot()\] or the
standalone \`opt_plot\_\*\` functions for visualizations.

## Details

Five objectives are available (the last two are cost-per-KPI convenience
wrappers around the ROI objectives):

- \`"max_kpi"\` (default):

  Maximize total KPI given a fixed budget constraint.

- \`"target_roi"\`:

  Maximize incremental KPI while keeping portfolio ROI \\\ge\\
  \`target_roi\`. The budget is an \*output\*, not an input. ROI is
  defined as total incremental KPI (f(x) - f(0)) divided by total spend.

- \`"target_mroi"\`:

  Set each channel's spend to the point where its marginal return
  (dy/dx) equals \`target_mroi\`. The budget is an \*output\* — the sum
  of per-channel solutions.

- \`"target_cpk"\`:

  Same optimization as \`"target_roi"\` but framed as a cost-per-KPI
  ceiling: keep average CPK \\\le\\ \`target_cpk\`. Internally converts
  to \`target_roi = 1 / target_cpk\`. Useful when the KPI is a
  non-revenue metric (leads, visits, etc.).

- \`"target_mcpk"\`:

  Same optimization as \`"target_mroi"\` but framed as a marginal
  cost-per-KPI threshold: stop spending on a channel once the marginal
  cost of the next KPI unit exceeds \`target_mcpk\`. Internally converts
  to \`target_mroi = 1 / target_mcpk\`.
