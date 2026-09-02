# Roadmap

This page tracks planned features and longer-term research directions for `mrmopt`.
Items are organized by horizon and reflect both near-term engineering work and
more exploratory modeling directions.

Feedback and contributions are welcome — open an issue on
[GitHub](https://github.com/Roeh-Marketing/mrmopt/issues) to discuss any of these.

---

## Recently Shipped

### Within-Channel Hierarchical Response Curves

**New function:** `fit_response_hier()` — **shipped.** See the
[Hierarchical Response Curves](https://roeh-marketing.github.io/mrmopt/articles/hierarchical_models.html)
article for a worked example.

Media channels are rarely homogeneous. TV spend spans broadcast, cable, and
streaming — each with different audience reach and response dynamics. Social
covers multiple partners, tactics, and creatives. Standard response curve
modeling treats the channel as a single unit, which either overfits sparse
sub-channel data or discards granularity altogether.

`fit_response_hier()` fits a single hierarchical model for one channel where
curve parameters are partially pooled across sub-channel groupings:

```r
fit_tv <- fit_response_hier(
  data      = tv_data,
  spend     = "spend",
  kpi       = "conversions",
  date      = "week",
  group     = c("subtype", "station"),   # nested hierarchy (any depth)
  type      = "gompertz"
)
```

The fixed effects represent the channel-level mean curve. Random effects at
each level (subtype → station) are drawn from the level above, so sparse units
(e.g., a small cable station with a handful of weeks) borrow strength from
better-identified peers. The degree of shrinkage is automatic and
data-driven — units with more observations get pulled less toward the group mean.

**Delivered:**

- Channel-level, sub-type-level, and unit-level curves from a single model
  (`mrm_summary_hier()`, `mrm_infer_hier()`)
- All six curve forms, including the log-based forms (midpoint reparameterized
  internally on the log scale for sampling stability)
- Arbitrary-depth nested hierarchies
- Posterior uncertainty correctly reflects data sparsity at each level
- Optimization at any level of the hierarchy via `as_mrmfit_list()` + `opt_mix()`
- Per-unit and shrinkage visualizations via `mrm_plot_hier()`
- Pooling operates on curve shape parameters (`b`, `e`); scale (`d`) is
  allowed to vary more freely to reflect size differences across units

---

### Rolling Window Response Curves (`mrm_rolling()`)

**New function:** `mrm_rolling()`

Standard response curve fitting treats the full observation window as a single
stationary period. In practice, the relationship between spend and KPI shifts
over time — seasonal demand, competitive pressure, and creative fatigue all
cause the effective ceiling and saturation point to vary. `mrm_rolling()` makes
this drift visible by fitting a sequence of response curves across overlapping
windows of the data.

```r
rolling_fits <- mrm_rolling(
  data        = channel_data,
  spend       = "spend",
  kpi         = "conversions",
  date        = "week",
  type        = "gompertz",
  window_size = 13,   # weeks per window
  stride      = 4     # weeks between window starts
)
```

Each window is fit independently using `fit_response()`, producing a list of
`mrmfit` objects anchored to their respective date ranges. A companion plot
shows how each parameter's posterior median and credible interval evolves across
windows, making seasonal drift in the ceiling (`d`) or midpoint (`e`) directly
visible.

**Key properties:**

- No new model architecture — each window calls `fit_response()` directly
- `window_size` controls the timescale of local estimation; `stride` controls
  resolution vs. compute cost
- Returns a named list of `mrmfit` objects compatible with `mrms_plot_compare()`
- A dedicated `mrm_plot_rolling()` will show parameter evolution over time with
  credible ribbons

**Note:** This is a diagnostic and exploratory tool. Because windows are fit
independently there is no formal pooling across them — adjacent windows do not
borrow strength from each other. The longer-term time-varying parameter model
(see below) addresses this limitation with a principled shared estimation
framework.

---

## Medium-Term

### Adstock Support

Estimated geometric adstock decay integrated into `fit_response()` via an
optional `adstock = TRUE` argument. The decay parameter will be estimated
jointly with the saturation curve parameters and carry full posterior
uncertainty, consistent with Meridian's approach.

```r
fit <- fit_response(
  data    = my_data,
  spend   = "tv_spend",
  kpi     = "conversions",
  date    = "week",
  type    = "gompertz",
  adstock = TRUE   # estimate decay parameter jointly
)
```

Default prior: `Uniform(0, 1)` on the decay parameter — uninformative,
consistent with Meridian's defaults. Users can tighten this with domain
knowledge about carryover windows for specific channels.

---

## Longer-Term / Exploratory

### Time-Varying Response Curves (`fit_response_tv()`)

**New function:** `fit_response_tv()`

`mrm_rolling()` makes parameter drift visible but estimates each window
independently. The proper formulation treats the curve parameters themselves as
functions of time — continuous, smooth, and estimated jointly across all
observations in a single model. This is a **time-varying parameter model**,
where `d` (ceiling) and `e` (midpoint) follow a random walk or Gaussian Process
over time:

$$d_t \sim \mathcal{N}(d_{t-1},\ \sigma_d), \qquad e_t \sim \mathcal{N}(e_{t-1},\ \sigma_e)$$

The evolution variance (`σ_d`, `σ_e`) is estimated from the data and controls
the timescale of parameter change — a small σ means near-stationary parameters;
a larger σ allows rapid seasonal swings. Parameters `b` (steepness) and `c`
(floor) are held fixed at the channel level, as these reflect structural
properties of the channel rather than seasonal demand.

```r
fit_tv <- fit_response_tv(
  data       = channel_data,
  spend      = "spend",
  kpi        = "conversions",
  date       = "week",
  type       = "gompertz",
  varying    = c("d", "e"),   # which parameters evolve over time
  evolution  = "random_walk"  # or "gp" for Gaussian Process
)
```

**Relationship to the rolling window approach:** The window size in
`mrm_rolling()` is a discrete proxy for the GP kernel bandwidth here. A
13-week window implies parameters change on a ~quarterly timescale — the
equivalent GP kernel would have a length-scale of ~13 weeks. The time-varying
model captures this continuously and without boundary artifacts at window edges.

**Relationship to seasonal hierarchical models:** A simpler intermediate
formulation — year-level partial pooling on `d` alone, fit via `brms` multilevel
machinery — is worth building first as a stepping stone. It addresses
year-over-year ceiling shifts (e.g., a growing market) without requiring custom
Stan code, and its output is directly compatible with the existing `opt_mix()`
infrastructure.

**Key properties:**

- `evolution = "random_walk"`: AR(1) prior on parameters — tractable in Stan,
  natural for slowly drifting seasonality
- `evolution = "gp"`: Gaussian Process prior — more flexible, kernel bandwidth
  estimated from data, but significantly more expensive
- Requires hand-written Stan model via `CmdStanR` — outside the `brms` formula
  interface
- Returns a `mrmfit_tv` object with time-indexed parameter posteriors and a
  `mrm_plot_tv()` visualization showing the evolving curve across the observation
  window

---

### Ground-Up Joint MMM (`fit_mmm()`)

`mrmopt` currently operates as a **response curve layer** — it fits saturation
curves to attributed KPI volumes produced by an upstream MMM. This is a valid
and practical workflow, but it means the saturation parameters are estimated
on data that already has an implicit independence and linearity assumption
baked in by the attribution model.

A ground-up joint MMM would estimate adstock decay, saturation curve
parameters, and channel contribution coefficients *simultaneously* from raw
spend and total KPI data — the approach taken by
[Google Meridian](https://github.com/google/meridian) and
[PyMC-Marketing](https://www.pymc-marketing.io/).

This would be implemented via a hand-written Stan model using
[`CmdStanR`](https://mc-stan.org/cmdstanr/) as the backend, giving full
control over the model structure without the constraints of the `brms`
formula interface. Priors would be aligned with Meridian's published defaults
(`ec ~ TruncatedNormal(0.8, 0.8, 0.1, 10)` on normalized spend,
`decay ~ Uniform(0, 1)`, `beta ~ HalfNormal(5)`).

**Key design:** A new `fit_mmm()` function returning a `mrmfit_joint` S3
class that feeds into the existing `opt_mix()` optimization infrastructure
via the same draw-matrix interface used by the posterior optimization path.

This is a substantial modeling and engineering undertaking and will be
developed incrementally. Single-market models without geo-level hierarchy
are the initial target scope.
