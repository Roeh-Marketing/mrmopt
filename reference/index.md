# Package index

## All functions

- [`as_draws_df(`*`<mrmfit_hier_unit>`*`)`](https://bdshaff.github.io/mrmopt/reference/as_draws_df.mrmfit_hier_unit.md)
  : Posterior draws accessor for hierarchical unit views
- [`as_mrmfit_list()`](https://bdshaff.github.io/mrmopt/reference/as_mrmfit_list.md)
  : Expose a hierarchical fit as a list of single-curve models for
  optimization
- [`fit_response()`](https://bdshaff.github.io/mrmopt/reference/fit_response.md)
  : Fit a response curve model using brms
- [`fit_response_hier()`](https://bdshaff.github.io/mrmopt/reference/fit_response_hier.md)
  : Fit a within-channel hierarchical response curve model using brms
- [`hlpr_define_response_form()`](https://bdshaff.github.io/mrmopt/reference/hlpr_define_response_form.md)
  : Define a response form for a nonlinear model
- [`hlpr_get_weekly_spend()`](https://bdshaff.github.io/mrmopt/reference/hlpr_get_weekly_spend.md)
  : Get Weekly Spend helper function
- [`hlpr_replace_variables_in_formula()`](https://bdshaff.github.io/mrmopt/reference/hlpr_replace_variables_in_formula.md)
  : Replace Variables in a Formula
- [`hlpr_scale_data()`](https://bdshaff.github.io/mrmopt/reference/hlpr_scale_data.md)
  : Helper function to scale the data for model prep
- [`hlpr_set_objective_function()`](https://bdshaff.github.io/mrmopt/reference/hlpr_set_objective_function.md)
  : Create an objective function for optimization
- [`hlpr_set_total_constraint()`](https://bdshaff.github.io/mrmopt/reference/hlpr_set_total_constraint.md)
  : Set a total constraint function
- [`mrm_infer()`](https://bdshaff.github.io/mrmopt/reference/mrm_infer.md)
  : Infer response from a fitted model
- [`mrm_infer_hier()`](https://bdshaff.github.io/mrmopt/reference/mrm_infer_hier.md)
  : Infer per-unit, per-level, and channel response curves from a
  hierarchical fit
- [`mrm_params()`](https://bdshaff.github.io/mrmopt/reference/mrm_params.md)
  : Extract response curve parameters from a fitted model
- [`mrm_plot()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot.md)
  : Plot a fitted response model
- [`mrm_plot_costper()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot_costper.md)
  : Plot cost per KPI of a fitted model
- [`mrm_plot_diagnostics()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot_diagnostics.md)
  : Plot brms diagnostics for a fitted response model
- [`mrm_plot_hier()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot_hier.md)
  : Plot a hierarchical response curve fit
- [`mrm_plot_hier_diagnostics()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot_hier_diagnostics.md)
  : Convergence diagnostics for a hierarchical fit
- [`mrm_plot_hier_response()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot_hier_response.md)
  : Per-level response curves from a hierarchical fit
- [`mrm_plot_hier_shrinkage()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot_hier_shrinkage.md)
  : Partial-pooling shrinkage plot from a hierarchical fit
- [`mrm_plot_response()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot_response.md)
  : Plot the response curve of a fitted model
- [`mrm_plot_return()`](https://bdshaff.github.io/mrmopt/reference/mrm_plot_return.md)
  : Plot Absolute and Marginal Rates of Return
- [`mrm_response_function()`](https://bdshaff.github.io/mrmopt/reference/mrm_response_function.md)
  : Get the response function from a fitted model
- [`mrm_summary()`](https://bdshaff.github.io/mrmopt/reference/mrm_summary.md)
  : Generate a channel-level summary from a fitted response model
- [`mrm_summary_hier()`](https://bdshaff.github.io/mrmopt/reference/mrm_summary_hier.md)
  : Per-unit, per-level, and channel summary from a hierarchical fit
- [`mrmopt_data`](https://bdshaff.github.io/mrmopt/reference/mrmopt_data.md)
  : Simulated multi-channel media spend dataset
- [`mrmopt_hier_data`](https://bdshaff.github.io/mrmopt/reference/mrmopt_hier_data.md)
  : Simulated hierarchical TV sub-channel dataset
- [`mrmopt_palette()`](https://bdshaff.github.io/mrmopt/reference/mrmopt_palette.md)
  : Package-level color palette for mrmopt plots
- [`mrmopt_prior()`](https://bdshaff.github.io/mrmopt/reference/mrmopt_prior.md)
  : Create a prior specification for response curve fitting
- [`mrms_plot_compare()`](https://bdshaff.github.io/mrmopt/reference/mrms_plot_compare.md)
  : Compare multiple fitted response models
- [`opt_generate_constraints()`](https://bdshaff.github.io/mrmopt/reference/opt_generate_constraints.md)
  : Generate constraints for optimization based on MRM return rates or
  total spend This function generates constraints for optimization based
  on the return rates from the MRM models or a total spend constraint.
  The constraints include lower bounds, upper bounds, and initial values
  for each channel.
- [`opt_mix()`](https://bdshaff.github.io/mrmopt/reference/opt_mix.md) :
  Optimize media mix allocation across channels
- [`opt_plot_allocation()`](https://bdshaff.github.io/mrmopt/reference/opt_plot_allocation.md)
  : Plot current vs. optimal spend or KPI allocation
- [`opt_plot_comparison()`](https://bdshaff.github.io/mrmopt/reference/opt_plot_comparison.md)
  : Dumbbell chart of spend reallocation
- [`opt_plot_curves()`](https://bdshaff.github.io/mrmopt/reference/opt_plot_curves.md)
  : Response curves with current and optimal spend points
- [`opt_plot_posterior()`](https://bdshaff.github.io/mrmopt/reference/opt_plot_posterior.md)
  : Posterior distribution of optimal spend allocation
- [`opt_plot_returns()`](https://bdshaff.github.io/mrmopt/reference/opt_plot_returns.md)
  : Average and marginal return curves with current and optimal points
- [`opt_summary()`](https://bdshaff.github.io/mrmopt/reference/opt_summary.md)
  : Print a formatted summary of an opt_mix_result object
- [`opt_table()`](https://bdshaff.github.io/mrmopt/reference/opt_table.md)
  : Tidy comparison table for opt_mix_result objects
- [`plot(`*`<mrmfit_hier>`*`)`](https://bdshaff.github.io/mrmopt/reference/plot.mrmfit_hier.md)
  : Plot method for mrmfit_hier objects
- [`plot(`*`<mrmfit_hier_unit>`*`)`](https://bdshaff.github.io/mrmopt/reference/plot.mrmfit_hier_unit.md)
  : Plot method for hierarchical unit views
- [`plot(`*`<opt_mix_result>`*`)`](https://bdshaff.github.io/mrmopt/reference/plot.opt_mix_result.md)
  : Plot method for opt_mix_result objects
- [`print(`*`<mrm_prior>`*`)`](https://bdshaff.github.io/mrmopt/reference/print.mrm_prior.md)
  : Print method for mrm_prior objects
- [`print(`*`<mrmfit>`*`)`](https://bdshaff.github.io/mrmopt/reference/print.mrmfit.md)
  : Print method for mrmfit objects
- [`print(`*`<mrmfit_hier>`*`)`](https://bdshaff.github.io/mrmopt/reference/print.mrmfit_hier.md)
  : Print method for mrmfit_hier objects
- [`print(`*`<mrmfit_hier_unit>`*`)`](https://bdshaff.github.io/mrmopt/reference/print.mrmfit_hier_unit.md)
  : Print method for hierarchical unit views
- [`print(`*`<opt_mix_result>`*`)`](https://bdshaff.github.io/mrmopt/reference/print.opt_mix_result.md)
  : Print method for opt_mix_result objects
- [`response()`](https://bdshaff.github.io/mrmopt/reference/response.md)
  : Response Curve Function
