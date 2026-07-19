# =============================================================================
# Tests for opt_mix with objective = "target_roi"
# =============================================================================

# --- Input validation --------------------------------------------------------

test_that("opt_mix errors when target_roi missing for target_roi objective", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  expect_error(
    opt_mix(mrms, objective = "target_roi", verbose = FALSE),
    "target_roi.*must be a positive number"
  )
})

test_that("opt_mix errors when target_roi is non-positive", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_error(
    opt_mix(mrms, objective = "target_roi", target_roi = 0, verbose = FALSE),
    "target_roi.*must be a positive number"
  )
  expect_error(
    opt_mix(mrms, objective = "target_roi", target_roi = -1, verbose = FALSE),
    "target_roi.*must be a positive number"
  )
})

test_that("opt_mix messages when budget supplied with target_roi", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_message(
    opt_mix(mrms, objective = "target_roi", target_roi = 0.001,
            budget = 100000, verbose = FALSE),
    "budget.*ignored"
  )
})

test_that("opt_mix warns about share constraints with flexible-budget objective", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  constr_df <- data.frame(
    channel = c("ch_a", "ch_b"),
    min_spend = c(10000, 10000),
    max_spend = c(500000, 500000),
    min_share = c(0.2, 0.2)
  )

  expect_warning(
    opt_mix(mrms, objective = "target_roi", target_roi = 0.001,
            constraints = constr_df, verbose = FALSE),
    "Share-based constraints.*ignored"
  )
})


# --- Point-estimate path ----------------------------------------------------

test_that("opt_mix target_roi point returns valid structure", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_roi",
                    target_roi = 0.001, verbose = FALSE)

  expect_s3_class(result, "opt_mix_result")
  expect_equal(result$method, "point")
  expect_equal(result$objective, "target_roi")
  expect_equal(result$target_roi, 0.001)
  expect_true(is.numeric(result$achieved_roi))
  expect_true(is.data.frame(result$solution))
  expect_equal(nrow(result$solution), 2)

  # Budget info should be updated (not the initial reference)
  expect_equal(result$budget_info$weekly_budget,
               sum(result$solution$weekly_spend))
})

test_that("opt_mix target_roi point: achieved ROI approximately matches target", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  # ROI constraint should be binding at optimum
  target <- 0.001
  result <- opt_mix(mrms, method = "point", objective = "target_roi",
                    target_roi = target, verbose = FALSE)

  # Achieved ROI should be approximately equal to target (binding constraint)
  expect_equal(result$achieved_roi, target, tolerance = 0.01)
})

test_that("opt_mix target_roi point: higher target -> lower budget", {

  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  r_low  <- opt_mix(mrms, method = "point", objective = "target_roi",
                    target_roi = 0.0005, verbose = FALSE)
  r_high <- opt_mix(mrms, method = "point", objective = "target_roi",
                    target_roi = 0.002, verbose = FALSE)

  # Higher ROI target means less spending (more restrictive)
  expect_lt(r_high$budget_info$weekly_budget,
            r_low$budget_info$weekly_budget)
})

test_that("opt_mix target_roi point respects box constraints", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  constr_df <- data.frame(
    channel = c("ch_a", "ch_b"),
    min_spend = c(50000, 50000),
    max_spend = c(400000, 400000)
  )

  result <- opt_mix(mrms, method = "point", objective = "target_roi",
                    target_roi = 0.001, constraints = constr_df,
                    verbose = FALSE)

  sol <- result$solution
  expect_true(all(sol$weekly_spend >= 50000 - 1))   # tolerance for float

  expect_true(all(sol$weekly_spend <= 400000 + 1))
})


# --- Posterior path ----------------------------------------------------------

test_that("opt_mix target_roi posterior returns valid structure", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "posterior", objective = "target_roi",
                    target_roi = 0.001, n_draws = 5, verbose = FALSE)

  expect_s3_class(result, "opt_mix_result")
  expect_equal(result$method, "posterior")
  expect_equal(result$objective, "target_roi")
  expect_equal(result$n_draws, 5)
  expect_true(!is.null(result$draws_matrix))
  expect_equal(nrow(result$draws_matrix), 5)
  expect_equal(ncol(result$draws_matrix), 2)

  # Budget info is an output
  expect_equal(result$budget_info$weekly_budget,
               sum(result$solution$weekly_spend))

  # ROI column in solution_draws
  expect_true("roi" %in% names(result$solution_draws))
})

test_that("opt_mix target_roi posterior: draws_matrix rows do NOT all sum to same budget", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "posterior", objective = "target_roi",
                    target_roi = 0.001, n_draws = 10, verbose = FALSE)

  row_sums <- rowSums(result$draws_matrix)
  # With flexible budget, row sums should vary (not all equal)
  expect_gt(stats::sd(row_sums), 0)
})


# --- opt_table and opt_summary work with target_roi --------------------------

test_that("opt_table works with target_roi result", {
  m1 <- make_mock_mrmfit("gompertz", with_units = TRUE)
  m2 <- make_mock_mrmfit("logistic", with_units = TRUE)
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_roi",
                    target_roi = 0.001, verbose = FALSE)

  tbl <- opt_table(result)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(attr(tbl, "objective"), "target_roi")

  # TOTAL row present
  expect_true("TOTAL" %in% tbl$channel)
})

test_that("opt_summary works with target_roi result", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_roi",
                    target_roi = 0.001, verbose = FALSE)

  output <- capture.output(opt_summary(result))
  # Should mention "target ROI" and "Achieved ROI" and "Optimal budget"
  expect_true(any(grepl("target ROI", output, ignore.case = TRUE)))
  expect_true(any(grepl("Achieved ROI", output, ignore.case = TRUE)))
  expect_true(any(grepl("Optimal budget", output, ignore.case = TRUE)))
})
