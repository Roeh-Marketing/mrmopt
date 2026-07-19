# =============================================================================
# Tests for opt_mix with objective = "target_cpk" and "target_mcpk"
# (cost-per-KPI convenience wrappers around target_roi / target_mroi)
# =============================================================================


# --- Input validation --------------------------------------------------------

test_that("opt_mix errors when target_cpk missing for target_cpk objective", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_error(
    opt_mix(mrms, objective = "target_cpk", verbose = FALSE),
    "target_cpk.*must be a positive number"
  )
})

test_that("opt_mix errors when target_cpk is non-positive", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_error(
    opt_mix(mrms, objective = "target_cpk", target_cpk = 0, verbose = FALSE),
    "target_cpk.*must be a positive number"
  )
  expect_error(
    opt_mix(mrms, objective = "target_cpk", target_cpk = -10, verbose = FALSE),
    "target_cpk.*must be a positive number"
  )
})

test_that("opt_mix errors when target_mcpk missing for target_mcpk objective", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_error(
    opt_mix(mrms, objective = "target_mcpk", verbose = FALSE),
    "target_mcpk.*must be a positive number"
  )
})

test_that("opt_mix messages when budget supplied with target_cpk", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_message(
    opt_mix(mrms, objective = "target_cpk", target_cpk = 1000,
            budget = 100000, verbose = FALSE),
    "budget.*ignored"
  )
})


# --- Equivalence with reciprocal ROI/mROI ------------------------------------

test_that("target_cpk produces same allocation as target_roi = 1/cpk", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  cpk_target <- 1000
  r_cpk <- opt_mix(mrms, method = "point", objective = "target_cpk",
                   target_cpk = cpk_target, verbose = FALSE)
  r_roi <- opt_mix(mrms, method = "point", objective = "target_roi",
                   target_roi = 1 / cpk_target, verbose = FALSE)

  # Same spend allocation
  expect_equal(r_cpk$solution$weekly_spend, r_roi$solution$weekly_spend,
               tolerance = 1)
  # Same KPI
  expect_equal(r_cpk$solution$weekly_kpi, r_roi$solution$weekly_kpi,
               tolerance = 0.1)
})

test_that("target_mcpk produces same allocation as target_mroi = 1/mcpk", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  mcpk_target <- 500
  r_mcpk <- opt_mix(mrms, method = "point", objective = "target_mcpk",
                    target_mcpk = mcpk_target, verbose = FALSE)
  r_mroi <- opt_mix(mrms, method = "point", objective = "target_mroi",
                    target_mroi = 1 / mcpk_target, verbose = FALSE)

  expect_equal(r_mcpk$solution$weekly_spend, r_mroi$solution$weekly_spend,
               tolerance = 1)
  expect_equal(r_mcpk$solution$weekly_kpi, r_mroi$solution$weekly_kpi,
               tolerance = 0.1)
})


# --- Return structure --------------------------------------------------------

test_that("target_cpk result has correct objective and CPK-specific fields", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_cpk",
                    target_cpk = 1000, verbose = FALSE)

  expect_s3_class(result, "opt_mix_result")
  expect_equal(result$objective, "target_cpk")
  expect_equal(result$target_cpk, 1000)
  expect_true(is.numeric(result$achieved_cpk))
  expect_true(is.finite(result$achieved_cpk))

  # Underlying ROI fields are also present
  expect_equal(result$target_roi, 1 / 1000)
  expect_true(is.numeric(result$achieved_roi))

  # Reciprocal relationship holds
  expect_equal(result$achieved_cpk, 1 / result$achieved_roi, tolerance = 1e-6)

  # Budget is an output
  expect_equal(result$budget_info$weekly_budget,
               sum(result$solution$weekly_spend))
})

test_that("target_mcpk result has correct objective and mCPK-specific fields", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_mcpk",
                    target_mcpk = 500, verbose = FALSE)

  expect_s3_class(result, "opt_mix_result")
  expect_equal(result$objective, "target_mcpk")
  expect_equal(result$target_mcpk, 500)
  expect_true(!is.null(result$channel_mcpk))
  expect_equal(length(result$channel_mcpk), 2)

  # Underlying mROI fields also present
  expect_equal(result$target_mroi, 1 / 500)
  expect_true(!is.null(result$channel_mroi))

  # Reciprocal relationship
  expect_equal(unname(result$channel_mcpk),
               unname(1 / result$channel_mroi), tolerance = 0.1)
})


# --- Posterior path ----------------------------------------------------------

test_that("target_cpk posterior returns valid structure", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "posterior", objective = "target_cpk",
                    target_cpk = 1000, n_draws = 5, verbose = FALSE)

  expect_equal(result$objective, "target_cpk")
  expect_equal(result$n_draws, 5)
  expect_true(!is.null(result$draws_matrix))
  expect_true(is.numeric(result$achieved_cpk))
})

test_that("target_mcpk posterior returns valid structure", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "posterior", objective = "target_mcpk",
                    target_mcpk = 500, n_draws = 5, verbose = FALSE)

  expect_equal(result$objective, "target_mcpk")
  expect_true(!is.null(result$channel_mcpk))
  expect_true(!is.null(result$draws_matrix))
})


# --- Display -----------------------------------------------------------------

test_that("opt_summary shows CPK framing for target_cpk", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_cpk",
                    target_cpk = 1000, verbose = FALSE)

  output <- capture.output(opt_summary(result))
  expect_true(any(grepl("target CPK", output, ignore.case = TRUE)))
  expect_true(any(grepl("Achieved CPK", output, ignore.case = TRUE)))
  expect_true(any(grepl("Optimal budget", output, ignore.case = TRUE)))
})

test_that("opt_summary shows mCPK framing for target_mcpk", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_mcpk",
                    target_mcpk = 500, verbose = FALSE)

  output <- capture.output(opt_summary(result))
  expect_true(any(grepl("target mCPK", output, ignore.case = TRUE)))
  expect_true(any(grepl("marginal CPK", output, ignore.case = TRUE)))
})

test_that("opt_table works with target_cpk result", {
  m1 <- make_mock_mrmfit("gompertz", with_units = TRUE)
  m2 <- make_mock_mrmfit("logistic", with_units = TRUE)
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_cpk",
                    target_cpk = 1000, verbose = FALSE)

  tbl <- opt_table(result)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(attr(tbl, "objective"), "target_cpk")
  expect_true("TOTAL" %in% tbl$channel)
})
