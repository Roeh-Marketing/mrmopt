# =============================================================================
# Tests for opt_mix with objective = "target_mroi"
# =============================================================================

# --- Input validation --------------------------------------------------------

test_that("opt_mix errors when target_mroi missing for target_mroi objective", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_error(
    opt_mix(mrms, objective = "target_mroi", verbose = FALSE),
    "target_mroi.*must be a positive number"
  )
})

test_that("opt_mix errors when target_mroi is non-positive", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_error(
    opt_mix(mrms, objective = "target_mroi", target_mroi = 0, verbose = FALSE),
    "target_mroi.*must be a positive number"
  )
})

test_that("opt_mix messages when budget supplied with target_mroi", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  expect_message(
    opt_mix(mrms, objective = "target_mroi", target_mroi = 0.001,
            budget = 100000, verbose = FALSE),
    "budget.*ignored"
  )
})


# --- Point-estimate path ----------------------------------------------------

test_that("opt_mix target_mroi point returns valid structure", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_mroi",
                    target_mroi = 0.001, verbose = FALSE)

  expect_s3_class(result, "opt_mix_result")
  expect_equal(result$method, "point")
  expect_equal(result$objective, "target_mroi")
  expect_equal(result$target_mroi, 0.001)
  expect_true(is.numeric(result$channel_mroi))
  expect_equal(length(result$channel_mroi), 2)
  expect_true(is.data.frame(result$solution))
  expect_equal(nrow(result$solution), 2)

  # Budget info should be updated from solution
  expect_equal(result$budget_info$weekly_budget,
               sum(result$solution$weekly_spend))
})

test_that("opt_mix target_mroi point: achieved mROI approximately matches target", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  target <- 0.001
  result <- opt_mix(mrms, method = "point", objective = "target_mroi",
                    target_mroi = target, verbose = FALSE)

  # Per-channel mROI should be near target (unless at a bound)
  for (ch in names(result$channel_mroi)) {
    sol <- result$solution
    ch_spend <- sol$weekly_spend[sol$channel == ch]
    ch_constr <- result$constraints[result$constraints$channel == ch, ]

    # If not at a bound, mROI should match target
    at_lb <- abs(ch_spend - ch_constr$lb) < 1
    at_ub <- abs(ch_spend - ch_constr$ub) < 1
    if (!at_lb && !at_ub) {
      expect_equal(unname(result$channel_mroi[ch]), target, tolerance = 0.0005)
    }
  }
})

test_that("opt_mix target_mroi point: higher target -> lower per-channel spend", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  r_low  <- opt_mix(mrms, method = "point", objective = "target_mroi",
                    target_mroi = 0.0005, verbose = FALSE)
  r_high <- opt_mix(mrms, method = "point", objective = "target_mroi",
                    target_mroi = 0.002, verbose = FALSE)

  # Higher mROI target = stop spending sooner = less total spend
  expect_lt(r_high$budget_info$weekly_budget,
            r_low$budget_info$weekly_budget)
})

test_that("opt_mix target_mroi point: spend at lb when MR always below target", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch_a = m1)

  # Very high target that MR can never reach
  result <- opt_mix(mrms, method = "point", objective = "target_mroi",
                    target_mroi = 1e6, verbose = FALSE)

  # Should be at lower bound
  expect_equal(result$solution$weekly_spend, result$constraints$lb,
               tolerance = 1)
})


# --- Posterior path ----------------------------------------------------------

test_that("opt_mix target_mroi posterior returns valid structure", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "posterior", objective = "target_mroi",
                    target_mroi = 0.001, n_draws = 5, verbose = FALSE)

  expect_s3_class(result, "opt_mix_result")
  expect_equal(result$method, "posterior")
  expect_equal(result$objective, "target_mroi")
  expect_equal(result$n_draws, 5)
  expect_true(!is.null(result$draws_matrix))
  expect_equal(nrow(result$draws_matrix), 5)
  expect_equal(ncol(result$draws_matrix), 2)
  expect_true(!is.null(result$channel_mroi))
})

test_that("opt_mix target_mroi posterior: budget varies across draws", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "posterior", objective = "target_mroi",
                    target_mroi = 0.001, n_draws = 10, verbose = FALSE)

  row_sums <- rowSums(result$draws_matrix)
  # Budget varies across draws (flexible budget)
  expect_gt(stats::sd(row_sums), 0)
})


# --- opt_table and opt_summary work with target_mroi -------------------------

test_that("opt_table works with target_mroi result", {
  m1 <- make_mock_mrmfit("gompertz", with_units = TRUE)
  m2 <- make_mock_mrmfit("logistic", with_units = TRUE)
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_mroi",
                    target_mroi = 0.001, verbose = FALSE)

  tbl <- opt_table(result)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(attr(tbl, "objective"), "target_mroi")
  expect_true("TOTAL" %in% tbl$channel)
})

test_that("opt_summary works with target_mroi result", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("logistic")
  mrms <- list(ch_a = m1, ch_b = m2)

  result <- opt_mix(mrms, method = "point", objective = "target_mroi",
                    target_mroi = 0.001, verbose = FALSE)

  output <- capture.output(opt_summary(result))
  expect_true(any(grepl("target mROI", output, ignore.case = TRUE)))
  expect_true(any(grepl("Optimal budget", output, ignore.case = TRUE)))
})
