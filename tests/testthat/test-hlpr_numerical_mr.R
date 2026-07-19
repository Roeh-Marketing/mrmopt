test_that("hlpr_numerical_mr approximates known logistic derivative", {
  # Logistic: y = c + (d-c)/(1 + exp(b*(x-e)))

  # dy/dx = -(d-c)*b*exp(b*(x-e)) / (1 + exp(b*(x-e)))^2
  b <- -5e-5; c_val <- 0; d_val <- 1000; e_val <- 500000
  fn <- rm_dispatch("logistic")

  x <- 300000
  # Analytical derivative
  z <- exp(b * (x - e_val))
  analytical_mr <- -(d_val - c_val) * b * z / (1 + z)^2

  numerical_mr <- hlpr_numerical_mr(fn, x, b, c_val, d_val, e_val)
  expect_equal(numerical_mr, analytical_mr, tolerance = 1e-4)
})

test_that("hlpr_numerical_mr approximates known gompertz derivative", {
  # Gompertz: y = c + (d-c)*exp(-exp(b*(x-e)))
  # dy/dx = -(d-c)*b*exp(b*(x-e))*exp(-exp(b*(x-e)))
  b <- -5e-5; c_val <- 0; d_val <- 1000; e_val <- 500000
  fn <- rm_dispatch("gompertz")

  x <- 400000
  z <- b * (x - e_val)
  analytical_mr <- -(d_val - c_val) * b * exp(z) * exp(-exp(z))

  numerical_mr <- hlpr_numerical_mr(fn, x, b, c_val, d_val, e_val)
  expect_equal(numerical_mr, analytical_mr, tolerance = 1e-4)
})

test_that("hlpr_numerical_mr handles log-form curves near zero", {
  fn <- rm_dispatch("weibull")
  # Should not error at small x
  result <- hlpr_numerical_mr(fn, x = 10, b = -3, c_param = 0,
                              d = 1000, e = 50000)
  expect_true(is.finite(result))
})

test_that("hlpr_numerical_mr returns positive MR for increasing curve", {
  fn <- rm_dispatch("gompertz")
  # Evaluate at inflection point where MR is maximised
  result <- hlpr_numerical_mr(fn, x = 500000, b = -5e-5, c_param = 0,
                              d = 1000, e = 500000)
  expect_gt(result, 0)
})


# --- hlpr_find_mroi_spend tests ---

test_that("hlpr_find_mroi_spend finds correct spend for logistic curve", {
  fn <- rm_dispatch("logistic")
  b <- -5e-5; c_val <- 0; d_val <- 1000; e_val <- 500000

  # Get MR at a known point, then try to recover that point
  target_x <- 400000
  target_mr <- hlpr_numerical_mr(fn, target_x, b, c_val, d_val, e_val)

  found_x <- hlpr_find_mroi_spend(fn, b, c_val, d_val, e_val,
                                   target_mr = target_mr,
                                   lb = 10000, ub = 1000000)
  expect_equal(found_x, target_x, tolerance = 500)
})

test_that("hlpr_find_mroi_spend returns ub when target near zero", {
  fn <- rm_dispatch("gompertz")
  b <- -5e-5; c_val <- 0; d_val <- 1000; e_val <- 500000

  # A very low target: the solution should be at high spend
  found_x <- hlpr_find_mroi_spend(fn, b, c_val, d_val, e_val,
                                   target_mr = 1e-10,
                                   lb = 10000, ub = 1000000)
  # Should be well past the inflection point

  expect_gt(found_x, 700000)
})

test_that("hlpr_find_mroi_spend returns lb when MR always below target", {
  fn <- rm_dispatch("gompertz")
  b <- -5e-5; c_val <- 0; d_val <- 1000; e_val <- 500000

  # Very high target — MR always below
  found_x <- hlpr_find_mroi_spend(fn, b, c_val, d_val, e_val,
                                   target_mr = 1e6,
                                   lb = 10000, ub = 1000000)
  expect_equal(found_x, 10000)
})

test_that("hlpr_find_mroi_spend respects bounds", {
  fn <- rm_dispatch("logistic")
  b <- -5e-5; c_val <- 0; d_val <- 1000; e_val <- 500000

  found_x <- hlpr_find_mroi_spend(fn, b, c_val, d_val, e_val,
                                   target_mr = 0.005,
                                   lb = 100000, ub = 800000)
  expect_gte(found_x, 100000)
  expect_lte(found_x, 800000)
})
