test_that("hlpr_baseline_kpi returns c for log-form curves", {
  # For log forms, f(0+) = c analytically (avoids log(0))
  for (type in c("log_logistic", "weibull", "reflected_weibull")) {
    fn <- rm_dispatch(type)
    result <- hlpr_baseline_kpi(fn, b = -3, c_param = 100, d = 800,
                                e = 50000, rc_type = type)
    expect_equal(result, 100)
  }
})

test_that("hlpr_baseline_kpi evaluates curve at 0 for standard forms", {
  # For standard forms, baseline is f(0), which depends on params
  for (type in c("logistic", "gompertz", "reflected_gompertz")) {
    fn <- rm_dispatch(type)
    expected <- fn(0, b = -5, c = 10, d = 1000, e = 500000)
    result <- hlpr_baseline_kpi(fn, b = -5, c_param = 10, d = 1000,
                                e = 500000, rc_type = type)
    expect_equal(result, expected)
  }
})

test_that("hlpr_baseline_kpi standard-form baseline is near floor for typical params", {
  # With midpoint far from 0, the curve should be near the floor at x=0

  fn <- rm_dispatch("gompertz")
  result <- hlpr_baseline_kpi(fn, b = -5, c_param = 0, d = 1000,
                              e = 500000, rc_type = "gompertz")
  # Should be close to c (floor) when e >> 0
  expect_lt(result, 50)
})

test_that("hlpr_baseline_kpi_vec returns vector of baselines", {
  fn <- rm_dispatch("logistic")
  n <- 10
  b_vec <- rep(-5, n)
  c_vec <- rep(0, n)
  d_vec <- rep(1000, n)
  e_vec <- rep(500000, n)

  result <- hlpr_baseline_kpi_vec(fn, b_vec, c_vec, d_vec, e_vec,
                                  rc_type = "logistic")
  expect_length(result, n)
  expect_true(all(is.finite(result)))

  # All should be equal since params are identical
  expect_equal(result, rep(result[1], n))
})

test_that("hlpr_baseline_kpi_vec returns c_vec for log forms", {
  fn <- rm_dispatch("weibull")
  c_vec <- c(10, 20, 30, 40, 50)
  result <- hlpr_baseline_kpi_vec(fn, rep(-3, 5), c_vec, rep(800, 5),
                                  rep(50000, 5), rc_type = "weibull")
  expect_equal(result, c_vec)
})
