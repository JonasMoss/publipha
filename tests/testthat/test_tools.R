context("tools")

expect_equal(
  extract_theta0(small_model),
  mean(rstan::extract(small_model)$theta0)
)
expect_equal(
  extract_theta(small_model),
  apply(rstan::extract(small_model)$theta, 2, mean)
)
expect_equal(
  extract_theta(small_model, i = 1),
  mean(rstan::extract(small_model)$theta[, 1])
)
expect_equal(
  extract_tau(small_model),
  mean(rstan::extract(small_model)$tau)
)
expect_equal(
  extract_eta(small_model, i = 1),
  mean(rstan::extract(small_model)$eta[, 1])
)
expect_equal(
  extract_eta(small_model),
  apply(rstan::extract(small_model)$eta, 2, mean)
)
isq <- extract_isq(small_model)
expect_lt(isq, 1)
expect_gt(isq, 0)
