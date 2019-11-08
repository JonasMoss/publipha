context("ma")

wrap <- function(x) suppressWarnings(capture_output(x))

# The wrappers
set.seed(313)
wrap(psma_model_psma <- publipha::psma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10, refresh = 0
))

set.seed(313)
wrap(psma_model_ma <- publipha::ma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10,
  bias = "publication selection", refresh = 0
))

set.seed(313)
wrap(phma_model_phma <- publipha::phma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10, refresh = 0
))

set.seed(313)
wrap(phma_model_ma <- publipha::ma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10,
  bias = "p-hacking", refresh = 0
))

set.seed(313)
wrap(cma_model_cma <- publipha::cma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10, refresh = 0
))
set.seed(313)
wrap(cma_model_ma <- publipha::ma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10,
  bias = "none", refresh = 0
))

expect_equal(extract_theta0(psma_model_psma), extract_theta0(psma_model_ma))
expect_equal(extract_theta0(phma_model_phma), extract_theta0(phma_model_ma))
expect_equal(extract_theta0(cma_model_cma), extract_theta0(cma_model_ma))

## Errors
expect_error(publipha::psma(
  yi = yi, vi = vi, effects = "fixed",
  data = dat.baskerville2012
))
expect_error(publipha::psma(
  yi = yi, vi = vi, likelihood = "fnormal",
  data = dat.baskerville2012
))
expect_error(publipha::ma(vi, yi,
  data = dat.baskerville2012,
  bias = "haha"
))
expect_error(publipha::phma(
  yi = yi, vi = vi, data = dat.baskerville2012,
  chains = 1, iter = 10, refresh = 0, prior = list(a = 5)
))

## Unequal

prior <- list(
  eta0 = c(3, 2, 1),
  theta0_mean = 10,
  theta0_sd = 0.1,
  tau_mean = 1,
  tau_sd = 1
)

set.seed(313)
wrap(model1 <- publipha::ma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10,
  bias = "none", refresh = 0
))

set.seed(313)
wrap(model2 <- publipha::ma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10,
  bias = "none", refresh = 0, prior = prior
))

expect_lt(extract_theta0(model1), extract_theta0(model2))


## Allma
wrap(model <- publipha::allma(
  yi = yi, vi = vi,
  data = dat.baskerville2012,
  chains = 1,
  iter = 10, refresh = 0, prior = prior
))

expect_equal(length(model), 3)
