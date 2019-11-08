context("densities-snorm")

set.seed(313)
x = rnorm(10)

theta0 = 0
tau = 0.05
sigma = 0.1
alpha = c(0, 0.025, 0.05, 1)
eta = c(1, 0.4, 0.1)

## Checks log.
expect_equal(
  log(dsnorm(x, theta0 = theta0, sigma = sigma, tau = tau, eta = eta)),
  dsnorm(x, theta0 = theta0, sigma = sigma, tau = tau, eta = eta, log = TRUE)
)

## Random variate generation.
n = 3
y = rep(1, n)

expect_equal({set.seed(1)
   rsnorm(n, theta0 = theta0, sigma = sigma, tau = tau, eta = eta)
   }, {
   set.seed(1); rsnorm(y, theta0 = theta0, sigma = sigma, tau = tau, eta = eta)
   },
)


expect_error(rsnorm(n, theta0 = theta0, sigma = sigma, tau = tau, eta = 1))

## Expectation.
integrand = function(theta)
  theta * dsnorm(theta, theta0, tau, sigma, alpha, eta)

expectation =  integrate(integrand, lower = -Inf, upper = Inf)$value

expect_equal(esnorm(theta0, tau, sigma, alpha, eta), expectation)

## Errors
expect_error(psnorm(x, theta0 = NA, sigma = sigma, tau = tau, eta = eta))
expect_error(psnorm(x, theta0 = theta0, sigma = -1, tau = tau, eta = eta))
expect_error(psnorm(x, theta0 = theta0, sigma = sigma, tau = 0, eta = eta))
expect_error(rsnorm(1, theta0 = NA, sigma = sigma, tau = tau, eta = eta))
expect_error(rsnorm(1, theta0 = theta0, sigma = -1, tau = tau, eta = eta))
expect_error(rsnorm(1, theta0 = theta0, sigma = sigma, tau = 0, eta = eta))
expect_error(dsnorm(x, theta0 = NA, sigma = sigma, tau = tau, eta = eta))
expect_error(dsnorm(x, theta0 = theta0, sigma = -1, tau = tau, eta = eta))
expect_error(dsnorm(x, theta0 = theta0, sigma = sigma, tau = 0, eta = eta))
