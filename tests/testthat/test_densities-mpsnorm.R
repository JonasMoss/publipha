context("densities-mpsnorm")

set.seed(313)
x = rnorm(10)

theta0 = 0
tau = 0.05
sigma = 0.1
alpha = c(0, 0.025, 0.05, 1)
eta = c(1, 0.4, 0.1)

## Density
expect_equal(
  log(dmpsnorm(x, theta0 = theta0, sigma = sigma, tau = tau, eta = eta)),
  dmpsnorm(x, theta0 = theta0, sigma = sigma, tau = tau,eta = eta, log = TRUE)
)

## Random variate generation
n = 3
y = rep(1, n)

expect_equal({set.seed(1)
  rmpsnorm(n, theta0 = theta0, sigma = sigma, tau = tau, eta = eta)
}, {
  set.seed(1); rmpsnorm(y, theta0 = theta0, sigma = sigma, tau = tau, eta = eta)
},
)

expect_error(rmpsnorm(n, theta = theta, sigma = sigma, eta = 1))

## CDF
expect_equal(
  log(pmpsnorm(x, theta0 = theta0, sigma = sigma, tau = tau, eta = eta)),
  pmpsnorm(x, theta0 = theta0, sigma = sigma, tau = tau, eta = eta, log = TRUE)
)

expect_equal(
  1 - pmpsnorm(x, theta0 = theta0, sigma = sigma, tau = tau, eta = eta),
  pmpsnorm(x, theta0 = theta0, sigma = sigma, tau = tau, eta = eta, lower.tail = FALSE)
)
