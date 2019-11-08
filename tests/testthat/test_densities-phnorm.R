context("densities-phnorm")

set.seed(313)
x = rnorm(10)

theta = 0
sigma = 1
alpha = c(0, 0.025, 0.05, 1)
eta = c(1, 0.4, 0.1)

## Density
expect_equal(
  log(dphnorm(x, theta = theta, sigma = sigma, eta = eta)),
  dphnorm(x, theta = theta, sigma = sigma, eta = eta, log = TRUE)
)

## Random variate generation
n = 3
y = rep(1, n)

expect_equal({set.seed(1)
  rphnorm(n, theta = theta, sigma = sigma, eta = eta)
}, {
  set.seed(1); rphnorm(y, theta = theta, sigma = sigma, eta = eta)
},
)

expect_error(rphnorm(n, theta = theta, sigma = sigma, eta = 1))

## CDF
expect_equal(
  log(pphnorm(x, theta = theta, sigma = sigma, eta = eta)),
  pphnorm(x, theta = theta, sigma = sigma, eta = eta, log = TRUE)
)

expect_equal(
  1 - pphnorm(x, theta = theta, sigma = sigma, eta = eta),
  pphnorm(x, theta = theta, sigma = sigma, eta = eta, lower.tail = FALSE)
)

## Errors
expect_error(pphnorm(x, theta = NA, sigma = sigma, eta = eta))
expect_error(pphnorm(x, theta = theta, sigma = -1, eta = eta))
expect_error(rphnorm(1, theta = NA, sigma = sigma, eta = eta))
expect_error(rphnorm(1, theta = theta, sigma = -1, eta = eta))
expect_error(dphnorm(x, theta = NA, sigma = sigma, eta = eta))
expect_error(dphnorm(x, theta = theta, sigma = -1, eta = eta))
expect_error(pphnorm("x", theta = theta, sigma = sigma, eta = eta))
