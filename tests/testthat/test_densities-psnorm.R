context("densities-psnorm")

set.seed(313)
x = rnorm(10)

theta = 0
sigma = 1
alpha = c(0, 0.025, 0.05, 1)
eta = c(1, 0.4, 0.1)

## Density
expect_equal(
  log(dpsnorm(x, theta = theta, sigma = sigma, eta = eta)),
  dpsnorm(x, theta = theta, sigma = sigma, eta = eta, log = TRUE)
)

## Random variate generation
n = 3
y = rep(1, n)

expect_equal({set.seed(1)
  rpsnorm(n, theta = theta, sigma = sigma, eta = eta)
}, {
  set.seed(1); rpsnorm(y, theta = theta, sigma = sigma, eta = eta)
},
)

expect_error(rpsnorm(n, theta = theta, sigma = sigma, eta = 1))

## CDF
expect_equal(
  log(ppsnorm(x, theta = theta, sigma = sigma, eta = eta)),
  ppsnorm(x, theta = theta, sigma = sigma, eta = eta, log = TRUE)
)

expect_equal(
  1 - ppsnorm(x, theta = theta, sigma = sigma, eta = eta),
  ppsnorm(x, theta = theta, sigma = sigma, eta = eta, lower.tail = FALSE)
)


## Errors
expect_error(ppsnorm(x, theta = NA, sigma = sigma, eta = eta))
expect_error(ppsnorm(x, theta = theta, sigma = -1, eta = eta))
expect_error(rpsnorm(1, theta = NA, sigma = sigma, eta = eta))
expect_error(rpsnorm(1, theta = theta, sigma = -1, eta = eta))
expect_error(dpsnorm(x, theta = NA, sigma = sigma, eta = eta))
expect_error(dpsnorm(x, theta = theta, sigma = -1, eta = eta))

