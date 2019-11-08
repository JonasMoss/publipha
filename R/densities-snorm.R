#' Selected Normal Effect Size Distribution
#'
#' Density, distribution, quantile, random variate generation, and expectation
#'     calculation for the effect size distribution for the selected effect size
#'     distribution of the one-sided normal discrete probability vector
#'     publication bias model.
#'
#' The effect size distribution for the publication selection model is not
#'     normal, but has itself been selected for. These functions assume a
#'     normal underyling effect size distribution and one-sided selection on the
#'     effects.
#'
#' @name snorm
#' @export
#' @param x Numeric vector of quantiles.
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken
#'     to be the number required.
#' @param theta0 Numeric vector; The mean of the underlying effect size
#'     distribution.
#' @param tau Numeric vector; The standard deviation of the underlying effect
#'     size distribution.
#' @param sigma Numeric vector; The standard deviation of the study, due to
#'     sampling error.
#' @param alpha Numeric vector; Specifies the thresholds for publication
#'     bias.
#' @param eta Numeric vector; Containing the probabilites of being a study
#'     with the given p-value from being published. This is normalized so that
#'     the maximumal element is 1.
#' @param log Logical; If \code{TRUE}, probabilities are given as
#'     \code{log(p)}.
dsnorm <- Vectorize(function(x, theta0, tau, sigma,
                             alpha = c(0, 0.025, 0.05, 1),
                             eta, log = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(theta0 = theta0, tau = tau, sigma = sigma)

  if (log) {
    log(I(sigma, x, alpha, eta)) + stats::dnorm(x, theta0, tau, log = TRUE) -
      log(J(sigma, theta0, tau, alpha, eta))
  } else {
    I(sigma, x, alpha, eta) * stats::dnorm(x, theta0, tau) /
      J(sigma, theta0, tau, alpha, eta)
  }
}, vectorize.args = c("x", "theta0", "tau"))

#' @rdname snorm
#' @export
rsnorm <- function(n, theta0, tau, sigma, alpha = c(0, 0.025, 0.05, 1), eta) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(theta0 = theta0, tau = tau, sigma = sigma)
  if (length(n) > 1) n <- length(n)

  stopifnot(length(alpha) == (length(eta) + 1))

  samples <- rep(NA, n)
  sigma <- rep_len(sigma, length.out = n)
  theta0 <- rep_len(theta0, length.out = n)
  tau <- rep_len(tau, length.out = n)

  for (i in 1:n) {
    while (TRUE) {
      proposal <- stats::rnorm(1, theta0[i], tau[i])
      probability <- I(sigma[i], proposal, alpha, eta)

      if (probability > stats::runif(1)) {
        samples[i] <- proposal
        break
      }
    }
  }

  samples
}

#' @rdname snorm
#' @export
esnorm <- Vectorize(function(theta0, tau, sigma, alpha, eta) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(theta0 = theta0, tau = tau, sigma = sigma)
  integrand <- function(theta) {
    theta * dsnorm(theta, theta0, tau, sigma, alpha, eta)
  }

  integrate(integrand, lower = -Inf, upper = Inf)$value
}, vectorize.args = c("sigma", "theta0", "tau"))
