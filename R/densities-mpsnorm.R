#' Marginal Publication Selection Meta-analysis Model
#'
#' Density, distribution, quantile, random variate generation, and expectation
#'     calculation for the marginalized distribution for the publication
#'     selection meta-analysis model
#'
#' The effect size distribution for the publication selection model is not
#'     normal, but has itself been selected for. These functions assume a
#'     normal underyling effect size distribution and one-sided selection on the
#'     effects.
#'
#' @name mpsnorm
#' @export
#' @param x,q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
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
#' @param log,log.p Logical; If \code{TRUE}, probabilities are given as
#'     \code{log(p)}.
#' @param lower.tail Logical; If \code{TRUE}, the lower tail is returned.

rmpsnorm = function(n, theta0, tau, sigma, alpha, eta) {

  samples = rep(NA, n)
  sigma = rep_len(sigma, length.out = n)
  theta0 = rep_len(theta0 , length.out = n)
  tau = rep_len(tau, length.out = n)

  for(i in 1:n)  {
    while(TRUE) {
      proposal = stats::rnorm(1, theta0[i], sqrt(tau[i]^2 + sigma[i]^2))
      position =  .bincode(x = stats::pnorm(-proposal/sigma[i]),
                           breaks = alpha,
                           include.lowest = TRUE)

      if(stats::runif(1) < eta[position]) {
        samples[i] = proposal
        break
      }
    }
  }

  samples

}

#' @rdname mpsnorm
#' @export
dmpsnorm = function(x, theta0, tau, sigma, alpha, eta, log = FALSE) {

  if(any(tau <= 0)) stop("'tau' must be positive")
  cutoffs = stats::qnorm(1 - alpha)
  indices = .bincode(x/sigma, sort(cutoffs))
  constant = J(sigma, theta0, tau, alpha, eta)
  probabilities = rev(eta)[indices]

  if(!log) {
    densities = stats::dnorm(x = x, mean = theta0, sd = sqrt(sigma^2 + tau^2))
    densities*probabilities/constant
  } else {
    densities = stats::dnorm(x = x, mean = theta0, sd = sqrt(sigma^2 + tau^2), log = TRUE)
    densities + log(probabilities) - log(constant)
  }

}

#' @rdname mpsnorm
#' @export
pmpsnorm = function(q, theta0, tau, sigma, alpha, eta, lower.tail = TRUE, log.p = FALSE) {

  if(any(tau <= 0)) stop("'tau' must be positive")
  cutoffs = stats::qnorm(1 - alpha)
  indices = .bincode(q/sigma, sort(cutoffs))
  constant = J(sigma, theta0, tau, alpha, eta)
  probabilities = rev(eta)[indices]

  i = 1:(length(alpha) - 2)
  extra = c(0, rev(eta)[i]*(stats::pnorm(rev(cutoffs)[i + 1]*sigma,
                                 mean = theta0,
                                 sd = sqrt(sigma^2 + tau^2)) -
                            stats::pnorm(rev(cutoffs)[i]*sigma,
                                    mean = theta0,
                                    sd = sqrt(sigma^2 + tau^2))))
  extra = cumsum(extra)

  upper = stats::pnorm(q = q, mean = theta0, sd = sqrt(sigma^2 + tau^2))
  lower = stats::pnorm(q = rev(cutoffs)[indices]*sigma, mean = theta0, sd = sqrt(sigma^2 + tau^2))
  prob = ((upper - lower)*probabilities + extra[indices])/constant

  prob = if(lower.tail) prob else 1 - prob
  if(!log.p) prob else log(prob)

}
