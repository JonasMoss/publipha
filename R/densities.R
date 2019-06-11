### ============================================================================
### This file contains the densities, cdfs, quantiles and random function
### generators.
###
### We have the functions for the densities of estimated effects under
### p-hacking, publication, bias and combined models. The supported setting is
### the normal with one-sided p-values. What's more, we have the corresponding
### functions for selected normal prior.
### ============================================================================

#' Normalizing Constants for the Publication Selection Meta-Analysis Model
#'
#' Normalizing constants for the publication selection meta-Analysis model.
#'     These are used in several other functions. The underlying effect size
#'     distribution is normal and the selection is one-sided.
#'
#' The function \code{I} calculates the normalizing constant for the density of
#'     the observed effect sizes. The function \code{J} calculates the
#'     normalizing constant for the density of the effect size distribution.
#'
#' @name normalizing_constant
#' @param sigma Numeric; The standard deviation of the study, due to
#'     sampling error.
#' @param theta Numeric; The mean of the underlying effect size.
#' @param theta0 Numeric; The mean of the underlying effect size
#'     distribution.
#' @param tau Numeric; The standard deviation of the underlying effect
#'     size distribution.
#' @param alpha Numeric vector; Specifies the thresholds for publication
#'     bias.
#' @param eta Numeric vector; Containing the probabilites of being a study
#'     with the given p-value from being published. This is normalized so that
#'     the maximumal element is 1.
#' @return The normalizing constant.

I = function(sigma, theta, alpha, eta) {
  k = length(alpha)
  cutoffs = stats::qnorm(1 - alpha)*sigma
  cdfs = stats::pnorm(cutoffs, theta, sigma)
  sum(sapply(1:(k - 1), function(i) eta[i]*(cdfs[i] - cdfs[i + 1])))
}

#' @rdname normalizing_constant
J = function(sigma, theta0, tau, alpha, eta) {
  k = length(alpha)
  cutoffs = stats::qnorm(1 - alpha)*sigma
  cdfs = stats::pnorm(cutoffs, theta0, sqrt(tau^2 + sigma^2))
  sum(sapply(1:(k - 1), function(i) eta[i]*(cdfs[i] - cdfs[i + 1])))
}

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

dsnorm = Vectorize(function(x, theta0, tau, sigma, alpha, eta, log = FALSE) {
  if(log) {
    log(I(sigma, x, alpha, eta)) + dnorm(x, theta0, tau, log = TRUE)-
      log(J(sigma, theta0, tau, alpha, eta))
  } else {
    I(sigma, x, alpha, eta)*dnorm(x, theta0, tau)/
      J(sigma, theta0, tau, alpha, eta)
  }

}, vectorize.args = c("x", "sigma", "theta0", "tau"))

#' @rdname snorm
#' @export
rsnorm = function(n, theta0, tau, sigma, alpha, eta) {

  samples = rep(NA, n)
  sigma = rep_len(sigma, length.out = n)
  theta0 = rep_len(theta0 , length.out = n)
  tau = rep_len(tau, length.out = n)

  for(i in 1:n)  {
    while(TRUE) {
      proposal = stats::rnorm(1, theta0[i], tau[i])
      probability = I(sigma[i], proposal, alpha, eta)
      if(probability > stats::runif(1)) {
        samples[i] = proposal
        break
      }
    }
  }

  samples

}

#' @rdname snorm
#' @export
esnorm = Vectorize(function(theta0, tau, sigma, alpha, eta) {
  integrand = function(theta)
    theta*desprior(theta, theta0, tau, sigma, alpha, eta)
  integrate(integrand, lower = -Inf, upper = Inf)$value
}, vectorize.args = c("sigma", "theta0", "tau"))


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
#' @name maps
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

rmaps = function(n, theta0, tau, sigma, alpha, eta) {

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

#' Publication Selection Meta-analysis Model
#'
#' Density, distribution, quantile, random variate generation, and expectation
#'     calculation for the distribution for the publication selection
#'     meta-analysis model
#'
#' The effect size distribution for the publication selection model is not
#'     normal, but has itself been selected for. These functions assume
#'     one-sided selection on the effects. These functions do not assume the
#'     existence of an underlying effect size distribution. For these, see
#'     \code{maps}.
#'
#' @name ps
#' @export
#' @param x,q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken
#'     to be the number required.
#' @param sigma Numeric vector; The standard deviation of the study, due to
#'     sampling error.
#' @param theta0 Numeric vector; The mean of the underlying effect size
#'     distribution.
#' @param tau Numeric vector; The standard deviation of the underlying effect
#'     size distribution.
#' @param alpha Numeric vector; Specifies the thresholds for publication
#'     bias.
#' @param eta Numeric vector; Containing the probabilites of being a study
#'     with the given p-value from being published. This is normalized so that
#'     the maximumal element is 1.
#' @param log,log.p Logical; If \code{TRUE}, probabilities are given as
#'     \code{log(p)}.
#' @param lower.tail Logical; If \code{TRUE}, the lower tail is returned.

dps = function(x, theta, sigma, alpha, eta, log = FALSE) {

  n = length(x)
  theta = rep_len(x = theta, length.out = n)
  sigma = rep_len(x = sigma, length.out = n)

  u = 1 - stats::pnorm(x/sigma)
  k = length(alpha)

  inclusions = .bincode(x = u, breaks = alpha, include.lowest = TRUE)
  cutoffs = stats::qnorm(1 - alpha)
  cdfs = sapply(1:n, function(i) stats::pnorm(cutoffs, theta[i]/sigma[i], 1))
  probabilities = eta*apply(cdfs, 2, diff)/c(eta%*%apply(cdfs, 2, diff))

  numbers = apply(probabilities, 2,
                  function(prob) sample(x = 1:(k - 1), size = 1, prob = prob))

  y = rep.int(x = 0, times = n)

  for(i in unique(inclusions)) {
    indices = (inclusions == i)
    lower = stats::qnorm(1 - alpha[i + 1])
    upper = stats::qnorm(1 - alpha[i])*sigma[indices]
    y[indices] = truncnorm::dtruncnorm(x = x[indices],
                                       mean = theta[indices],
                                       sd = sigma[indices],
                                       a = lower*sigma[indices],
                                       b = upper*sigma[indices])*probabilities[i]
  }

  if(!log) y else log(y)

}

#' @rdname ps
#' @export
pps = function(q, theta, sigma, alpha, eta, lower.tail = TRUE, log.p = FALSE) {

  n = length(q)
  theta = rep_len(x = theta, length.out = n)
  sigma = rep_len(x = sigma, length.out = n)

  u = 1 - stats::pnorm(q/sigma)
  k = length(alpha)

  inclusions = .bincode(x = u, breaks = alpha, include.lowest = TRUE)
  cutoffs = stats::qnorm(1 - alpha)
  cdfs = sapply(1:n, function(i) stats::pnorm(cutoffs, theta[i]/sigma[i], 1))
  probabilities = eta*apply(cdfs, 2, diff)/c(eta%*%apply(cdfs, 2, diff))

  numbers = apply(probabilities, 2,
                  function(prob) sample(x = 1:(k - 1), size = 1, prob = prob))

  y = rep.int(x = 0, times = n)

  for(i in unique(inclusions)) {
    indices = (inclusions == i)
    extra = if(i < (k - 1)) sum(probabilities[(k - 1):(i + 1)]) else 0
    lower = stats::qnorm(1 - alpha[i + 1])
    upper = stats::qnorm(1 - alpha[i])*sigma[indices]
    y[indices] = truncnorm::ptruncnorm(q = q[indices],
                                       mean = theta[indices],
                                       sd = sigma[indices],
                                       a = lower*sigma[indices],
                                       b = upper*sigma[indices])*probabilities[i] +
      extra
  }

  if(!lower.tail) 1 - y
  if(!log.p) y else log(y)

}

#' @rdname ps
#' @export
rps = function(n, theta, sigma, alpha, eta) {
  stopifnot(length(alpha) == (length(eta) + 1))

  theta = rep_len(theta, length.out = n)
  sigma = rep_len(sigma, length.out = n)

  k = length(alpha)
  cutoffs = stats::qnorm(1 - alpha)
  cdfs = sapply(1:n, function(i) stats::pnorm(cutoffs, theta[i]/sigma[i], 1))
  probabilities = eta*apply(cdfs, 2, diff)/c(eta%*%apply(cdfs, 2, diff))
  numbers = apply(probabilities, 2,
                  function(prob) sample(x = 1:(k - 1), size = 1, prob = prob))

  samples = vector("numeric", n)

  for(i in unique(numbers)) {
    indices = (numbers == i)
    lower = stats::qnorm(1 - alpha[i + 1])
    upper = stats::qnorm(1 - alpha[i])
    samples[indices] = truncnorm::rtruncnorm(n = sum(indices),
                                             mean = theta[indices],
                                             sd = sigma[indices],
                                             a = lower*sigma[indices],
                                             b = upper*sigma[indices])
  }


  sample(samples)
}

#' p-hacking Meta-analysis Model
#'
#' Density, distribution, quantile, random variate generation, and expectation
#'     calculation for the distribution for the publication selection
#'     meta-analysis model
#'
#' These functions assume one-sided selection on the effects, and do not take
#'     the effect size distribution into account. For that, see \code{mph}.
#'
#' @name ph
#' @export
#' @param x,q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken
#'     to be the number required.
#' @param theta Numeric vector; The mean of the underlying normal distribution.
#' @param sigma Numeric vector; The standard deviation of the study, due to
#'     sampling error.
#' @param alpha Numeric vector; Specifies the thresholds for publication
#'     bias.
#' @param eta Numeric vector; The mixing probability for each component.
#' @param log,log.p Logical; If \code{TRUE}, probabilities are given as
#'     \code{log(p)}.
#' @param lower.tail Logical; If \code{TRUE}, the lower tail is returned.

dph = function(x, theta, sigma, alpha, eta, log = FALSE) {

  n = length(x)
  theta = rep_len(x = theta, length.out = n)
  sigma = rep_len(x = sigma, length.out = n)
  u = 1 - stats::pnorm(x/sigma)
  k = length(alpha)
  inclusions = .bincode(x = u, breaks = alpha, include.lowest = TRUE)
  probabilities = eta/sum(eta)
  y = rep.int(x = 0, times = n)

  for(i in (k-1):min(inclusions)) {
    indices = (inclusions <= i)
    cutoffs = stats::qnorm(1 - alpha[i + 1])*sigma[indices]
    y[indices] = y[indices] + truncnorm::dtruncnorm(x = x[indices],
                                                    mean = theta[indices],
                                                    sd = sigma[indices],
                                                    a = cutoffs)*probabilities[i]
  }

  if(!log) y else log(y)

}

#' @rdname ph
#' @export
rph = function(n, theta, sigma, alpha, eta) {

  stopifnot(length(alpha) == (length(eta) + 1))

  shuffle = sample(1:n)
  theta = rep_len(theta, length.out = n)[shuffle]
  sigma = rep_len(sigma, length.out = n)[shuffle]

  probabilities = eta/sum(eta)

  numbers = c(c(stats::rmultinom(1, n, probabilities)))
  cumulatives = cumsum(c(0, numbers))

  samples = vector("numeric", n)

  for(i in (1:length(eta))[numbers != 0]) {
    indices = (cumulatives[i] + 1):cumulatives[i + 1]
    cutoff = stats::qnorm(1 - alpha[i + 1])
    samples[indices] = truncnorm::rtruncnorm(n = numbers[i],
                                             mean = theta[indices],
                                             sd = sigma[indices],
                                             a = cutoff*sigma[indices])
  }

  samples[order(shuffle)]

}

#' @rdname ph
#' @export

pph = function(q, theta, sigma, alpha, eta, lower.tail = TRUE, log.p = FALSE) {

  n = length(q)
  theta = rep_len(x = theta, length.out = n)
  sigma = rep_len(x = sigma, length.out = n)
  u = 1 - stats::pnorm(q/sigma)
  k = length(alpha)
  inclusions = .bincode(x = u, breaks = alpha, include.lowest = TRUE)
  probabilities = eta/sum(eta)
  y = rep.int(x = 0, times = n)

  for(i in (k-1):min(inclusions)) {
    indices = (inclusions <= i)
    cutoffs = stats::qnorm(1 - alpha[i + 1])*sigma[indices]
    y[indices] = y[indices] + truncnorm::ptruncnorm(q = q[indices],
                                                    mean = theta[indices],
                                                    sd = sigma[indices],
                                                    a = cutoffs)*probabilities[i]
  }

  if(!lower.tail) 1 - y
  if(!log.p) y else log(y)


}
