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
#' @name psnorm
#' @export
#' @param x,q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken
#'     to be the number required.
#' @param theta Numeric vector; The mean of the underlying effect size
#'     distribution.
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

dpsnorm = function(x, theta, sigma, alpha, eta, log = FALSE) {

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

#' @rdname psnorm
#' @export
ppsnorm = function(q, theta, sigma, alpha, eta, lower.tail = TRUE, log.p = FALSE) {

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

#' @rdname psnorm
#' @export
rpsnorm = function(n, theta, sigma, alpha, eta) {
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
}}
