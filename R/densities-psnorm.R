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
#'     [`mpsnorm`][mpsnorm].
#'
#' @name psnorm
#' @export
#' @param x,q vector of quantiles.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken
#'     to be the number required.
#' @param theta vector of means.
#' @param sigma vector of study standard deviations.
#' @param alpha vector of thresholds for publication bias.
#' @param eta vector of publication probabilities, normalized to sum to 1.
#' @param log,log.p logical; If \code{TRUE}, probabilities are given as
#'     \code{log(p)}.
#' @param lower.tail logical; If \code{TRUE} (default), the probabilities are
#' \eqn{P[X\leq x]} otherwise, \eqn{P[X\geq x]}.
#' @references Hedges, Larry V. "Modeling publication selection effects
#' in meta-analysis." Statistical Science (1992): 246-255.
#'
#' Moss, Jonas and De Bin, Riccardo. "Modelling publication
#' bias and p-hacking" Forthcoming (2019)
#'
#' @examples
#' rpsnorm(100, theta = 0, sigma = 0.1, eta = c(1, 0.5, 0.1))
dpsnorm <- function(x, theta, sigma, alpha = c(0, 0.025, 0.05, 1), eta,
                    log = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(x, theta = theta, sigma = sigma)

  n <- length(x)

  theta <- rep_len(x = theta, length.out = n)
  sigma <- rep_len(x = sigma, length.out = n)

  u <- 1 - stats::pnorm(x / sigma)

  inclusions <- .bincode(x = u, breaks = alpha, include.lowest = TRUE)
  cutoffs <- stats::qnorm(1 - alpha)
  cdfs <- sapply(1:n, function(i) stats::pnorm(cutoffs, theta[i] / sigma[i], 1))
  probabilities <- eta * apply(cdfs, 2, diff) / c(eta %*% apply(cdfs, 2, diff))

  y <- rep.int(x = 0, times = n)

  for (i in unique(inclusions)) {
    indices <- (inclusions == i)
    lower <- stats::qnorm(1 - alpha[i + 1])
    upper <- stats::qnorm(1 - alpha[i]) * sigma[indices]
    y[indices] <- truncnorm::dtruncnorm(
      x = x[indices],
      mean = theta[indices],
      sd = sigma[indices],
      a = lower * sigma[indices],
      b = upper * sigma[indices]
    ) * probabilities[i]
  }

  if (!log) y else log(y)
}

#' @rdname psnorm
#' @export
ppsnorm <- function(q, theta, sigma, alpha = c(0, 0.025, 0.05, 1), eta,
                    lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(q, theta = theta, sigma = sigma)

  n <- length(q)
  theta <- rep_len(x = theta, length.out = n)
  sigma <- rep_len(x = sigma, length.out = n)

  u <- 1 - stats::pnorm(q / sigma)
  k <- length(alpha)

  inclusions <- .bincode(x = u, breaks = alpha, include.lowest = TRUE)
  cutoffs <- stats::qnorm(1 - alpha)
  cdfs <- sapply(1:n, function(i) stats::pnorm(cutoffs, theta[i] / sigma[i], 1))
  probabilities <- eta * apply(cdfs, 2, diff) / c(eta %*% apply(cdfs, 2, diff))

  y <- rep.int(x = 0, times = n)

  for (i in unique(inclusions)) {
    indices <- (inclusions == i)
    extra <- if (i < (k - 1)) sum(probabilities[(k - 1):(i + 1)]) else 0
    lower <- stats::qnorm(1 - alpha[i + 1])
    upper <- stats::qnorm(1 - alpha[i]) * sigma[indices]
    y[indices] <- truncnorm::ptruncnorm(
      q = q[indices],
      mean = theta[indices],
      sd = sigma[indices],
      a = lower * sigma[indices],
      b = upper * sigma[indices]
    ) * probabilities[i] + extra
  }

  if (!lower.tail) y <- 1 - y
  if (!log.p) y else log(y)
}

#' @rdname psnorm
#' @export
rpsnorm <- function(n, theta, sigma, alpha = c(0, 0.025, 0.05, 1), eta) {
  if (length(n) > 1) n <- length(n)

  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(1, theta = theta, sigma = sigma)

  theta <- rep_len(theta, length.out = n)
  sigma <- rep_len(sigma, length.out = n)

  k <- length(alpha)
  cutoffs <- stats::qnorm(1 - alpha)
  cdfs <- sapply(1:n, function(i) stats::pnorm(cutoffs, theta[i] / sigma[i], 1))
  probabilities <- eta * apply(cdfs, 2, diff) / c(eta %*% apply(cdfs, 2, diff))
  numbers <- apply(
    probabilities, 2,
    function(prob) sample(x = 1:(k - 1), size = 1, prob = prob)
  )

  samples <- vector("numeric", n)

  for (i in unique(numbers)) {
    indices <- (numbers == i)
    lower <- stats::qnorm(1 - alpha[i + 1])
    upper <- stats::qnorm(1 - alpha[i])
    samples[indices] <- truncnorm::rtruncnorm(
      n = sum(indices),
      mean = theta[indices],
      sd = sigma[indices],
      a = lower * sigma[indices],
      b = upper * sigma[indices]
    )
  }

  sample(samples)
}
