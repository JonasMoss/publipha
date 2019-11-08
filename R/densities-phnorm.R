#' p-hacking Meta-analysis Model
#'
#' Density, distribution, quantile, random variate generation, and expectation
#'     calculation for the distribution for the publication selection
#'     meta-analysis model
#'
#' These functions assume one-sided selection on the effects, and do not take
#'     the effect size distribution into account. For that, see \code{mph}.
#'
#' @name phnorm
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

dphnorm <- function(x, theta, sigma, alpha = c(0, 0.025, 0.05, 1), eta, log = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(theta = theta, sigma = sigma)

  n <- length(x)
  theta <- rep_len(x = theta, length.out = n)
  sigma <- rep_len(x = sigma, length.out = n)
  u <- 1 - stats::pnorm(x / sigma)
  k <- length(alpha)
  inclusions <- .bincode(x = u, breaks = alpha, include.lowest = TRUE)
  probabilities <- eta / sum(eta)
  y <- rep.int(x = 0, times = n)

  for (i in (k - 1):min(inclusions)) {
    indices <- (inclusions <= i)
    cutoffs <- stats::qnorm(1 - alpha[i + 1]) * sigma[indices]
    y[indices] <- y[indices] + truncnorm::dtruncnorm(
      x = x[indices],
      mean = theta[indices],
      sd = sigma[indices],
      a = cutoffs
    ) * probabilities[i]
  }

  if (!log) y else log(y)
}

#' @rdname phnorm
#' @export
rphnorm <- function(n, theta, sigma, alpha = c(0, 0.025, 0.05, 1), eta) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(theta = theta, sigma = sigma)
  if (length(n) > 1) n <- length(n)

  shuffle <- sample(1:n)
  theta <- rep_len(theta, length.out = n)[shuffle]
  sigma <- rep_len(sigma, length.out = n)[shuffle]

  probabilities <- eta / sum(eta)

  numbers <- c(c(stats::rmultinom(1, n, probabilities)))
  cumulatives <- cumsum(c(0, numbers))

  samples <- vector("numeric", n)

  for (i in (1:length(eta))[numbers != 0]) {
    indices <- (cumulatives[i] + 1):cumulatives[i + 1]
    cutoff <- stats::qnorm(1 - alpha[i + 1])
    samples[indices] <- truncnorm::rtruncnorm(
      n = numbers[i],
      mean = theta[indices],
      sd = sigma[indices],
      a = cutoff * sigma[indices]
    )
  }

  samples[order(shuffle)]
}

#' @rdname phnorm
#' @export
pphnorm <- function(q, theta, sigma, alpha = c(0, 0.025, 0.05, 1), eta, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(theta = theta, sigma = sigma)

  n <- length(q)
  theta <- rep_len(x = theta, length.out = n)
  sigma <- rep_len(x = sigma, length.out = n)
  u <- 1 - stats::pnorm(q / sigma)
  k <- length(alpha)
  inclusions <- .bincode(x = u, breaks = alpha, include.lowest = TRUE)
  probabilities <- eta / sum(eta)
  y <- rep.int(x = 0, times = n)

  for (i in (k - 1):min(inclusions)) {
    indices <- (inclusions <= i)
    cutoffs <- stats::qnorm(1 - alpha[i + 1]) * sigma[indices]
    y[indices] <- y[indices] + truncnorm::ptruncnorm(
      q = q[indices],
      mean = theta[indices],
      sd = sigma[indices],
      a = cutoffs
    ) * probabilities[i]
  }

  if (!lower.tail) y <- 1 - y
  if (!log.p) y else log(y)
}
