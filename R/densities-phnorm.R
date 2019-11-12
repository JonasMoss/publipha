#' p-hacking Meta-analysis Model
#'
#' Density, distribution, and random variate generation for the p-hacking meta-
#'    analysis model.
#'
#' These functions assume one-sided selection on the effects. `alpha` contains
#'    the selection thresholds and `eta` the vector of *p*-hacking
#'    probabilities. `theta` is the true effect, while `sigma` is the true
#'    standard deviation before selection.
#'
#' @name phnorm
#' @export
#' @param x,q vector of quantiles.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken
#'     to be the number required.
#' @param theta vector of means.
#' @param sigma vector of study standard deviations.
#' @param alpha vector of thresholds for p-hacking.
#' @param eta vector of p-hacking probabilities, normalized to sum to 1.
#' @param log,log.p logical; If \code{TRUE}, probabilities are given as
#'     \code{log(p)}.
#' @param lower.tail logical; If \code{TRUE} (default), the probabilities are
#' \eqn{P[X\leq x]} otherwise, \eqn{P[X\geq x]}.
#' @references Moss, Jonas and De Bin, Riccardo. "Modelling publication
#' bias and p-hacking" Forthcoming (2019)
#'
#' @examples
#' rphnorm(100, theta = 0, sigma = 0.1, eta = c(1, 0.5, 0.1))
dphnorm <- function(x, theta, sigma, alpha = c(0, 0.025, 0.05, 1), eta,
                    log = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(x, theta = theta, sigma = sigma)

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
  density_input_checker(1, theta = theta, sigma = sigma)
  if (length(n) > 1) n <- length(n)

  shuffle <- sample(1:n)
  theta <- rep_len(theta, length.out = n)[shuffle]
  sigma <- rep_len(sigma, length.out = n)[shuffle]

  probabilities <- eta / sum(eta)

  numbers <- c(c(stats::rmultinom(1, n, probabilities)))
  cumulatives <- cumsum(c(0, numbers))

  samples <- vector("numeric", n)

  for (i in seq(eta)[numbers != 0]) {
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
pphnorm <- function(q, theta, sigma, alpha = c(0, 0.025, 0.05, 1), eta,
                    lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(q, theta = theta, sigma = sigma)

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
