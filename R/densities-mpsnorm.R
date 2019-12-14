# Marginal Publication Selection Meta-analysis Model
# Copyright (C) 2019 Jonas Moss
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
# USA.

#' Marginal Publication Selection Meta-analysis Model
#'
#' Density, distribution, and random variate generation for the marginalized
#'     distribution of the publication selection meta-analysis model
#'
#' These functions assume a normal underlying effect size distribution and
#'     one-sided selection on the effects. For the fixed effects publication
#'     bias model see [`psnorm`][psnorm].
#'
#' @name mpsnorm
#' @export
#' @param x,q vector of quantiles.
#' @param n number of observations. If \code{length(n) > 1}, the length is taken
#'     to be the number required.
#' @param theta0 vector of means.
#' @param tau vector of heterogeneity parameters.
#' @param sigma vector of study standard deviations.
#' @param alpha vector of thresholds for publication bias.
#' @param eta vector of publication probabilities, normalized to sum to 1.
#' @param log,log.p logical; If \code{TRUE}, probabilities are given as
#'     \code{log(p)}.
#' @param lower.tail logical; If \code{TRUE} (default), the probabilities are
#' \eqn{P[X\leq x]} otherwise, \eqn{P[X\geq x]}.
#' @return `dmpsnorm` gives the density, `pmpsnorm` gives the distribution
#'    function, and `rmpsnorm` generates random deviates.
#' @references Hedges, Larry V. "Modeling publication selection effects
#' in meta-analysis." Statistical Science (1992): 246-255.
#'
#' Moss, Jonas and De Bin, Riccardo. "Modelling publication
#' bias and p-hacking" Forthcoming (2019)
#'
#' @examples
#' rmpsnorm(100, theta0 = 0, tau = 0.1, sigma = 0.1, eta = c(1, 0.5, 0.1))
#' @rdname mpsnorm
#' @export
dmpsnorm <- function(x, theta0, tau, sigma, alpha = c(0, 0.025, 0.05, 1), eta,
                     log = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(x, theta0 = theta0, tau = tau, sigma = sigma)

  cutoffs <- stats::qnorm(1 - alpha)
  indices <- .bincode(x / sigma, sort(cutoffs))
  constant <- J(sigma, theta0, tau, alpha, eta)
  probabilities <- rev(eta)[indices]

  if (!log) {
    densities <- stats::dnorm(x = x, mean = theta0, sd = sqrt(sigma^2 + tau^2))
    densities * probabilities / constant
  } else {
    densities <- stats::dnorm(
      x = x, mean = theta0, sd = sqrt(sigma^2 + tau^2),
      log = TRUE
    )
    densities + log(probabilities) - log(constant)
  }
}


#' @rdname mpsnorm
#' @export
pmpsnorm <- function(q, theta0, tau, sigma, alpha = c(0, 0.025, 0.05, 1),
                     eta, lower.tail = TRUE, log.p = FALSE) {
  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(q, theta0 = theta0, tau = tau, sigma = sigma)

  cutoffs <- stats::qnorm(1 - alpha)
  indices <- .bincode(q / sigma, sort(cutoffs))
  constant <- J(sigma, theta0, tau, alpha, eta)
  probabilities <- rev(eta)[indices]

  i <- 1:(length(alpha) - 2)
  extra <- c(0, rev(eta)[i] * (stats::pnorm(rev(cutoffs)[i + 1] * sigma,
    mean = theta0,
    sd = sqrt(sigma^2 + tau^2)
  ) -
    stats::pnorm(rev(cutoffs)[i] * sigma,
      mean = theta0,
      sd = sqrt(sigma^2 + tau^2)
    )))
  extra <- cumsum(extra)

  upper <- stats::pnorm(q = q, mean = theta0, sd = sqrt(sigma^2 + tau^2))
  lower <- stats::pnorm(
    q = rev(cutoffs)[indices] * sigma, mean = theta0,
    sd = sqrt(sigma^2 + tau^2)
  )
  prob <- ((upper - lower) * probabilities + extra[indices]) / constant

  prob <- if (lower.tail) prob else 1 - prob
  if (!log.p) prob else log(prob)
}

#' @rdname mpsnorm
#' @export
rmpsnorm <- function(n, theta0, tau, sigma, alpha = c(0, 0.025, 0.05, 1), eta) {
  if (length(n) > 1) n <- length(n)

  stopifnot(length(alpha) == (length(eta) + 1))
  density_input_checker(1, theta0 = theta0, tau = tau, sigma = sigma)

  samples <- rep(NA, n)
  sigma <- rep_len(sigma, length.out = n)
  theta0 <- rep_len(theta0, length.out = n)
  tau <- rep_len(tau, length.out = n)

  for (i in 1:n) {
    while (TRUE) {
      proposal <- stats::rnorm(1, theta0[i], sqrt(tau[i]^2 + sigma[i]^2))
      position <- .bincode(
        x = stats::pnorm(-proposal / sigma[i]),
        breaks = alpha,
        include.lowest = TRUE
      )

      if (stats::runif(1) < eta[position]) {
        samples[i] <- proposal
        break
      }
    }
  }

  samples
}
