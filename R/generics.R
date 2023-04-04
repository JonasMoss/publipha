# Generics for the publipha package
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


setGeneric("loo", package = "loo")

#' Calculate the \code{loo} for an \code{ma} object.
#'
#' Computes PSIS-LOO CV, approximate leave-one-out cross-validation
#' using Pareto smoothed importance sampling, see [`loo`][loo::loo].
#'
#' `...` affect the function through two parameters, `marginal` and
#' `lower_bound`. When `marginal`is `TRUE`, the PSIS-LOO CV is based on the
#' marginal likelihood, i.e. with the dependence on `theta` integrated out.
#' `marginal` defaults to `TRUE`. `lower_bound` species the lower bound where
#' log-likelihoods are dropped; this is only used in the *p*-hacking model
#' and defaults to -6.
#'
#' @param x an object of class `mafit`.
#' @param ... passed to [`loo`][loo::loo]. Only
#' @include ma.R
#' @docType methods
#' @export
#' @return A [`loo`][loo::loo] object.
#' @examples
#' \donttest{
#' phma_model <- phma(yi, vi, data = metadat::dat.begg1989)
#' psma_model <- psma(yi, vi, data = metadat::dat.begg1989)
#' loo(phma_model)
#' loo(psma_model)
#' }

setMethod("loo", "mafit", function(x, ...) {
  dots <- list(...)

  if(is.null(dots$to_na)) dots$lower_bound = -6
  lower_bound = dots$lower_bound

  if (is.null(dots$marginal)) dots$marginal <- TRUE

  if (dots$marginal) {
    if (x@bias != "p-hacking") {
      log_lik <- loo::extract_log_lik(x,
                                      parameter_name = "log_lik_marginal",
                                      merge_chains = FALSE
      )

      if (is.null(dots$r_eff)) dots$r_eff <- loo::relative_eff(exp(log_lik))
      do_call(.fn = loo::loo.array, .args = c(list(x = log_lik), dots))
    } else {

      llfun <- function(data_i, draws) {
        N <- length(draws$theta0)
        yi_i <- data_i[1]
        vi_i <- data_i[2]
        result_i <- vector("numeric", N)

        for (n in 1:N) {
          eta <- draws$eta[n, ]
          theta0 <- draws$theta0[n]
          tau <- draws$tau[n]

          f <- function(theta) {
            publipha::dphnorm(yi_i / sqrt(vi_i), theta, 1, x@alpha, eta) *
              stats::dnorm(theta, theta0 / sqrt(vi_i), tau / sqrt(vi_i))
          }

          integral <- tryCatch({ log(stats::integrate(
            f = f,
            lower = -Inf,
            upper = Inf
          )$value)},  error = function(e) NA)

          result_i[n] <- integral - log(sqrt(vi_i))
        }

        result_i
      }

      data = cbind(x@yi, x@vi)
      draws = rstan::extract(x)

      values = sapply(seq_along(x@yi), function(i) llfun(data[i, ], draws))
      values[is.infinite(values)] = NA
      values[values < lower_bound] = NA
      values = values[!is.na(rowSums(values)), ]

      loo(values)
    }
  } else {
    log_lik <- loo::extract_log_lik(x, merge_chains = FALSE)

    if (is.null(dots$r_eff)) dots$r_eff <- loo::relative_eff(exp(log_lik))

    do_call(.fn = loo::loo.array, .args = c(list(x = log_lik), dots))
  }
})

