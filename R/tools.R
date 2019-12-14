# Tools for the publipha package.
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

#' Extract Parameters from an `mafit` Object
#'
#' Extract samples from a model of class [`mafit`][mafit-class] and apply a
#' function `fun` to them.
#'
#' Support parameters for extraction are: The meta-analytic mean `theta0`, the
#' individual means `theta`, the heterogeneity parameter `tau`, the selection
#' bias parameter `eta`, and the I squared `isq`. See Higgins and Thompson
#' (2002) for details about I squared.
#'
#' All `extract_` functions are wrappers
#' around [`rstan::extract`][rstan::extract].
#'
#' @name ExtractParameters
#' @export
#' @param object an object of class [`mafit`][mafit-class].
#' @param fun the function to be applied to the fitted parameters.
#' @param i an optional index specifying which parameter to apply `fun` to. Only
#'    for `extract_eta` and `extract_theta`.
#' @return The result of `FUN` being applied to all estimated parameters of
#'    `object`.
#' @examples
#' \donttest{
#' set.seed(313)
#' model <- publipha::psma(yi = yi, vi = vi, data = dat.baskerville2012)
#' extract_theta0(model, mean) # [1] extract_theta0(model, mean)
#' extract_theta0(model, sd) # [1] 0.1095921
#' extract_tau(model, mean) # [1] 0.1315312
#' extract_theta(model, hist, i = 5)
#' }
#' @references Higgins, J. P., & Thompson, S. G. (2002). Quantifying
#' heterogeneity in a meta-analysis. Statistics in medicine, 21(11), 1539-1558.

extract_theta0 <- function(object, fun = mean) {
  fun(rstan::extract(object)$theta0)
}

#' @rdname ExtractParameters
#' @export

extract_theta <- function(object, fun = mean, i) {
  if (missing(i)) {
    apply(rstan::extract(object)$theta, 2, fun)
  } else {
    fun(rstan::extract(object)$theta[, i])
  }
}

#' @rdname ExtractParameters
#' @export

extract_tau <- function(object, fun = mean) fun(rstan::extract(object)$tau)

#' @rdname ExtractParameters
#' @export

extract_eta <- function(object, fun = mean, i) {
  if (missing(i)) {
    apply(rstan::extract(object)$eta, 2, fun)
  } else {
    fun(rstan::extract(object)$eta[, i])
  }
}

#' @rdname ExtractParameters
#' @export

extract_isq <- function(object, fun = mean) {
  sigma <- sqrt(object@vi)
  tau <- extract_tau(object, fun = identity)
  Isqs <- sapply(tau, function(tau) mean(tau^2 / (sigma^2 + tau^2)))
  fun(Isqs)
}
