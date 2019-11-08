setGeneric("loo", package = "loo")

#' Calculate the \code{loo} for an \code{ma} object.
#'
#' Computes PSIS-LOO CV, approximate leave-one-out cross-validation
#' using Pareto smoothed importance sampling, see [`loo`][loo::loo].
#'
#' `...` affect the function through one parameter, `marginal`. When `marginal`
#' is `TRUE`, the PSIS-LOO CV is based on the marginal likelihood, i.e. with
#' the dependence on `theta` integrated out. `marginal` defaults to true, as
#' recommended by?
#'
#' @param x an object of class `mafit`.
#' @param ... passed to [`loo`][loo::loo]. Only
#' @include ma.R
#' @docType methods
#' @export
#' @return A [`loo`][loo::loo] object.
#' @examples
#' \dontrun{
#' phma_model <- phma(yi, vi, data = metafor::dat.begg1989)
#' psma_model <- psma(yi, vi, data = metafor::dat.begg1989)
#' loo(phma_model)
#' loo(psma_model)
#' }

setMethod("loo", "mafit", function(x, ...) {
  dots <- list(...)

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
            dphnorm(yi_i / sqrt(vi_i), theta, 1, x@alpha, eta) *
              stats::dnorm(theta, theta0 / sqrt(vi_i), tau / sqrt(vi_i))
          }

          integral <- log(stats::integrate(
            f = f,
            lower = -Inf,
            upper = Inf
          )$value)

          result_i[n] <- integral - log(sqrt(vi_i))
        }

        result_i
      }

      loo(llfun, draws = rstan::extract(x), data = cbind(x@yi, x@vi), ...)
    }
  } else {
    log_lik <- loo::extract_log_lik(x, merge_chains = FALSE)

    if (is.null(dots$r_eff)) dots$r_eff <- loo::relative_eff(exp(log_lik))

    do_call(.fn = loo::loo.array, .args = c(list(x = log_lik), dots))
  }
})
