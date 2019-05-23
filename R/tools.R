#' Extract parameters form an \code{ma} object.
#'
#' @name extract_parameter
#' @export
#' @param object The fitted \code{ma} object.
#' @param fun The function to apply to the parameters.
#' @param i Optional index, specifying which parameter among many to apply fun
#'     to. Only makes sense for \code{eta} and \code{theta}.
#' @return The same kind of object as \code{fun} returns.

extract_theta0 = function(object, fun = mean) fun(rstan::extract(object)$theta0)

#' @rdname extract_parameter
#' @export

extract_theta = function(object, fun = mean, i = NULL) {
  if(is.null(i)) {
    apply(rstan::extract(object)$theta, 2, fun)
  } else {
    fun(rstan::extract(object)$theta[, i])
  }
}

#' @rdname extract_parameter
#' @export

extract_tau = function(object, fun = mean) fun(rstan::extract(object)$tau)

#' @rdname extract_parameter
#' @export

extract_eta = function(object, fun = mean, i = NULL) {
  if(is.null(i)) {
    apply(rstan::extract(object)$eta, 2, fun)
  } else {
    fun(rstan::extract(object)$eta[, i])
  }
}


setGeneric("loo", package = "loo")

#' Calculate the \code{loo} for an \code{ma} object.
#'
#' @export
#' @docType methods
#' @param object The fitted \code{hma} object.
#' @return A \code{loo} object.

setMethod("loo", "mafit", function(x, ...) {

  dots = list(...)
  if(is.null(dots$marginal)) dots$marginal = FALSE
  if(dots$marginal) {
    if(x@bias != "p-hacking") {

      log_lik = loo::extract_log_lik(x,
                                     parameter_name = "log_lik_marginal",
                                     merge_chains = FALSE)

      if(is.null(dots$r_eff)) dots$r_eff = loo::relative_eff(exp(log_lik))
      do_call(.fn = loo::loo.array, .args = c(list(x = log_lik), dots))

    } else {

      llfun = function(data_i, draws) {

        N = length(draws$theta0)
        yi_i = data_i[1]
        vi_i = data_i[2]
        result_i = vector("numeric", N)

        for(n in 1:N) {
          eta = draws$eta[n, ]
          theta0 = draws$theta0[n]
          tau = draws$tau[n]

          f = function(theta) dph(yi_i/sqrt(vi_i), theta, 1, alpha, eta)*
            dnorm(theta, theta0/sqrt(vi_i), tau/sqrt(vi_i))

          result_i[n] = log(integrate(f = f, lower = -Inf, upper = Inf)$value) -
            log(sqrt(vi_i))
        }

        result_i

      }

      loo(llfun, draws = rstan::extract(x), data = cbind(x@yi, x@vi), ...)

    }
  } else {

    log_lik = loo::extract_log_lik(x, merge_chains = FALSE)
    if(is.null(dots$r_eff)) dots$r_eff = loo::relative_eff(exp(log_lik))
    do_call(.fn = loo::loo.array, .args = c(list(x = log_lik), dots))

  }

})

