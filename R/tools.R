#' Extract parameters from an \code{ma} object.
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

#' @rdname extract_parameter
#' @export

extract_Isq= function(object, fun = mean) {

    alpha = data$alphas
    sigma = mean(sqrt(data$vi))
    tau0s = rstan::extract(object)$sigma0
    mean(tau0s^2/(sigma[i]^2 + tau0s^2)
    c(mean(sapply(1:length(sigma), function(i) mean(tau0s^2/(sigma[i]^2 + tau0s^2)))),
      mean(tau0s^2/(mean(sigma)^2 + tau0s^2)),
      mean(tau0s^2)/mean((mean(sigma)^2 + tau0s^2)))
  }
  if(is.null(i)) {
    apply(rstan::extract(object)$eta, 2, fun)
  } else {
    fun(rstan::extract(object)$eta[, i])
  }
}
