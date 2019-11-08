#' Normalizing Constants for the Publication Selection Meta-Analysis Model
#'
#' Normalizing constants for the publication selection meta-Analysis model.
#'     These are used in several other functions. The underlying effect size
#'     distribution is normal and the selection is one-sided.
#'
#' The function \code{I} calculates the normalizing constant for the density of
#'     the observed effect sizes. The function \code{J} calculates the
#'     normalizing constant for the density of the effect size distribution.
#'
#' @keywords internal
#' @name normalizing_constant
#' @param sigma Numeric; The standard deviation of the study, due to
#'     sampling error.
#' @param theta Numeric; The mean of the underlying effect size.
#' @param theta0 Numeric; The mean of the underlying effect size
#'     distribution.
#' @param tau Numeric; The standard deviation of the underlying effect
#'     size distribution.
#' @param alpha Numeric vector; Specifies the thresholds for publication
#'     bias.
#' @param eta Numeric vector; Containing the probabilites of being a study
#'     with the given p-value from being published. This is normalized so that
#'     the maximumal element is 1.
#' @return The normalizing constant.

I = function(sigma, theta, alpha, eta) {
  k = length(alpha)
  cutoffs = stats::qnorm(1 - alpha)

  sapply(sigma, function(sigma) {
    cdfs = stats::pnorm(cutoffs, theta/sigma, 1)
    sum(sapply(1:(k - 1), function(i) eta[i]*(cdfs[i] - cdfs[i + 1])))
  })
}

#' @rdname normalizing_constant
J = function(sigma, theta0, tau, alpha, eta) {

  k = length(alpha)
  cutoffs = stats::qnorm(1 - alpha)

  sapply(sigma, function(sigma) {
    cdfs = stats::pnorm(cutoffs, theta0/sigma, sqrt(tau^2 + sigma^2)/sigma)
    sum(sapply(1:(k - 1), function(i) eta[i]*(cdfs[i] - cdfs[i + 1])))
  })

}

density_input_checker <- function(x, theta0 = NULL, theta = NULL, sigma = NULL,
                                 tau = NULL) {

  if(any(!is.numeric(x)))
    stop("'x' must be numeric")

  if (any(!is.numeric(c(theta0, theta, tau, sigma))))
    stop("parameters must be numeric")

  if (any(is.na(c(theta0, sigma, theta, tau))))
    stop("parameters cannot be na")

  if (any(tau <= 0))
    stop("'tau' must be positive")

  if (any(sigma <= 0))
    stop("'sigma' must be positive")

}
