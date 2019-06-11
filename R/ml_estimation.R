#' Maximum likelihood estimation of publication bias and p-hacking models
#'
#' @param yi Numeric vector of effect sies.
#' @param vi Numeric vector of variances.
#' @param alpha Numeric vector of thresholds.
#' @return List of maximum likelihood estimates.
#' @export

ml_psma = function(yi, vi, alpha = c(0, 0.025,0.05, 1)) {

  f = function(p) {

    theta0 = p[1]
    tau = p[2]
    eta = p[3:length(alpha)]

    -ll_psma(yi = yi,
             vi = vi,
             theta0 = theta0,
             tau = exp(tau),
             alpha = alpha,
             eta = c(1, pnorm(eta)))

  }

  p = c(0, 0, rep(0, length(alpha) - 2))

  optimum = nlm(f = f, p = p)
  estimate = optimum$estimate
  parameters = list(theta0 = estimate[1],
                     tau = exp(estimate[2]),
                     eta =  pnorm(estimate[3:length(estimate)]))
  attr(parameters, "maximum") = -optimum$minimum
  parameters

}

#' Llikelihood function of publication bias and p-hacking models
#'
#' @param yi Numeric vector of effect sies.
#' @param vi Numeric vector of variances.
#' @param theta0 Numeric mean.
#' @param tau Numeric standard deviation.
#' @param alpha Numeric vector of thresholds.
#' @param eta Numeric vector of publication probabilities.
#' @return Likelihood.

ll_psma = function(yi, vi, theta0, tau, alpha, eta) {
  mean(dmaps(yi,
             theta0 = theta0,
             tau = tau,
             sigma = sqrt(vi),
             alpha = alpha,
             eta = eta,
             log = TRUE))
}
