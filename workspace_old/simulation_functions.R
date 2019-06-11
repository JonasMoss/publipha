#' Simulate normal studies under one-sided publication bias.
#' 
#' @param vi Numeric vector of study variances.
#' @param alpha Numeric vector of cutoffs. Should include 0 and 1.
#' @param eta Numeric vector of publication probabilities.
#' @param theta0 Numeric scalar. Mean of effect size distribution.
#' @param tau Numeric scalar. Standard deviation of effect size distribution. 
#'     If equal to 0, fixed effects is used.
#' @return A list containing estimates \code{yi} and variances \code{vi}.

simulate_studies = function(vi, alpha, eta, theta0, tau) {
  n = length(vi)
  spec = "norm"
  
  pvalue = function(x) pnorm(-x)
  
  parameters = if(tau > 0) {
    function(i) list(mean = rnorm(1, mean = theta0, sd = tau)/sqrt(vi[i]))
  } else {
    function(i) list(mean = theta0/sqrt(vi[i]))
  }
  
  z = rpubli(n = n, 
             spec = spec, 
             parameters = parameters, 
             pvalue = pvalue,
             alpha = alpha, 
             eta = eta)
  
  simulated = data.frame(yi = z*sqrt(vi), vi = vi)
  # simulated$type = 5 # Normal, p-hacked
  # simulated$lower = qnorm(0.95)
  simulated
}
