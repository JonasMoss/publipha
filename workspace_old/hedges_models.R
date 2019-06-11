Sys.setenv(USE_CXX14 = 1)
library("rstan")

meta_analysis_models = new.env()
meta_analysis_models$hedges_normal_random = rstan::stan_model(
  file = "stan/hedges_normal_random.stan", 
  model_name = "Hedges meta-analysis, normal likelihood, normal random effects")

meta_analysis_models$hedges_fnormal_random = rstan::stan_model(
  file = "stan/hedges_fnormal_random.stan", 
  model_name = "Hedges meta-analysis, folded normal likelihood, folded normal random effects")


meta_analysis_models$hedges_fixed = rstan::stan_model(
  file = "stan/hedges_fixed.stan", 
  model_name = "Hedges' PSB Meta-analysis, Fixed Effects")

meta_analysis_models$classical_random = rstan::stan_model(
  file = "stan/classical_random.stan", 
  model_name = "Classical Meta-analysis, Random Effects")

meta_analysis_models$classical_fixed = rstan::stan_model(
  file = "stan/classical_fixed.stan", 
  model_name = "Classical Meta-analysis, Fixed Effects")


#' Bayesian Meta-analysis with Publication Selection
#' 
#' @param data A list containing the effect size estimates \code{yi} and 
#'     variances \code{vi}.
#' @param effects The type of meta-analysis model to use.
#' @param parameters A list of parameters, including the vector of cuttoffs
#'     alpha, the parameter for the Dirichlet prior, and parameters for the 
#'     normal prior for theta0 and truncated normal prior for tau.
#' @param classical Logical; If \code{TRUE}, runs a classical meta-analysis. If
#'     \code{FALSE}, runs a Hedges meta-analysis.
#' @param ... Passed to \code{rstan::sampling}.


hedges = function(data, 
                  effects = c("random", "fixed"), 
                  likelihood = c("normal", "fnormal"), 
                  parameters = NULL, 
                  classical = FALSE, ...) {
  
  praenomen = if (classical) "classical" else "hedges"
  nomen = paste0(praenomen, "_", likelihood, "_", effects)
  
  sizes = list(N = length(data$yi),
               k = length(parameters$alpha))
  
  obj = rstan::sampling(object = meta_analysis_models[[nomen]], 
                        data = c(data, parameters, sizes), ...)
  attr(obj, "data") = data
  attr(obj, "parameters") = parameters
  obj
}

hedges_loo = function(object) loo::loo(loo::extract_log_lik(object))

#' Calculate the standard deviation ratio
#' 
#' Calculate the standard deviation for the posterior mean between two models.
#'    The first standard deviation is supplied, while the other is calculated 
#'    as if it were from a random effects meta-analysis with a normal prior 
#'    for the mean.
#' 
#' @param sigma Numeric; Standard deviation of the posterior mean for the 
#'     model.
#' @param tau Numeric vector; The standard deviaiton of the effect size 
#'     distribution. Set equal to zero for a fixed effects model. Is averaged
#'     over if it is a vector.
#' @param theta0_sigma Numeric; Standard deviation for the prior on the mean
#'     effect.
#' @param vi Numeric vector; Vector of study-specific variances.
#' @return Numeric; The standard deviation ratio

sd_ratio = function(sigma, tau, theta0_sigma, vi) {
  f = function(tau) sigma/sqrt(1/(theta0_sigma + sum(1/(vi + tau^2))))
  if(length(tau) > 1) mean(sapply(tau, f)) else f(tau)
}


theta0 = function(object, fun = mean) fun(rstan::extract(object)$theta0)

theta = function(object, fun = mean) apply(rstan::extract(object)$theta, 2, fun)

tau = function(object, fun = mean) fun(rstan::extract(object)$theta0)


bias_rate = function(object, data, Nreps = 1000) {
  
  alpha = data$alphas
  sigma = sqrt(data$vi)
  
  theta0s = rstan::extract(object)$theta0
  tau0s = rstan::extract(object)$sigma0
  weights = rstan::extract(object)$weights
  indices = sample.int(length(tau0), Nreps, replace = FALSE)
  
  samples = sapply(indices, function(i) 
    expectation(sigma, tau0s[i], theta0s[i], alpha, weights[i, ]))
  
  EX = mean(samples)
  EY = mean(rstan::extract(object)$theta0)
  
  EX/EY
}

hedges_expectation_x = function(object, data, Nreps = 1000, weighted = TRUE) {
    
    alpha = data$alphas
    sigma = sqrt(data$vi)
    
    theta0s = rstan::extract(object)$theta0
    tau0s = rstan::extract(object)$sigma0
    weights = rstan::extract(object)$weights
    indices = sample.int(length(tau0), Nreps, replace = FALSE)
    
    samples = sapply(indices, function(i) 
      expectation(sigma, tau0s[i], theta0s[i], alpha, weights[i, ]))
    
    if (weighted) mean((1/sigma^2) %*% samples)/sum(1/sigma^2) else mean(samples)

}

I = function(sigma, theta, alpha, eta) {
  k = length(alpha)
  cutoffs = qnorm(1 - alpha)*sigma
  cdfs = pnorm(cutoffs, theta, sigma)
  sum(sapply(1:(k - 1), function(i) eta[i]*(cdfs[i] - cdfs[i + 1])))
}

J = function(sigma, tau, theta0, alpha, eta) {
  k = length(alpha)
  cutoffs = qnorm(1 - alpha)*sigma
  cdfs = pnorm(cutoffs, theta0, sqrt(tau^2 + sigma^2))
  sum(sapply(1:(k - 1), function(i) eta[i]*(cdfs[i] - cdfs[i + 1])))
}


dcprior = Vectorize(function(x, sigma, theta0, tau, alpha, eta) {
  I(sigma, x, alpha, eta)*dnorm(x, theta0, tau)/
    J(sigma, tau, theta0, alpha, eta)
}, vectorize.args = c("x", "sigma", "theta0", "tau"))


ecprior = Vectorize(function(sigma, theta0, tau, alpha, eta) {
  integrand = function(theta)
    theta*I(sigma, theta, alpha, eta)*dnorm(theta, theta0, tau)
  numerator = integrate(integrand, lower = -Inf, upper = Inf)$value
  numerator/J(sigma, tau, theta0, alpha, eta)
}, vectorize.args = c("sigma", "theta0", "tau"))



K = function(sigma, tau, theta0, alpha, eta) {
  k = length(alpha)                                                             
  cutoffs = qnorm(1 - alpha)*sigma
  pdfs = dnorm(cutoffs, theta0, sqrt(tau^2 + sigma^2))
  sum(sapply(1:(k - 1), function(i) eta[i]*(pdfs[i + 1] - pdfs[i])))
}

expectation = function(sigma, tau, theta0, alpha, eta) {
  Vectorize(function(sigma, tau, theta0) {
    var = tau^2 + sigma^2
    modulator = K(sigma, tau, theta0, alpha, eta)/J(sigma, tau, theta0, alpha, eta)
    theta0 + var*modulator
  })(sigma, tau, theta0)
}

Isq = function(object, data) {
  alpha = data$alphas
  sigma = mean(sqrt(data$vi))
  tau0s = rstan::extract(object)$sigma0
  c(mean(sapply(1:length(sigma), function(i) mean(tau0s^2/(sigma[i]^2 + tau0s^2)))),
    mean(tau0s^2/(mean(sigma)^2 + tau0s^2)),
    mean(tau0s^2)/mean((mean(sigma)^2 + tau0s^2)))
}

normalizer = function(sigma, tau, theta0, alpha, eta) {
  k = length(alpha)
  cutoffs = qnorm(1 - alpha)
  cdfs = pnorm(cutoffs, theta0, sqrt(sigma^2 + tau^2), lower.tail = FALSE)
  summands = sapply(1:(k-1), function(i) eta[i]*(cdfs[i] - cdfs[i + 1]))
  sum(summands)
}

hellinger = function(sigma, tau, theta0, alpha, eta) {
  k = length(alpha)
  cutoffs = qnorm(1 - alpha)
  constant = normalizer(sigma, tau, theta0, alpha, eta)
  weights = eta/constant
  cdfs = pnorm(cutoffs, theta0, sqrt(sigma^2 + tau^2));
  
  summands = sapply(1:(k-1), function(i) sqrt(weights[i])*(cdfs[i] - cdfs[i + 1]))
  sqrt(1 - sum(summands))
}

l1 = function(sigma, tau, theta0, alpha, eta) {
  k = length(alpha)
  cutoffs = qnorm(1 - alpha)
  constant = normalizer(sigma, tau, theta0, alpha, eta)
  weights = eta/constant
  cdfs = pnorm(cutoffs, theta0, sqrt(sigma^2 + tau^2));
  summands = sapply(1:(k-1), function(i) abs(1 - weights[i])*(cdfs[i] - cdfs[i + 1]))
  sum(summands)
}

kl = function(sigma, tau, theta0, alpha, eta) {
  k = length(alpha)
  cutoffs = qnorm(1 - alpha)
  constant = normalizer(sigma, tau, theta0, alpha, eta)
  weights = eta/constant
  cdfs = pnorm(cutoffs, theta0, sqrt(sigma^2 + tau^2))
  logs = weights*log(weights)
  logs[weights == 0] = 0
  summands = sapply(1:(k-1), function(i) logs[i]*(cdfs[i] - cdfs[i + 1]))
  sum(summands)
}

match_parameters = function(alpha1, alpha2, eta1, eta2) {
  new_alpha = sort(union(alpha1, alpha2))
  new_eta1 = eta1[.bincode(tail(new_alpha, -1), alpha1, include.lowest = TRUE)]
  new_eta2 = eta2[.bincode(tail(new_alpha, -1), alpha2, include.lowest = TRUE)]
  list(alpha = new_alpha,
       eta1 = new_eta1,
       eta2 = new_eta2)
}

tv = function(sigma, tau, theta0, alpha1, eta1, alpha2 = NULL, eta2 = NULL) {
  if(is.null(alpha2)) {
    alpha2 = c(0, 1)
    eta2 = 1
  }
  matched_parameters = match_parameters(alpha1, alpha2, eta1, eta2)
  alpha = matched_parameters$alpha
  eta1_ = matched_parameters$eta1
  eta2_ = matched_parameters$eta2
  k = length(alpha)
  cutoffs = qnorm(1 - alpha)
  weights1 = eta1_/normalizer(sigma, tau, theta0, alpha, eta1_)
  weights2 = eta2_/normalizer(sigma, tau, theta0, alpha, eta2_)
  cdfs = pnorm(cutoffs, theta0, sqrt(sigma^2 + tau^2));
  summands = sapply(1:(k-1), function(i) abs(weights1[i] - weights2[i])*(cdfs[i] - cdfs[i + 1]))
  sum(summands)/2
}


alpha1 = c(0, 0.01, 0.4, 1)
alpha2 = c(0, 0.05, 1)
eta1 = c(1, 0.3, 0.1)
eta2 = c(1, 0.1)



alpha1 = c(0, 0.05, 1)
alpha2 = c(0, 0.05, 1)
eta1 = c(1, 0.05)
eta2 = c(1, 1)
sigma = 1/sqrt(40)
tau = .3
theta0 = 1

tv(sigma, tau, theta0, alpha1, eta1, alpha2, eta2)

plot(xx, dnorm(xx, mean = theta0, sd = sqrt(tau^2 + sigma^2)))

alpha = c(0, 0.05, 1)
eta = c(1, 0.1)
sigma = 1/sqrt(40)
eta = c(1, 0)
tau = 0.5
theta0 = 0.05
l1(sigma, tau, theta0, alpha, eta)
thetas = seq(-5, 5, by = 0.01)
plot(thetas, sapply(thetas, function(theta) l1(sigma, tau, theta, alpha, eta)),
     type = "l")
lines(thetas, sapply(thetas, function(theta) hellinger(sigma, tau, theta, alpha, eta)),
      col = "blue")
plot(thetas, sapply(thetas, function(theta) kl(sigma, tau, theta, alpha, eta)),
     col = "red")


hellinger(sigma, tau, theta0, alpha, eta)
