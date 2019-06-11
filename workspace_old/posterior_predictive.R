#' Posterior predictive distribution of classical meta-analysis means
#' 
#' @param n Number of observations. If \code{length(n) > 1}, the length is 
#'     taken to be the number required.
#' @param object A \code{publipha} object.
#' @return A numeric vector containing expected mean effects as calculated under
#'     no publication bias.

rpremean = function(n, object) {
  
  if(length(n) > 1) n = length(n)
  
  vi = attr(object, "data")$vi
  theta0_mean = attr(object, "parameters")$theta0_mean
  theta0_sd = attr(object, "parameters")$theta0_sd
  alpha = attr(object, "parameters")$alphas
  
  theta0_all = rstan::extract(object)$theta0
  tau_all = rstan::extract(object)$tau
  weights_all = rstan::extract(object)$weights
  
  indices = sample.int(n = length(theta0_all), 
                       size = n, 
                       replace = TRUE)
  
  theta0 = theta0_all[indices]
  tau = tau_all[indices]
  weights = weights_all[indices, ]

  new_sigma = function(tau) sqrt(1/(theta0_sd + sum(1/(vi + tau^2))))
  
  n = length(vi)
  spec = "norm"
  pvalue = function(x) pnorm(-x)
  
  new_mean = function(i) {
    #yi = expectation(sqrt(vi), tau[i], theta0[i], alpha, weights[i, ])
    parameters = if(tau[i] > 0) {
      function(j) list(mean = rnorm(1, mean = theta0[i], sd = tau[i])/sqrt(vi[j]))
    } else {
      function(j) list(mean = theta0[i]/sqrt(vi[j]))
    }
    
    yi = rpubli(n = n, 
                spec = spec, 
                parameters = parameters, 
                pvalue = pvalue,
                alpha = alpha, 
                eta = weights[i, ])*sqrt(vi)
    
    new_sigma(tau[i])^2*(theta0_mean/theta0_sd^2 + sum(yi/(vi + tau[i]^2)))
  }

  sapply(1:length(theta0), new_mean)
}

rsd = function(n, object) {
  if(length(n) > 1) n = length(n)
  
  vi = attr(object, "data")$vi
  theta0_sd = attr(object, "parameters")$theta0_sd
  
  tau_all = rstan::extract(object)$tau
  indices = sample.int(n = length(tau_all), size = n, replace = TRUE)
  
  tau = sample(x = tau_all, 
               size = n, 
               replace = TRUE)
  
  new_sigma = function(tau) sqrt(1/(theta0_sd + sum(1/(vi + tau^2))))

  sapply(tau, new_sigma)
}


vi = cuddy2018$vi
alpha = c(0, 0.05, 1)
eta = c(0.97, 0.03)
theta0 = 0
tau = 0.2

new_data = simulate_studies(vi, alpha, eta, theta0, tau)

plot(1/new_data$vi, new_data$yi)
points(1/cuddy2018$vi, cuddy2018$yi, col = "red")


hedges_re

sigmas = sqrt(cuddy2018$vi)
mean(sapply(sigmas, function(sigma) 
  tv(sigma, tau = 0.22, theta0 = 1, c(0, 0.05, 1), c(0.96, 0.04))))


mean(sapply(sigmas, function(sigma) 
  expectation(sigma, tau = 0.22, theta0 = 0.05, c(0, 0.05, 1), c(0.96, 0.04))))

