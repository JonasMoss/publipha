setGeneric("predict")

predict_ = function(object,
                    newdata = NULL,
                    type = c("yi", "effect"),
                    observed = FALSE,
                    fun,
                    Nreps = 1000,
                    ...) {

  type = match.arg(type, c("yi", "effect"))

  if(missing(vi)) vi = object@vi
  if(is.function(vi)) vi_ = vi
  if(!is.function(vi)) vi_ = function() vi

  n = length(vi_())
  mat = matrix(data = NA, nrow = Nreps, ncol = n)
  alpha = object@alpha
  theta0s = extract_theta0(object, identity)
  taus = extract_tau(object, identity)
  etas = extract_eta(object, identity)

  indices = sample.int(n = length(theta0s), size = Nreps, replace = TRUE)

  if(type == "effect") {
    if(observed == FALSE) {
      for(i in 1:Nreps)
        mat[i, ] = rnorm(n, theta0s[indices[i]], taus[indices[i]])
    } else if (observed == TRUE) {
      for(i in 1:Nreps)
        mat[i, ] = resprior(n, sqrt(vi_()), theta0s[indices[i]], taus[indices[i]], alpha, eta)
    }
  } else if (type == "yi"){
    if(observed == FALSE) {
      for(i in 1:Nreps)
        mat[i, ] = rnorm(n, theta0s[indices[i]], sqrt(vi_() + taus[indices[i]]^2))
    } else if (observed == TRUE) {
      for(i in 1:Nreps)
        mat[i, ] = rmmodel(n, sqrt(vi_()), theta0s[indices[i]], taus[indices[i]], alpha, eta)
    }
  }

  if(!missing(fun)) apply(mat, 2, fun) else mat

}

#' Predict method for Publication Selection Meta-analysis Fits
#'
#' @export
#' @docType methods
#' @param object Oject of class inheriting from \code{mafit}.
#' @param newdata Either a vector of variances or a function of no arguments
#'     generating a fixed length vector of variances.
#' @param type The type of predicton. This could be the predicted \code{yi} or
#'     the predicted effect.
#' @param observed Logical; If \code{TRUE}, the prediction is about the post-
#'     selected effect or \code{yi}. If \code{FALSE}, the prediction is about
#'     the pre-selected effect or \code{yi}
#' @param fun Optional function to apply on each column of the sample matrix.
#' @param Nreps Number of samples to draw from the posterior predictive
#'     distribution.
#' @return Matrix of samples from the posterior predictive distribution. The ith
#'     column contains samples from the ith vi.

setMethod("predict", "mafit", predict_);

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

K = function(sigma, tau, theta0, alpha, eta) {
  k = length(alpha)
  cutoffs = qnorm(1 - alpha)*sigma
  pdfs = dnorm(cutoffs, theta0, sqrt(tau^2 + sigma^2))
  sum(sapply(1:(k - 1), function(i) eta[i]*(pdfs[i + 1] - pdfs[i])))
}

expectation = function(sigma, tau, theta0, alpha, eta) {
  Vectorize(function(sigma, tau, theta0) {
    var = tau^2 + sigma^2
    modulator = K(sigma, tau, theta0, alpha, eta)/J(sigma, theta0, tau, alpha, eta)
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
