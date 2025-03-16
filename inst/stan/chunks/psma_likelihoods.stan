#include /chunks/densities.stan

// The normalizing constant has the same shape for both the prior and the
// likelihood.

real normal_lnorm(real theta, real tau, real sigma,
                  array[] real alpha, vector eta) {
  int k = size(alpha);
  real cutoff;
  real cdf;
  array[k - 1] real summands;

  summands[1] = eta[1];

  for(i in 2:(k - 1)) {
    cutoff = inv_Phi(1 - alpha[i])*sigma;
    cdf = normal_cdf(cutoff | theta, sqrt(tau * tau + sigma * sigma));
    summands[i] = cdf*(eta[i] - eta[i - 1]);
  }


  return(log(sum(summands)));
}

// Both the prior and likelihood make use of the same normalizing constant from
// the likelihood, which is omitted in the 'mini' functions. 'Maxi' includes the
// normalizing constant.

real psma_normal_prior_mini_lpdf(real theta, real theta0, real tau, real sigma,
                                 array[] real alpha, vector eta) {
  real y = normal_lpdf(theta | theta0, tau);
  real normalizer = normal_lnorm(theta0, tau, sigma, alpha, eta);
  return(y - normalizer);
}

real psma_normal_mini_lpdf(real x, real theta, real sigma,
                           array[] real alpha, vector eta) {
  int k = size(alpha);
  real y = normal_lpdf(x | theta, sigma);
  real u = (1 - normal_cdf(x | 0, sigma));

  for(i in 1:(k - 1)){
    if(alpha[i] < u && u <= alpha[i + 1]) {
      y += log(eta[i]);
      break;
    }
  }

  return(y);
}

real psma_normal_maxi_lpdf(real x, real theta, real sigma,
                           array[] real alpha, vector eta) {
  real y = psma_normal_mini_lpdf(x | theta, sigma, alpha, eta);
  real normalizer = normal_lnorm(theta, 0, sigma, alpha, eta);
  return(y - normalizer);
}

// This is the marginal lpdf as in Hedges' paper.

real psma_normal_marginal_lpdf(real x, real theta0, real tau, real sigma,
                               array[] real alpha, vector eta) {

  int k = size(alpha);
  real y = normal_lpdf(x | theta0, sqrt(tau * tau + sigma * sigma));
  real u = (1 - normal_cdf(x | 0, sigma));
  real normalizer = normal_lnorm(theta0, tau, sigma, alpha, eta);

  for(i in 1:(k - 1)){
    if(alpha[i] < u && u <= alpha[i + 1]) {
      y += log(eta[i]);
      break;
    }
  }

  return(y - normalizer);
}
