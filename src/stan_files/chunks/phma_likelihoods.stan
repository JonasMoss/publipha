#include /chunks/densities.stan

real phma_normal_lpdf(real x, real theta, real sigma, real [] alpha, vector eta) {
  int k = size(alpha);
  real y[k - 1];
  real u = (1 - normal_cdf(x, 0, sigma));
  real cutoff;

  for(i in 1:(k - 2)){
    if(alpha[i + 1] < u) {
      y[i] = negative_infinity();
    } else {
      cutoff = inv_Phi(1 - alpha[i + 1]);
      y[i] = log(eta[i]) - normal_lccdf(cutoff | theta/sigma, 1);
    }
  }

  y[k - 1] = log(eta[k - 1]);
  return(log_sum_exp(y) + normal_lpdf(x | theta, sigma));
}

// Not correct.
real phma_fnormal_lpdf(real x, real theta, real sigma, real [] alpha, vector eta) {
  int k = size(alpha);
  real y = fnormal_lpdf(x | theta, sigma);
  real u = (1 - normal_cdf(x, 0, sigma))*2;
  real cutoff;

  for(i in 2:(k - 1)){
    if(alpha[i] < u) break;
    cutoff = inv_Phi(1 - alpha[i]/2)*sigma;
    y += log(eta[i]) - fnormal_lccdf(cutoff | theta, sigma);
  }

  return(y);
}

//
// real phma_normal_marginal_lpdf(real x, real theta0, real tau, real theta,
//                                real sigma, real [] alpha, vector eta) {
//   phma_normal_lpdf(yi[n] | theta[n], sqrt(vi[n]), alpha, eta) +
//   normal_lpdf(theta[n], )
// }
