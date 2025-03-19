#include /chunks/densities.stan

real phma_normal_lpdf(real x, real theta, real sigma, array[] real alpha, vector eta) {
  int k = size(alpha);
  array[k - 1] real y;
  real u = (1 - normal_cdf(x | 0, sigma));
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
