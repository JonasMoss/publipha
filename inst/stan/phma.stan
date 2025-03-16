functions {
#include /chunks/phma_likelihoods.stan
}

data {

  // Input data.
  int<lower = 0> N;   // Number of observations.
  int<lower = 0> k;   // Length of alpha.
  array[k] real alpha;      // The vector of cuttoffs.
  array[N] real yi;         // The estimated effect sizes.
  array[N] real vi;         // The study-specific variances.

  // Prior parameters.
  vector[k - 1] eta0;
  real theta0_mean;
  real <lower = 0> theta0_sd;
  real tau_mean;
  real <lower = 0> tau_sd;
  real <lower = 0> u_min;
  real <lower = 0> u_max;
  real <lower = 0> shape;
  real <lower = 0> scale;
  int tau_prior;


}

parameters {
  real theta0;
  array[N] real theta;
  real <lower = 0> tau;
  simplex[k - 1] eta;

}

model {

  theta0 ~ normal(theta0_mean, theta0_sd);

  if(tau_prior == 1) {
    tau ~  normal(tau_mean, tau_sd) T[0, ];
  } else if (tau_prior == 2) {
    tau ~ uniform(u_min, u_max);
  } else if (tau_prior == 3) {
    tau ~ inv_gamma(shape, scale);
  }

  eta ~ dirichlet(eta0);
  theta ~ normal(theta0, tau);

  for(n in 1:N) yi[n] ~ phma_normal(theta[n], sqrt(vi[n]), alpha, eta);

}

generated quantities {

  vector[N] log_lik;

  for(n in 1:N)
    log_lik[n] = phma_normal_lpdf(yi[n] | theta[n], sqrt(vi[n]), alpha, eta);

}
