functions {
#include /chunks/densities.stan
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
  real <lower = 0> tau;
  array[N] real theta;

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

  theta ~ normal(theta0, tau);

  for(n in 1:N) yi[n] ~ normal(theta[n], sqrt(vi[n]));

}

generated quantities {

  vector[N] log_lik_marginal;
  vector[N] log_lik;

  for(n in 1:N)
    log_lik[n] = normal_lpdf(yi[n] | theta[n], sqrt(vi[n]));

  for(n in 1:N)
    log_lik_marginal[n] = normal_lpdf(yi[n] | theta0, sqrt(vi[n] + tau * tau));

}
