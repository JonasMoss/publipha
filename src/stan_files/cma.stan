functions {
#include /chunks/densities.stan
}

data {

  // Input data.
  int<lower = 0> N;   // Number of observations.
  int<lower = 0> k;   // Length of alpha.
  real alpha[k];      // The vector of cuttoffs.
  real yi[N];         // The estimated effect sizes.
  real vi[N];         // The study-specific variances.

  // Prior parameters.
  vector[k - 1] eta0;
  real theta0_mean;
  real <lower = 0> theta0_sd;
  real tau_mean;
  real <lower = 0> tau_sd;

}

parameters {
  real theta0;
  real <lower = 0> tau;
  real theta[N];

}

model {

  theta0 ~ normal(theta0_mean, theta0_sd);
  tau ~  normal(tau_mean, tau_sd) T[0, ];
  theta ~ normal(theta0, tau);

  for(n in 1:N) yi[n] ~ normal_lpdf(theta[n], sqrt(vi[n]));

}

generated quantities {

  vector[N] log_lik_marginal;
  vector[N] log_lik;

  for(n in 1:N)
    log_lik[n] = normal_lpdf(yi[n] | theta[n], sqrt(vi[n]));

  for(n in 1:N)
    log_lik_marginal[n] = normal_lpdf(yi[n] | theta0, sqrt(vi[n] + tau * tau));

}
