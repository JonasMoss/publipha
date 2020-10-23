functions {
#include /chunks/psma_likelihoods.stan
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
  real <lower = 0> u_min;
  real <lower = 0> u_max;
  real <lower = 0> shape;
  real <lower = 0> scale;
  int tau_prior;

}

parameters {

  real theta0;
  real <lower = 0> tau;
  positive_ordered[k - 1] weights;
  real theta[N];

}

transformed parameters {
  vector[k - 1] eta;
  for(i in 1:(k - 1)) eta[i] = weights[k - i]/weights[k - 1];
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

  weights ~ gamma(eta0, 1);

  for(n in 1:N) {
    theta[n] ~ psma_normal_prior_mini_lpdf(theta0, tau, sqrt(vi[n]), alpha, eta);
    yi[n] ~ psma_normal_mini_lpdf(theta[n], sqrt(vi[n]), alpha, eta);
  }

}

generated quantities {

  vector[N] log_lik_marginal;
  vector[N] log_lik;

  for(n in 1:N)
    log_lik[n] = psma_normal_maxi_lpdf(yi[n] | theta[n], sqrt(vi[n]), alpha, eta);

  for(n in 1:N)
    log_lik_marginal[n] = psma_normal_marginal_lpdf(yi[n] | theta0, tau, sqrt(vi[n]), alpha, eta);

}
