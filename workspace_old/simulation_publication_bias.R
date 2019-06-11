### ============================================================================
### baskerville
### ============================================================================

get(data("baskerville", package = "straussR"))
data = list(yi = baskerville$yi,
            vi = baskerville$vi)
rm(baskerville)

alphas = c(0, 0.05, 0.1, 1)

parameters = list(alphas = alphas,
                  pis = rep(1, length(alphas) - 1),
                  theta0_mean = 0,
                  theta0_sd = 1,
                  tau_mean = 0,
                  tau_sd = 1)

baskerville_hre = hedges(data,
                      effects = "random",
                      parameters = parameters,
                      classical = "FALSE",
                      chains = 4)

baskerville_cre = hedges(data,
                      effects = "random",
                      parameters = parameters,
                      classical = "TRUE",
                      chains = 4)

rm(data)
rm(parameters)

theta0(baskerville_hre)/theta0(baskerville_cre)
mean(rsdratio(1000, baskerville_hre))
hedges_loo(baskerville_hre)

### ============================================================================
### cuddy2018
### ============================================================================

cuddy2018 = readRDS("data/cuddy2018.rds")
data = list(yi = cuddy2018$yi,
            vi = cuddy2018$vi)
yi = data$yi
rm(cuddy2018)

alpha = c(0, 0.05, 1)

parameters = list(alpha = alpha,
                  weights_lambda = rep(1, length(alpha) - 1),
                  theta0_mean = 0,
                  theta0_sd = 1,
                  tau_mean = 0,
                  tau_sd = 1)

cuddy2018_psma = psma(yi, vi, likelihood = "normal", data = data,
                      effects = "random", chains = 1)

init <- function() {
  list(theta0 = 0,
       tau = 1,
       theta = rep(0, length(yi)),
       eta = rep(1, length(alpha) - 1)/(length(alpha) - 1))
}

cuddy2018_phma = phma(yi, vi, likelihood = "normal", data = data,
                    effects = "random", chains = 1, init = init)


ma_loo(cuddy2018_psma)
ma_loo(cuddy2018_phma)

### ============================================================================
### dat.bangertdrowns2004
### ============================================================================



hedges_expectation_x(hedges_re, data)
hedges_loo(hedges_re)
hedges_loo(classical_re)

data$vi = dat.bangertdrowns2004$vi
data$alphas = alpha
data$k = length(alpha)
data$N = length(data$vi)
data$pis = rep(1, length(alpha) - 1)
data$yi = dat.bangertdrowns2004$yi

hedges_re    = hedges(data, effects = "random", classical = "FALSE", chains = 4)
hedges_fe    = hedges(data, effects = "fixed", classical = "FALSE", chains = 4)
classical_re = hedges(data, effects = "random", classical = "TRUE", chains = 4)


hedges_loo(hedges_re)
hedges_loo(hedges_fe)
hedges_loo(classical_re)
hedges_expectation_x(hedges_re, data)
hedges_expectation_x(hedges_fe, data)


data = list(yi = cuddy2018$yi,
            vi = cuddy2018$vi)

parameters = list(alphas = alpha,
                  pis = rep(1, length(alpha) - 1),
                  theta0_mean = 0,
                  theta0_sd = 1,
                  tau_mean = 0,
                  tau_sd = 1)

hedges_re    = hedges(data,
                      effects = "random",
                      parameters = parameters,
                      classical = "FALSE",
                      chains = 4)

hedges_fe    = hedges(data, effects = "fixed", classical = "FALSE", chains = 4)
classical_re = hedges(data, effects = "random", classical = "TRUE", chains = 4)
classical_fe = hedges(data, effects = "fixed", classical = "TRUE", chains = 4)


vi = cuddy2018$vi
tau = 0.22
sigma_theta = 1
sigma = 0.12
sqrt(1/(sigma_theta + sum(1/(vi + tau^2))))

0.12/sqrt(1/(sigma_theta + sum(1/(vi + tau^2))))




sigma = sd(rstan::extract(hedges_re)$theta0)
tau = rstan::extract(hedges_re)$sigma
sd_ratio(sigma, rstan::extract(hedges_re)$sigma, sigma_theta, vi)



hedges_loo(hedges_re)
hedges_loo(hedges_fe)
hedges_loo(classical_re)
hedges_expectation_x(hedges_re, data)
hedges_expectation_x(hedges_fe, data)

cuddy2018_not_phacked$type = 20
# Mixed model without p-hacking.
set.seed(seed)
straussR(formula = z ~ normal(mean ~ 1, sd ~ 1),
         data = cuddy2018_not_phacked,
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ exponential(1))),
         chains = chains,
         control = list(adapt_delta = 0.9999,
                        max_treedepth = 15)) ->
  cuddy2018_not_phacked_mixed

# Mixed model with p-hacking.
cuddy2018$type = 5
set.seed(seed)
straussR(formula = z ~ normal(mean ~ 1, sd ~ 1, p ~ 1),
         data = cuddy2018,
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ exponential(1)),
                       p    = list((Intercept) ~ beta(1, 1))),
         chains = 4,
         control = list(adapt_delta = 0.9999,
                        max_treedepth = 15)) ->
  cuddy2018_phacked_mixed

bias_rate(hedges_re, cuddy2018)

# Mixed model with p-hacking.
cuddy2018$type = 5
cuddy2018$lower = qnorm(0.95)
set.seed(seed)
straussR(formula = z ~ normal(mean ~ 1, sd ~ 1, p ~ 1),
         data = cuddy2018,
         priors = list(mean = list((Intercept) ~ normal(0, 1)),
                       sd   = list((Intercept) ~ exponential(1)),
                       p    = list((Intercept) ~ beta(1, 1))),
         chains = 4,
         control = list(adapt_delta = 0.9999,
                        max_treedepth = 15)) ->
  cuddy2018_phacked_mixed
