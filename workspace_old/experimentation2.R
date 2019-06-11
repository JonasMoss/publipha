set.seed(1313)
n = 60
theta0 = 0.17
tau = 0.5
sigma = 1/sqrt(sample(x = 20:200, size = n, replace = TRUE))
alpha = c(0, 0.05, 1)
eta = c(0.9, 0.1)

yi = rmaps(n, theta0, tau, sigma, alpha, eta)
yi = rph(n, rnorm(n, theta0, tau), sigma, alpha, eta)
#yi = rph(n, theta0, sigma, alpha, eta)
#yi = rps(n, theta0, sigma, alpha, eta)
vi = rep(sigma, length.out = n)^2

object_psma = psma(yi, vi, chains = 1)
object_phma = phma(yi, vi, chains = 1)
object_cma  = cma(yi, vi, chains = 1)

loo(object_psma)
loo(object_phma)
loo(object_cma)

extract_eta(object_psma)



loo(llfun, draws = rstan::extract(object_phma), data = cbind(yi, vi))

data_i = c(object_phma@yi[1], object_phma@vi[1])
draws = rstan::extract(object_phma)

llfun = function(data_i, draws) {

  N = length(draws$theta0)
  yi_i = data_i[1]
  vi_i = data_i[2]
  result_i = vector("numeric", N)

  for(n in 1:N) {
    eta = draws$eta[n, ]
    theta0 = draws$theta0[n]
    tau = draws$tau[n]

    f = function(theta) dph(yi_i/sqrt(vi_i), theta, 1, alpha, eta)*
      dnorm(theta, theta0/sqrt(vi_i), tau/sqrt(vi_i))

    result_i[n] = log(integrate(f = f, lower = -Inf, upper = Inf)$value) -
      log(sqrt(vi_i))
  }

  result_i

}







# extract_theta0(object, hist)
# abline(v = extract_theta0(object, mean))
# extract_theta0(object, mean)
# extract_eta(object, function(x) quantile(x, c(0.05, 0.95)))
# forest(object)


g = Vectorize(function(yi_i) {
  f = function(theta) dph(yi_i/sqrt(vi_i), theta, 1, alpha, eta)*
      dnorm(theta, theta0/sqrt(vi_i), tau/sqrt(vi_i))

  integrate(f = f, lower = -Inf, upper = Inf)$value/sqrt(vi_i)
})
