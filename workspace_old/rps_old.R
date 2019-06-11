n = 10000
sigma = 1
theta = 1
alpha = c(0, 0.001, 0.5, 1)
eta = c(1, 0.1, 0)
hist(rps(n, sigma, theta, alpha, eta), breaks = 100)

rps2 = function(n, sigma, theta, alpha, eta) {
  stopifnot(length(alpha) == (length(eta) + 1))

  theta = rep_len(theta, length.out = n)
  sigma = rep_len(sigma, length.out = n)

  k = length(alpha)
  cutoffs = qnorm(1 - alpha)*sigma[1]
  cdfs = pnorm(cutoffs, theta[1], sigma[1])
  powers = -diff(cdfs)*eta
  probabilities = powers/sum(powers)

  numbers = c(c(rmultinom(1, n, probabilities)))
  cumulatives = cumsum(c(0, numbers))

  samples = vector("numeric", n)

  for(i in (1:(k - 1))[numbers != 0]) {
    indices = (cumulatives[i] + 1):cumulatives[i + 1]
    lower = qnorm(1 - alpha[i + 1])
    upper = qnorm(1 - alpha[i])
    samples[indices] = truncnorm::rtruncnorm(n = numbers[i],
                                             mean = theta[indices],
                                             sd = sigma[indices],
                                             a = lower*sigma[indices],
                                             b = upper*sigma[indices])
  }

  sample(samples)
}


rps = function(n, sigma, theta, alpha, eta) {
  stopifnot(length(alpha) == (length(eta) + 1))

  theta = rep_len(theta, length.out = n)
  sigma = rep_len(sigma, length.out = n)

  k = length(alpha)
  cutoffs = qnorm(1 - alpha)
  cdfs = sapply(1:n, function(i) pnorm(cutoffs, theta[i]/sigma[i], 1))
  probabilities = eta*apply(cdfs, 2, diff)/c(eta%*%apply(cdfs, 2, diff))
  numbers = apply(probabilities, 2,
                  function(prob) sample(x = 1:(k - 1), size = 1, prob = prob))

  samples = vector("numeric", n)

  for(i in unique(numbers)) {
    indices = (numbers == i)
    lower = qnorm(1 - alpha[i + 1])
    upper = qnorm(1 - alpha[i])
    samples[indices] = truncnorm::rtruncnorm(n = sum(indices),
                                             mean = theta[indices],
                                             sd = sigma[indices],
                                             a = lower*sigma[indices],
                                             b = upper*sigma[indices])
  }


  sample(samples)
}



n = 10000
sigma = 1
theta = 0
alpha = c(0, 0.01, 0.025, 0.05, 1)
eta = c(1, 0.9, 0.5, 0.1)


x = seq(-10, 10, by = 0.01)
hist(rps(n, sigma, theta, alpha, eta), breaks = 100, freq = FALSE)
lines(x, dps(x, sigma, theta, alpha, eta), type = "l")



hist(rps(n, sigma, theta, alpha, eta), breaks = 100, freq = FALSE)
hist(rph(n, sigma, theta, alpha, eta), breaks = 100, freq = FALSE)


sigma = 1
theta = 0
alpha = c(0, 0.025, 0.05, 1)
eta = c(0.1, 0.3, 0.6)

x = seq(-1, 4, by = 0.01)
plot(x, dph(x, theta, sigma, alpha, eta), type = "l")

sigma = 1
theta = 0
alpha = c(0, 0.025, 0.05, 1)
eta = c(1, 0.6, 0.1)

x = seq(-1, 4, by = 0.01)
plot(x, dps(x, theta, sigma, alpha, eta), type = "l")




n = 10000
sigma = 1
theta = 0
alpha = c(0, 0.05, 1)
eta = c(1, 0.1)

integrate(dph, qnorm(0.95), Inf, theta = theta, sigma = sigma, alpha = alpha, eta = eta)$value
integrate(dph, -Inf, qnorm(0.95), theta = theta, sigma = sigma, alpha = alpha, eta = eta)$value
eta/sum(eta)



integrate(function(x) dph(x, theta = theta, sigma = sigma,  alpha = alpha, eta = eta),
          -Inf, Inf)

x = seq(-1, 4, by = 0.01)
hist(rps(n, sigma, theta, alpha, eta), breaks = 100, freq = FALSE)
lines(x, dps(x, sigma, theta, alpha, eta), type = "l")

plot(x, pps(x, theta, sigma, alpha, eta))

plot(x, pph(x, theta, sigma, alpha, eta))
