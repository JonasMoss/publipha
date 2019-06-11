#' The Kolmogorov-Smirnoff test and distance.
#' 
#' @param x Numerical vector.
#' @param y Numerical vecor.
#' @param test Logical; If \code{TRUE}, returns the test statistic. 
#'     If \code{FALSE}, returns the KS distance.

kolmogorov = function(x, y, test = TRUE) {
  length_x = length(x)
  length_y = length(y)
  x_and_y = c(x, y)
  z = cumsum(ifelse(order(x_and_y) <= length_x, 1/length_x, -1/length_y))
  statistic = max(abs(z[c(which(diff(sort(x_and_y)) != 0), length_x + length_y)]))
  if(test) 1 - .Call(stats:::C_pSmirnov2x, statistic, length_x, length_y)
  else statistic
}

#' Sampling from Hedges' selection model
#' 
#' @param n Integer. Sample size.
#' @param mean Numeric. Vector of means.
#' @param sd Numeric. Vector of standard deviations.
#' @param weighths Numeric. Vector of weights given to each bin.
#' @param alphas Numeric. Vector of split points.


rhselection = function(n, mean, sd, weights, alphas) {
  stopifnot(length(alphas) == (length(weights) - 1))
  
  mean = rep_len(mean, length.out = n)
  sd = rep_len(sd, length.out = n)
  endpoints = c(0, alphas, 1)
  probabilities = weights/max(weights)
  accepted = 0
  samples = vector("numeric", n)
  
  while(accepted < n) {
    
    proposal = rnorm(n = 1, 
                     mean = mean[accepted + 1],
                     sd = sd[accepted + 1])
    
    position = .bincode(pnorm(-proposal/sd[accepted + 1]), endpoints)
    
    if(runif(1) < probabilities[position]) {
      samples[accepted + 1] = proposal
      accepted = accepted + 1
    }
  }
  
  samples
  
}


recycle = function(n, ...) {
  envir = parent.frame()
  dots  = list(...)
  names = names(dots)
  recycled = lapply(dots, rep, length.out = n)
  for(name in names) envir[[name]] = recycled[[name]]
}

rmixtnorm = function(n, mean = 0, sd = 1, pi = 1, lower = qnorm(0.975),
                     upper = Inf) {
  
  recycle(n, mean_  = mean, 
             sd_    = sd, 
             pi_    = pi, 
             lower_ = lower, 
             upper_ = upper)
  
  z = as.logical(rbinom(n, 1, pi_))
  n_trunc = sum(z)
  n_norm = n - n_trunc
  result = rep(NA, n)
  result[z] = truncnorm::rtruncnorm(n_trunc,
                                    mean = mean_[z],
                                    sd = sd_[z],
                                    a = lower_[z],
                                    b = upper_[z])
  result[!z] = stats::rnorm(n_norm, mean_[z], sd_[z])
  result
}


#' The Publi Distribution
#'
#' Density, distribution, quantile and random generation for the Publi
#'     distribtion.
#'     
#' The Publi distribution is the discrete variant of the weighting function 
#'     selection model. Only \code{rpubli} is implemented right now. The 
#'     interface is subject to change.
#'     
#' @param x,q Numeric vector of quantiles.
#' @param p Numeric vector of probabilities.
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken
#'     to be the number required.
#' @param spec The specification of the likelihood, such as \code{norm} or 
#'     \code{exp}. 
#' @param parameters A named vector of parameters or a function of one integer
#'     argument generating a named vector of parameters. The names should match
#'     the argument list of \code{density}. See the details.
#' @param pvalue A function of \code{x}. Turns \code{x} into a p-value which is
#'     used by the \code{alpha_pi} and \code{alpha_rho} vectors below. 
#' @param alpha Numeric vector; Specifies the thresholds for publication
#'     bias.
#' @param eta Numeric vecto; Containing the probabilites of being a study
#'     with the given p-value from being published. This is normalized so that 
#'     the maximumal element is 1.
#' @param log,log.p Logical; If \code{TRUE}, probabilities are given as 
#'     \code{log(p)}.
#' @param lower.tail Logical; If \code{TRUE}, the lower tail is returned.

rpubli = function(n, 
                  spec, 
                  parameters, 
                  pvalue, 
                  alpha = c(0, 1),
                  eta = 1) {
  
  spec_subst = deparse(substitute(spec))
  spec = if(is.character(spec)) spec else spec_subst
  
  stopifnot(length(eta) + 1 == length(alpha))
  I = length(eta)
  eta = eta/max(eta)
  
  output = rep(NA, times = n)
  rmodel = get(paste0("r", spec), mode = "function")
  tries  = 0
  parameter_tries = vector(mode = "list", length = n)
  
  for(i in 1:n) {
    
    while(TRUE) {
      
      args = parameters(i)
      parameter_arg = args
      args$n = 1
      
      tries = tries + 1

      proposal = do.call(what = rmodel, args = args) 
      pvalue_proposal = pvalue(proposal)
      index = .bincode(x = pvalue_proposal, 
                       breaks = alpha, 
                       include.lowest = TRUE)
      #index = as.numeric(cut(pvalue_proposal, breaks = alpha))
      probability = eta[index]
      u = runif(1)
      
      if(u < probability) {
        output[i] = proposal
        parameter_tries[[i]] = parameter_arg 
        break
      } 
      
    }   
  }
  
  attr(output, "tries") = tries
  attr(output, "parameters") = parameter_tries
  output
  
}
