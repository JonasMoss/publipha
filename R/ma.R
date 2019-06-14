#' Class \code{mafit}: Fitted Meta-analysis model.
#'
#' @slot bias The kind of bias modelled. Can be one of
#'     \code{publication_selection}, \code{p-hacking} or \code{none}.
#' @slot effect A string saying if random or fixed effects have been used.
#' @slot alpha Ordered numeric vector of cuttoffs including 0 and 1.
#' @slot yi Numeric vector of eDstimated effect sizes.
#' @slot vi Numeric vector of study-specific variances.
#' @slot parameters The list of prior parameters used in the fitting.
#' @name mafit
#' @rdname mafit
#' @exportClass mafit

setClass(Class = "mafit",
         contains = "stanfit",
         representation = representation(bias = "character",
                   effect = "character",
                   alpha = "numeric",
                   yi = "numeric",
                   vi = "numeric",
                   parameters = "list"))

#' Meta-analysis Correcting for Publication Bias or p-hacking
#'
#' Do a Bayesian meta-analysis. Correct for publication selection or p-hacking,
#'     or run an ordinary meta-analysis without correction.
#'
#' @export
#' @param yi Numeric vector of length code{k} with observed effect size
#'     estimates.
#' @param vi Numeric vector of length code{k} with sampling variances.
#' @param bias String; If "publication bias", corrects for publication bias. If
#'     "p-hacking", corrects for p-hacking.
#' @param likelihood String; Either a vector of length code{k} or a string
#'     giving the likelihood for each observation. Valid choices are "normal"
#'     and "fnormal".
#' @param data Optional list or data frame containing \code{yi}, \code{vi} and
#'     \code{likelihood}.
#' @param effects The type of meta-analysis model to use. Valid choices are
#'     "random" and "fixed". Currently only random effects are supported.
#' @param alpha Numeric vector; Specifies the cuttoffs for significance.
#'     Should include 0 and 1. Defaults to (0, 0.025, 0.05, 1).
#' @param prior Optional list of prior parameters. See the details.
#' @param classical Logical; If \code{TRUE}, runs a classical meta-analysis. If
#'     \code{FALSE}, runs a Hedges meta-analysis. Currently not supported.
#' @param ... Passed to \code{rstan::sampling}.
#' @return An S4 object of class \code{mafit}.

ma = function(yi,
              vi,
              bias = c("publication selection", "p-hacking", "none"),
              likelihood = NULL,
              data,
              effects = c("random", "fixed"),
              alpha = c(0, 0.025, 0.05, 1),
              prior = NULL,
              classical = FALSE, ...) {

  dots = list(...)
  alpha = sort(alpha)
  bias = match.arg(bias, c("publication selection", "p-hacking", "none"))
  effects = match.arg(effects, c("random", "fixed"))

  if(is.null(likelihood)) {
    likelihood = "normal"
  } else {
    for(i in 1:length(likelihood)) {
      likelihood[i] = match.arg(likelihood, c("normal", "formal"))
    }
  }

  ## Finds `yi` and `vi` in `data` if it is supplied.
  if(!missing(data)) {
    yi_name = deparse(substitute(yi))
    vi_name = deparse(substitute(vi))
    likelihood_name = deparse(substitute(likelihood))
    if(!is.null(data[[yi_name]])) yi = data[[yi_name]]
    if(!is.null(data[[vi_name]])) vi = data[[vi_name]]
    if(!is.null(data[[likelihood_name]])) yi = data[[likelihood_name]]
  }

  ## Populate unspecified priors with the default values.
  if(is.null(prior$eta0)) prior$eta0 = rep(1, length(alpha) - 1)
  if(is.null(prior$theta0_mean)) prior$theta0_mean = 0
  if(is.null(prior$theta0_sd)) prior$theta0_sd = 1
  if(is.null(prior$tau_mean)) prior$tau_mean = 0
  if(is.null(prior$tau_sd)) prior$tau_sd = 1

  ## `parameters` in ultimately passed to stan.
  parameters = prior
  parameters$alpha = alpha

  ## Changes stan default parameters to something conservative.
  if(is.null(dots$control$max_treedepth)) dots$control$max_treedepth = 15
  if(is.null(dots$control$adapt_delta)) dots$control$adapt_delta = 0.99

  sizes = list(N = length(yi),
               k = length(alpha))

  index = ifelse(likelihood == "normal", 0, 1)

  input_data = c(list(yi = yi,
                      vi = vi,
                      likelihood = rep_len(index, length.out = sizes$N)),
                 sizes,
                 parameters)

  if(bias == "publication selection") {

    eta_start = 1 + (1:(length(alpha) - 1))/10
    theta_start = rep(0, length(yi))

    if(is.null(dots$init)) dots$init = function() list(theta0 = 0,
                                                       tau = 1,
                                                       theta = theta_start,
                                                       eta = eta_start)
    model = stanmodels$psma
  } else if (bias == "p-hacking") {

    eta_start = rep(1, length(alpha) - 1)/(length(alpha) - 1)
    theta_start = rep(0, length(yi))

    if(is.null(dots$init)) dots$init = function() list(theta0 = 0,
                                                  tau = 1,
                                                  theta = theta_start,
                                                  eta = eta_start)

    model = stanmodels$phma
  } else {
    model = stanmodels$cma
  }

  obj = as(object = do_call(rstan::sampling, c(list(object = model,
                                        data = input_data),
                                   dots)),
           Class = "mafit")

  parameters$alpha = NULL
  obj@yi = yi
  obj@vi = vi
  obj@parameters = parameters
  obj@alpha = alpha
  obj@bias = bias
  obj@effect = effects
  obj

}


#' Meta-analysis with Publication Selection
#'
#' @export
#' @param yi Numeric vector of length code{k} with observed effect size
#'     estimates.
#' @param vi Numeric vector of length code{k} with sampling variances.
#' @param likelihood String; Either a vector of length code{k} or a string
#'     giving the likelihood for each observation. Valid choices are "normal"
#'     and "fnormal".
#' @param data Optional list or data frame containing \code{yi}, \code{vi} and
#'     \code{likelihood}.
#' @param effects The type of meta-analysis model to use. Valid choices are
#'     "random" and "fixed". Currently only random effects are supported.
#' @param alpha Numeric vector; Specifies the cuttoffs for significance.
#'     Should include 0 and 1. Defaults to (0, 0.025, 0.05, 1).
#' @param prior Optional list of prior parameters. See the details.
#' @param classical Logical; If \code{TRUE}, runs a classical meta-analysis. If
#'     \code{FALSE}, runs a Hedges meta-analysis. Currently not supported.
#' @param ... Passed to \code{rstan::sampling}.

psma = function(yi,
                vi,
                likelihood = c("normal", "fnormal"),
                data,
                effects = c("random", "fixed"),
                alpha = c(0, 0.025, 0.05, 1),
                prior = NULL,
                classical = FALSE, ...) {
  args = arguments(expand_dots = TRUE)
  do_call(ma, c(args, bias = "publication selection"))
}

#' Meta-analysis with p-hacking
#'
#' Do a meta-analysis that corrects for p-hacking. This is a wrapper for
#'     \code{ma}.
#'
#' @export
#' @param yi Numeric vector of length code{k} with observed effect size
#'     estimates.
#' @param vi Numeric vector of length code{k} with sampling variances.
#' @param likelihood String; Either a vector of length code{k} or a string
#'     giving the likelihood for each observation. Valid choices are "normal"
#'     and "fnormal".
#' @param data Optional list or data frame containing \code{yi}, \code{vi} and
#'     \code{likelihood}.
#' @param effects The type of meta-analysis model to use. Valid choices are
#'     "random" and "fixed". Currently only random effects are supported.
#' @param alpha Numeric vector; Specifies the cuttoffs for significance.
#'     Should include 0 and 1. Defaults to (0, 0.025, 0.05, 1).
#' @param prior Optional list of prior parameters. See the details.
#' @param classical Logical; If \code{TRUE}, runs a classical meta-analysis. If
#'     \code{FALSE}, runs a Hedges meta-analysis. Currently not supported.
#' @param ... Passed to \code{rstan::sampling}.

phma = function(yi,
               vi,
               likelihood = c("normal", "fnormal"),
               data,
               effects = c("random", "fixed"),
               alpha = c(0, 0.025, 0.05, 1),
               prior = NULL,
               classical = FALSE, ...) {
  args = arguments(expand_dots = TRUE)
  do_call(ma, c(args, bias = "p-hacking"))
}

#' Classical meta-analysis
#'
#' Do a classical Bayesian meta-analysis. This is a wrapper for
#'     \code{ma}.
#'
#' @export
#' @param yi Numeric vector of length code{k} with observed effect size
#'     estimates.
#' @param vi Numeric vector of length code{k} with sampling variances.
#' @param likelihood String; Either a vector of length code{k} or a string
#'     giving the likelihood for each observation. Valid choices are "normal"
#'     and "fnormal".
#' @param data Optional list or data frame containing \code{yi}, \code{vi} and
#'     \code{likelihood}.
#' @param effects The type of meta-analysis model to use. Valid choices are
#'     "random" and "fixed". Currently only random effects are supported.
#' @param prior Optional list of prior parameters. See the details.
#' @param classical Logical; If \code{TRUE}, runs a classical meta-analysis. If
#'     \code{FALSE}, runs a Hedges meta-analysis. Currently not supported.
#' @param ... Passed to \code{rstan::sampling}.

cma = function(yi,
                vi,
                likelihood = c("normal", "fnormal"),
                data,
                effects = c("random", "fixed"),
                alpha = c(0, 0.025, 0.05, 1),
                prior = NULL,
                classical = FALSE, ...) {
  args = arguments(expand_dots = TRUE)
  do_call(ma, c(args, bias = "none"))
}


#' Run a meta-analysis with all corrections.
#'
#' Do a publication bias-correct, a p-hacking-correct and a classical
#'    meta-analysis.
#'
#' @export
#' @param yi Numeric vector of length code{k} with observed effect size
#'     estimates.
#' @param vi Numeric vector of length code{k} with sampling variances.
#' @param likelihood String; Either a vector of length code{k} or a string
#'     giving the likelihood for each observation. Valid choices are "normal"
#'     and "fnormal".
#' @param data Optional list or data frame containing \code{yi}, \code{vi} and
#'     \code{likelihood}.
#' @param effects The type of meta-analysis model to use. Valid choices are
#'     "random" and "fixed". Currently only random effects are supported.
#' @param prior Optional list of prior parameters. See the details.
#' @param classical Logical; If \code{TRUE}, runs a classical meta-analysis. If
#'     \code{FALSE}, runs a Hedges meta-analysis. Currently not supported.
#' @param ... Passed to \code{rstan::sampling}.

allma = function(yi, vi,data, alpha = c(0, 0.025, 0.05, 1), ...) {
  args = arguments(expand_dots = TRUE)
  list(phma = do_call(ma, c(args, bias = "p-hacking")),
       psma = do_call(ma, c(args, bias = "publication selection")),
       cma  = do_call(ma, c(args, bias = "none")))
}
