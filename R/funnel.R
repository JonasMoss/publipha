#' Funnel Plot with Significance Lines
#'
#' Create a funnel plot with lines for the significance thresholds.
#'
#' @name funnel
#' @export
#' @param object The \code{mafit} object to plot.
#' @param ... Passed to \code{plot}.
#' @return \code{NULL}, the function is called for its side effects.

funnel = function(object, ...) {

  dots = list(...)
  alpha = object@alpha
  yi = object@yi
  vi = object@vi

  cutoffs = stats::qnorm(alpha, lower.tail = FALSE) ## Assumes the usual cutoffs.
  if(is.null(dots$xlim)) dots$xlim = c(1, max(1/vi) + 10)
  if(is.null(dots$ylim)) dots$ylim = c(0, max(yi))
  y = seq(dots$xlim[1], dots$xlim[2] + 10, by = (dots$xlim[2] - dots$xlim[1])/1000)

  if(is.null(dots$pch)) dots$pch = 20

  do_call(plot, c(list(x = 1/vi,
                       y = yi,
                       xlab = "Inverse variance",
                       ylab = "Effect sizes"),
                  dots))

  for(cutoff in cutoffs) lines(y, cutoff/sqrt(y))

}


#' @rdname funnel
#' @export
logfunnel = function(yi, vi, data, likelihood, alpha,
                     col = c("red", "blue"),
                     pch = c(20, 20), ...) {

  if(!missing(data)) {
    yi = data$yi
    vi = data$vi
  }

  yi_ = abs(yi)
  col_index = (sign(yi) > 0) + 1
  col_ = col[col_index]
  funnel(yi_, vi, likelihood = likelihood, alpha = alpha, col = col_, log = "xy")
}


#' Forest Plot
#'
#' Create a post-analaysis forest plot.
#'
#' @export
#' @param object The \code{mafit} object to plot.
#' @param labeltext Character vector describing each study. Defaults to
#'     "Study 1", "Study 2", etc.
#' @param ... Passed to \code{forestplot}.q1
#' @return \code{NULL}, the function is called for its side effects.

forest = function(object, labeltext = NULL, ...) {

  theta = rstan::extract(object)$theta
  n = length(object@yi)
  if(is.null(labeltext)) labeltext = paste("Study", 1:n)
  quantiles = apply(theta, 2, function(x) quantile(x, c(0.05, 0.5, 0.95)))

  forestplot(labeltext = labeltext,
             mean = quantiles[2, ],
             lower = quantiles[1, ],
             upper = quantiles[3, ], ...)
}

