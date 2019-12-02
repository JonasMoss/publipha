#' The 'publipha' package.
#'
#' @description Meta-analysis that corrects for publication selection bias and
#'     p-hacking.
#'
#' @docType package
#' @name publipha-package
#' @aliases publipha
#' @useDynLib publipha, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom rstan sampling
#'
#' @references
#' Hedges, Larry V. "Modeling publication selection effects in meta-analysis."
#' Statistical Science (1992): 246-255.
#'
#' Moss, Jonas and De Bin, Riccardo. "Modelling publication
#' bias and p-hacking" (2019) arXiv:1911.12445
#'
#' Stan Development Team (2018). RStan: the R interface to Stan. R package
#' version 2.18.1. https://mc-stan.org
#'
NULL
