% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ma.R
\docType{class}
\name{mafit-class}
\alias{mafit-class}
\title{Class \code{mafit}: Fitted Meta-analysis Model}
\description{
Class \code{mafit}: Fitted Meta-analysis Model
}
\section{Slots}{

\describe{
\item{\code{bias}}{The kind of bias modelled. Can be one of
\code{publication_selection}, \code{p-hacking} or \code{none}.}

\item{\code{alpha}}{Ordered numeric vector of cutoffs including 0 and 1.}

\item{\code{yi}}{Numeric vector of estimated effect sizes.}

\item{\code{vi}}{Numeric vector of study-specific variances.}

\item{\code{parameters}}{The list of prior parameters used in the fitting.}
}}

