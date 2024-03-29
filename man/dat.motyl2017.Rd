% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-motyl2017.R
\docType{data}
\name{dat.motyl2017}
\alias{dat.motyl2017}
\title{Effect Sizes from 875 Studies in Psychology.}
\format{
The tibble contains the following columns:
\tabular{lll}{
\strong{author}       \tab \code{character} \tab first author of study \cr
\strong{year}         \tab \code{numeric}   \tab publication year \cr
\strong{study}        \tab \code{numeric}   \tab the number given to the study in the original paper (0 = only one study was reported in the original paper; \cr
\strong{journal}      \tab \code{character} \tab journal where the study was published \cr
\strong{concealed}    \tab \code{character} \tab design of the study; "Between", "Within", or "Mixed"   \cr
\strong{experimental} \tab \code{numeric}   \tab \code{TRUE} for an experimental study \cr
\strong{ni}           \tab \code{numeric}   \tab sample size \cr
\strong{yi}           \tab \code{numeric}   \tab observed mean difference in outcome \cr
\strong{vi}           \tab \code{numeric}   \tab corresponding sampling variance
}
}
\source{
\url{https://osf.io/he8mu/}
}
\usage{
data(dat.motyl2017)
}
\description{
Effect sizes from 875 studies in psychology. Adopted from Motyl et al.
(2017).
}
\references{
Motyl, M., Demos, A. P., Carsel, T. S., Hanson, B. E.,
Melton, Z. J., Mueller, A. B., ... & Yantis, C. (2017). The state of social
and personality science: Rotten to the core, not so bad, getting better,
or getting worse?. Journal of personality and social psychology, 113(1), 34.
}
\keyword{datasets}
