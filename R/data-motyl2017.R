#' Effect Sizes from 875 Studies in Psychology.
#'
#' Effect sizes from 875 studies in psychology. Adopted from Motyl et al.
#' (2017).
#'
#' @usage data(dat.motyl2017)
#'
#' @format The tibble contains the following columns:
#'     \tabular{lll}{
#'         \strong{author}       \tab \code{character} \tab first author of study \cr
#'         \strong{year}         \tab \code{numeric}   \tab publication year \cr
#'         \strong{study}        \tab \code{numeric}   \tab the number given to the study in the original paper (0 = only one study was reported in the original paper; \cr
#'         \strong{journal}      \tab \code{character} \tab journal where the study was published \cr
#'         \strong{concealed}    \tab \code{character} \tab design of the study; "Between", "Within", or "Mixed"   \cr
#'         \strong{experimental} \tab \code{numeric}   \tab `TRUE` for an experimental study \cr
#'         \strong{ni}           \tab \code{numeric}   \tab sample size \cr
#'         \strong{yi}           \tab \code{numeric}   \tab observed mean difference in outcome \cr
#'         \strong{vi}           \tab \code{numeric}   \tab corresponding sampling variance
#'    }
#'
#' @keywords datasets
#'
#' @references Motyl, M., Demos, A. P., Carsel, T. S., Hanson, B. E.,
#' Melton, Z. J., Mueller, A. B., ... & Yantis, C. (2017). The state of social
#' and personality science: Rotten to the core, not so bad, getting better,
#' or getting worse?. Journal of personality and social psychology, 113(1), 34.
#'
#' @source \url{https://osf.io/he8mu/}
"dat.motyl2017"
