#' Studies on Effect of Violent Video Games on Negative Outcomes
#'
#' Results from 477 studies on the effect of violent video games on negative
#'    outcomes.
#'
#' @usage dat.anderson2010
#'
#' @format The data frame contains the following columns:
#'     \tabular{lll}{
#'         \strong{author}       \tab \code{character} \tab first author \cr
#'         \strong{year}         \tab \code{numeric}   \tab publication year \cr
#'         \strong{outcome}      \tab \code{character} \tab one of seven outcomes \cr
#'         \strong{best   }      \tab \code{boolean}   \tab if \code{TRUE}, the was a best practice study \cr
#'         \strong{experimental} \tab \code{boolean}   \tab if \code{TRUE}, the study was experimental \cr
#'         \strong{adult}        \tab \code{boolean}   \tab if \code{TRUE}, the study subjects were adults \cr
#'         \strong{country}      \tab \code{character} \tab country of study \cr
#'         \strong{ni}           \tab \code{numeric}   \tab sample size \cr
#'         \strong{yi}           \tab \code{numeric}   \tab observed mean difference in outcome (violent vs. non-violent) \cr
#'         \strong{vi}           \tab \code{numeric}   \tab corresponding sampling variance
#'    }
#' @keywords datasets
#'
#' @references
#'     Baskerville, N. B., Liddy, C., & Hogg, W. (2012). Systematic review and meta-analysis of practice facilitation within primary care settings. The Annals of Family Medicine, 10(1), 63-74.
#'     Hilgard, J., Engelhardt, C. R., & Rouder, J. N. (2017). Overstated evidence for short-term effects of violent games on affect and behavior: A reanalysis of Anderson et al.(2010).
#'
#' @source \url{https://github.com/Joe-Hilgard/Anderson-meta}
"dat.anderson2010"
