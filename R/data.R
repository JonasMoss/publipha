#' Studies on the Effect of Power Posing
#'
#' Results from 27 studies related to power posing.
#'
#' The data points are taken from the p-curve analysis of Cuddy et al. (2018),
#'    restricted to 2 cell designs with mean difference as the outcome
#'    variable.
#'
#' @usage dat.cuddy2018
#'
#' @format The data frame contains the following columns:
#'     \tabular{lll}{
#'         \strong{author}       \tab \code{character} \tab first author \cr
#'         \strong{year}         \tab \code{numeric}   \tab publication year \cr
#'         \strong{power}        \tab \code{boolean}   \tab if \code{TRUE}, the outcome was feelin of power \cr
#'         \strong{ease}         \tab \code{boolean}   \tab if \code{TRUE}, the outcome was an EASE variable \cr
#'         \strong{yi}           \tab \code{numeric}   \tab standardized mean difference \cr
#'         \strong{vi}           \tab \code{numeric}   \tab corresponding sampling variance
#'    }
#'
#' @keywords datasets
#'
#' @references
#'     Cuddy, A. J., Schultz, S. J., & Fosse, N. E. (2018). P-curving a more comprehensive body of research on postural feedback reveals clear evidential value for power-posing effects: Reply to Simmons and Simonsohn (2017). Psychological science, 29(4), 656-666.
#'
#' @source \url{https://osf.io/jx3av/}
"dat.cuddy2018"

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


#' Studies on Practice Facilitation
#'
#' Results from 23 studies on the effect of practice facilitation in a
#'     primary care setting.
#'
#' @usage dat.baskerville2012
#'
#' @format The data frame contains the following columns:
#'     \tabular{lll}{
#'         \strong{author}    \tab \code{character} \tab first author of study \cr
#'         \strong{year}      \tab \code{numeric}   \tab publication year \cr
#'         \strong{design}    \tab \code{character} \tab study design (RCT, C-RCT, or CCT) \cr
#'         \strong{blinded}   \tab \code{boolean}   \tab if \code{TRUE}, the study was blinded \cr
#'         \strong{concealed} \tab \code{boolean}   \tab if \code{TRUE}, the study was concealed \cr
#'         \strong{yi}        \tab \code{numeric}   \tab observed mean difference in outcome (facilitated vs non-facilitated) \cr
#'         \strong{vi}        \tab \code{numeric}   \tab corresponding sampling variance
#'    }
#' @keywords datasets
#'
#' @source Baskerville, N. B., Liddy, C., & Hogg, W. (2012). Systematic review and meta-analysis of practice facilitation within primary care settings. The Annals of Family Medicine, 10(1), 63-74.
#'
"dat.baskerville2012"


#' Motyl 2017
#'
#' Data from a QTL experiment on gravitropism in
#' Arabidopsis, with data on 162 recombinant inbred lines (Ler x
#' Cvi). The outcome is the root tip angle (in degrees) at two-minute
#' increments over eight hours.
#'
#' @usage data(dat.motyl2017)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{https://phenome.jax.org/projects/Moore1b}{QTL Archive}
"dat.motyl2017"

#' Dang 2018
#'
#' Results from 150 studies of ego depletion, the claim that self-control is a
#'     limited resource which is tapped whenever self-control is exerted.
#'
#' @usage data(dat.dang2018)
#'
#' @format The data frame contains the following columns:
#'
#'
#' @keywords datasets
#'
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{https://phenome.jax.org/projects/Moore1b}{QTL Archive}
"dat.dang2018"
