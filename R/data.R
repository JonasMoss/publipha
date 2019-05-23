#' Arabidopsis QTL data on gravitropism
#'
#' Data from a QTL experiment on gravitropism in
#' Arabidopsis, with data on 162 recombinant inbred lines (Ler x
#' Cvi). The outcome is the root tip angle (in degrees) at two-minute
#' increments over eight hours.
#'
#' @usage data(dat.cuddy2018)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{https://phenome.jax.org/projects/Moore1b}{QTL Archive}
#'
#' @examples
#' data(grav)
#' times <- attr(grav, "time")
#' phe <- grav$pheno
#' \donttest{iplotCurves(phe, times)}
"dat.cuddy2018"

#' Anderson 2010
#'
#' Data from a QTL experiment on gravitropism in
#' Arabidopsis, with data on 162 recombinant inbred lines (Ler x
#' Cvi). The outcome is the root tip angle (in degrees) at two-minute
#' increments over eight hours.
#'
#' @usage data(dat.anderson2010)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{https://phenome.jax.org/projects/Moore1b}{QTL Archive}
#'
#' @examples
#' data(grav)
#' times <- attr(grav, "time")
#' phe <- grav$pheno
#' \donttest{iplotCurves(phe, times)}
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
#' @examples
#' data(grav)
#' times <- attr(grav, "time")
#' phe <- grav$pheno
#' \donttest{iplotCurves(phe, times)}
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
#'
#' @examples
#' data(grav)
#' times <- attr(grav, "time")
#' phe <- grav$pheno
#' \donttest{iplotCurves(phe, times)}
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
#'
#' @examples
#' data(grav)
#' times <- attr(grav, "time")
#' phe <- grav$pheno
#' \donttest{iplotCurves(phe, times)}
"dat.dang2018"
