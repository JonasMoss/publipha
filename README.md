
<!-- README.md is generated from README.Rmd. Please edit that file -->

# publipha <img src="man/figures/logo.png" align="right" width="177" height="65" />

[![DOI](https://zenodo.org/badge/120678148.svg)](https://zenodo.org/badge/latestdoi/120678148)

An `R` package for Bayesian meta-analysis that accounts for publication
bias or *p*-hacking.

## Overview

publipha is an package for doing Bayesian meta-analysis that accounts
for publication bias or *p*-hacking. Its main functions are `psma` for
publication bias meta-analyisis and `phma` for *p*-hacking
meta-analysis. These functions have approximately the same syntax as
`rma` from the package
[`metafor`](https://cran.r-project.org/package=metafor). Its
functionality is:

  - `psma` does random effects meta-analysis under publication bias with
    a one-sided *p*-value based *selection probability*. The model is
    roughly the same as in Hedges (1992).
  - `phma` does random effects meta-analysis under a certain model of
    *p*-hacking with a one-sided *p*-value based *propensity to*
    p*-hack*.
  - `cma` does classical random effects meta-analysis with the same
    priors as `psma` and `cma`.

The objects returned from the -`ma` functions are `stan` objects, and
can be handled by the functions from the
[`rstan`](https://cran.r-project.org/web/packages/rstan) package, but
`publipha` offers some convenience functions to handle them with too.

## Installation

From inside `R`, use the following command:

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/publipha")
```

Call the `library` function and use it like a barebones `metafor::rma`.
The `alpha` tells `psma` or `phma` where they should place the cutoffs
for significance.

``` r
library("publipha")
# Publication bias model
set.seed(313) # For reproducibility
model_psma = publipha::psma(yi = yi,
                            vi = vi,
                            alpha = c(0, 0.025, 0.05, 1),
                            data = metafor::dat.bangertdrowns2004)

# Classical model
set.seed(313)
model_cma = publipha::cma(yi = yi,
                          vi = vi,
                          alpha = c(0, 0.025, 0.05, 1),
                          data = metafor::dat.bangertdrowns2004)
```

You can calculate the posterior means of the meta-analytic mean with
`extract_theta0`:

``` r
extract_theta0(model_psma)
#> [1] 0.1241181
```

``` r
extract_theta0(model_cma)
#> [1] 0.2206233
```

If you wish to plot a histogram of the posterior distribution of `tau`,
the standard deviation of the effect size distribution, you can do it
like this:

``` r
extract_tau(model_psma, hist)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="750px" />

## Description

Kernel density estimation with a *parametric start* was introduced by
Hjort and Glad in [Nonparametric Density Estimation with a Parametric
Start (1995)](https://projecteuclid.org/euclid.aos/1176324627). The idea
is to start out with a parametric density before you do your kernel
density estimation, so that your actual kernel density estimation will
be a correction to the original parametric estimate. This is a good idea
because the resulting estimator will be better than an ordinary kernel
density estimator whenever the true density is close to your suggestion;
and the estimator can be superior to the ordinary kernal density
estimator even when the suggestion is pretty far off.

In addition to parametric starts, the package implements some
*asymmetric kernels*. These kernels are useful when modelling data with
sharp boundaries, such as data supported on the positive half-line or
the unit interval. Currently we support the following asymmetric
kernels:

  - Jones and Henderson’s *Gaussian copula KDE*, from [Kernel-Type
    Density Estimation on the Unit Interval
    (2007)](https://academic.oup.com/biomet/article-abstract/94/4/977/246269).
    This is used for data on the unit interval. The bandwidth selection
    mechanism described in that paper is implemented as well. This
    kernel is called `gcopula`.

  - Chen’s two *beta kernels* from [Beta kernel estimators for density
    functions
    (1999)](https://www.sciencedirect.com/science/article/pii/S0167947399000109).
    These are used for data supported on the on the unit interval, and
    are called `beta` and `beta_biased`.

  - Chen’s two *gamma kernels* from [Probability Density Function
    Estimation Using Gamma Kernels
    (2000)](https://link.springer.com/article/10.1023/A:1004165218295).
    These are used for data supported on the positive half-line, and are
    called `gamma` and `gamma_biased`.

These features can be combined to make asymmetric kernel densities
estimators with parametric starts, see the example below. The package
contains only one function, `kdensity`, in addition to the generics
`plot`, `points`, `lines`, `summary`, and `print`. \#\# References

  - [Hedges, Larry V. “Modeling publication selection effects in
    meta-analysis.” Statistical Science (1992):
    246-255.](https://www.jstor.org/stable/pdf/2246311.pdf)
