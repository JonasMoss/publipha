# publipha

*Note:* This project has no pre-release yet! If you're interested in a working package, you will have to wait.

## Description

`straussR` is an `R`-package for Bayesian meta-analysis that corrects for publication bias and p-hacking. Its main features are:

* Explicit modeling and correction for publication bias and p-hacking. The degree of p-hacking can depend
  on covariates.
* The random effects distribution can be chosen among possibly non-normal alternatives.
* Support for covariates in all the parameters of the random effects distribution, and a large number of links.
* Flexible choice of priors for all parameters.

The sampling is done in [STAN](mc-stan.org/).

## Example usage

Here's an artificial example with an imaginary data set. `x` and `y` are two covariates, `z` are 
the z-values of the studies, and `n` contains the (scaled) number of participants in each study. 
The effect size distribution is `gumbel`, which is a more reasonable choice than the normal if we
suspect the effects to be skewed. `p` is the propensity to p-hack, and lives on the unit interval. 
I use the term `probit(p) ~ 1 + n` since the p-hacking propensity should decrease with the study size, 
due to the intuition that large studies get published no matter what.

```r
formula = z ~ gumbel(mean ~ 1 + x, 
                     log(sd) ~ 1 + y, 
                     probit(p) ~ 1 + n)
                     
priors = list(mean = list((Intercept) ~ gamma(2, 1),
                          x ~ weibull(2, 3),
              sd   = list((Intercept) ~ normal(0, 1)),
                          y ~ gumbel(0, 1),
              p    = list((Intercept) ~ student_t(2, 0, 1)),
                          n ~ skew_normal(0, 1, 10))

straussR(formula = formula, data = data, priors = priors)
```
