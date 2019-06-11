### ============================================================================
### anderson2010
### ============================================================================

set.seed(313)
dat = subset(dat.anderson2010, experimental == TRUE)
alpha = c(0, 0.025, 0.05, 1)

ma_anderson2010 = allma(yi, vi, dat, alpha = alpha, chains = 1)
loo_anderson2010 = lapply(ma_anderson2010, loo)

funnel(ma_anderson2010$phma)
### ============================================================================
### cuddy2018
### ============================================================================

set.seed(313)
dat = subset(dat.cuddy2018)
alpha = c(0, 0.025, 0.05, 1)

ma_cuddy2018 = allma(yi, vi, dat, alpha = alpha, chains = 4)
loo_cuddy2018 = lapply(ma_cuddy2018, loo)

### ============================================================================
### baskerville2012
### ============================================================================

set.seed(313)
dat = subset(dat.baskerville2012)
alpha = c(0, 0.05, 1)

ma_baskerville2012 = allma(yi, vi, dat, alpha = alpha, chains = 1)
loo_baskerville2012 = lapply(ma_baskerville2012, loo)


### ============================================================================
### molloy2014
### ============================================================================

dat = metafor::escalc(measure = "ZCOR",
                      ri = ri,
                      ni = ni,
                      data = metafor::dat.molloy2014,
                      slab = paste(authors, year, sep=", "))
alpha = c(0, 0.025, 0.05, 1)

ma_molloy2014 = allma(yi, vi, dat, alpha = alpha, chains = 1)
loo_molloy2014 = lapply(ma_molloy2014, loo)

### ============================================================================
### bangertdrowns2004
### ============================================================================

dat = metafor::dat.bangertdrowns2004
alpha = c(0, 0.01, 1)

ma_bangertdrowns2004 = allma(yi, vi, dat, alpha = alpha, chains = 1)
loo_bangertdrowns2004 = lapply(ma_bangertdrowns2004, loo)

### ============================================================================
### begg1989
### ============================================================================

dat = metafor::dat.begg1989
alpha = c(0, 0.01, 1)

ma_begg1989 = allma(yi, vi, dat, alpha = alpha, chains = 1)
loo_begg1989 = lapply(ma_begg1989, loo)

## =============================================================================
## simulations2018
## =============================================================================

set.seed(1337)
theta0 = 0
tau = 0.2
n = 40
alpha = c(0, 0.01, 1)
eta = c(0.9, 0.1)
vi = 1/sample(40:80, n, replace = TRUE)
yi = rph(n, rnorm(n, theta0, tau), sqrt(vi), alpha, eta)
dat = list(yi = yi, vi = vi)

ma_simulations2018 = allma(yi, vi, dat, alpha = alpha, chains = 1)
loo_simulations2018 = lapply(ma_simulations2018, loo)
