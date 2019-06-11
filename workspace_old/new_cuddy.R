pch = rep(20, nrow(dat.cuddy2018))
pch[dat.cuddy2018$subset == "EASE"] = 10
pch[dat.cuddy2018$subset == "Power"] = 0

plot(1/dat.cuddy2018$vi, dat.cuddy2018$yi,
     pch = 20, cex = 2)
z = 1:1000
lines(z, 1.96/sqrt(z))
lines(z, 1.64/sqrt(z))

set.seed = 313
model = allma(yi, vi,
              alpha = c(0, 0.025, 0.05, 1),
              data = dplyr::filter(dat.cuddy2018))

plot(density(extract_theta(model$psma, identity), adjust = 2))
plot(density(extract_theta(model$phma, identity), adjust = 2))

plot(density(extract_theta0(model$phma, identity), adjust = 3), lty = 3,
     ylim = c(0, 9), lwd = 2, xlim = c(-0.2, 0.6), main = "n")
lines(density(extract_theta0(model$psma, identity), adjust = 3), lty = 2, lwd = 2)
lines(density(extract_theta0(model$cma, identity), adjust = 3), lty = 1, lwd = 2)
ecdf(extract_theta0(model$phma, identity))(0)
ecdf(extract_theta0(model$psma,identity))(0)
ecdf(extract_theta0(model$cma, identity))(0)
