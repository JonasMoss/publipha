# Run to generate testdata.
set.seed(313)
small_model <- publipha::psma(yi = yi, vi = vi, data = dat.baskerville2012,
                             chains = 1, iter = 10)

phack_model <- publipha::phma(yi = yi, vi = vi, data = dat.baskerville2012,
                              chains = 1, iter = 10)


saveRDS(small_model, "inst/testdata/small_model.rds")
saveRDS(phack_model, "inst/testdata/phack_model.rds")
