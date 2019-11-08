# Run to generate testdata.
set.seed(313)
small_model <- publipha::psma(yi = yi, vi = vi, data = dat.baskerville2012,
                             chains = 1, iter = 10)


saveRDS(small_model, "inst/testdata/small_model.rds")
