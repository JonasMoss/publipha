dat.baskerville2012 = read.csv("data_raw/dat.baskerville2012.csv",
                               row.names = NULL,
                               header = FALSE)

names(dat.baskerville2012) = c("author","year","yi", "vi",
                               "blinded", "design","concealed")

dat.baskerville2012$blinded = dat.baskerville2012$blinded == 1
dat.baskerville2012$vi = dat.baskerville2012$vi^2

dat.baskerville2012 = dplyr::select(dat.baskerville2012,
                                    author,
                                    year,
                                    design,
                                    blinded,
                                    concealed,
                                    yi,
                                    vi)

save(dat.baskerville2012, file = "data/dat.baskerville2012.rda")
dat.baskerville2012 = load("data/dat.baskerville2012.rda")
rm(dat.baskerville2012)
