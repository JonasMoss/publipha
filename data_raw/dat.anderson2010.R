library("magrittr")

dat.anderson2010 = readxl::read_xls("data_raw/dat.anderson2010.xls")

dat.anderson2010 %>% dplyr::transmute(author = `Full Reference`,
                              outcome = Outcome,
                              best = `Best?` == "y",
                              experimental = Setting == "Exp",
                              adult = AGE == "Adult",
                              country = Country,
                              ni = `Sample size`,
                              yi = `Fisher's Z`,
                              vi = `Std Err`^2) ->
  dat.anderson2010

## Fills in unknown years.
years = stringr::str_split(stringr::str_split(drop(dat.anderson2010$reference),
                                              c(" \\("), simplify = TRUE)[ , 2],
                           c("\\)"), simplify = TRUE)[, 1]
dat.anderson2010$year = as.integer(years)
dat.anderson2010$in_press = (years == "in press")
dat.anderson2010$year[104:106] = 2009
dat.anderson2010$year[is.na(dat.anderson2010$year)] = 2010

rm(years)
save(dat.anderson2010, file = "data/dat.anderson2010.rda")
load("data/dat.anderson2010.rda")
rm(dat.anderson2010)
