library("magrittr")

dat.motyl2017 = readxl::read_xls("data_raw/dat.motyl2017.xls")

## Filters the data so only F and t are left.
dat.motyl2017 %>%
  dplyr::filter(stringr::str_detect(copied.statistic, "^F\\(1") |
                  stringr::str_detect(copied.statistic, "^t")) ->
  dat.motyl2017

dat.motyl2017 %>%
  dplyr::mutate(z = ifelse(stringr::str_detect(copied.statistic, "^F\\(1"),
                           sqrt(test.statistic),
                           test.statistic)) ->
  dat.motyl2017

## Use only those with df given:
dat.motyl2017 = dplyr::filter(dat.motyl2017, !is.na(df.denominator) & !is.na(z))

dat.motyl2017$vi = 1/dat.motyl2017$df.denominator
dat.motyl2017$vi[sapply(dat.motyl2017$design == "Between", isTRUE)] =
  dat.motyl2017$vi[sapply(dat.motyl2017$design == "Between", isTRUE)]*2

dat.motyl2017$yi = dat.motyl2017$z*sqrt(dat.motyl2017$vi)

dat.motyl2017 = dplyr::transmute(dat.motyl2017,
                                 author = first.author,
                                 year = year,
                                 study = study.number,
                                 journal = journal,
                                 design = design,
                                 experimental = expcor == "Experimental (i.e., all IVs were manipulated)",
                                 ni = sample.size,
                                 yi = yi,
                                 vi = vi)

dat.motyl2017 = dplyr::filter(dat.motyl2017, yi < 3)

save(dat.motyl2017, file = "data/dat.motyl2017.rda")
load("data/dat.motyl2017.rda")
