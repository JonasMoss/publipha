library("magrittr")

# This file massages the data from Cuddy et al. 2017.
# The xlsx file is from https://osf.io/pfh6r/, the supplementaty material of
#
# @article{cuddy2018p,
# title={P-Curving a More Comprehensive Body of Research on Postural Feedback
#   Reveals Clear Evidential Value For Power-Posing Effects: Reply to Simmons and
#   Simonsohn (2017)},
# author={Cuddy, Amy JC and Schultz, S Jack and Fosse, Nathan E},
# journal={Psychological science},
# volume={29},
# number={4},
# pages={656--666},
# year={2018},
# publisher={SAGE Publications Sage CA: Los Angeles, CA}
# }

dat.cuddy2018 = readxl::read_xlsx("data_raw/dat.cuddy2018.xlsx")

dat.cuddy2018 %>%
  dplyr::filter(!is.na(`(Main) Results`)) %>%
  dplyr::select(study = Study, results = `(Main) Results`) ->
  dat.cuddy2018

splited = matrix(unlist(strsplit(dat.cuddy2018$results, "=")), nrow = 2)
z = as.numeric(splited[2, ])
z = sqrt(z)*(substr(splited[1, ], 1, 1) != "t") +
    z*(substr(splited[1, ], 1, 1) == "t")

df = c(83,  83,  40,  83,  26,  67,  39, Inf, Inf, Inf,
       164, 60, Inf,  34,  95, Inf, 147, 126,  73, Inf,
       53,  68,  79,  39,  28,  70, 101,  79,  58, 196,
       20,  37,  37,  18,  16,  18,  51, Inf,  37, Inf,
       79,  58,  84, Inf, 118,  87,  80,  58, Inf, Inf,
       29,  66,  Inf)

dat.cuddy2018 %>%
  dplyr::filter(!is.infinite(df)) ->
  dat.cuddy2018

dat.cuddy2018$vi = 2/(df[!is.infinite(df)])
dat.cuddy2018$yi = z[!is.infinite(df)]*sqrt(dat.cuddy2018$vi)

dat.cuddy2018 = dplyr::transmute(dat.cuddy2018,
                                 author = study,
                                 yi = yi,
                                 vi = vi)

save(dat.cuddy2018, file = "data/dat.cuddy2018.rda")
rm(splited, df, z)
load("data/dat.cuddy2018.rda")
rm(dat.cuddy2018)
