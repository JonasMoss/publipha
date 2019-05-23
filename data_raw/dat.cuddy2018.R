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

# Find the subsets
dat.cuddy2018$subset = "Non-EASE"
dat.cuddy2018$subset[!is.na(dat.cuddy2018$`(Feelings of Power) Results`)] = "Power"
dat.cuddy2018$subset[!is.na(dat.cuddy2018$`(EASE Subset) Results`)] = "EASE"

# Make yis and vis.
dat.cuddy2018$vi = 2/dat.cuddy2018$df
dat.cuddy2018$yi = dat.cuddy2018$value
dat.cuddy2018$yi[dat.cuddy2018$test != "t"] = sqrt(dat.cuddy2018$yi[dat.cuddy2018$test != "t"])
dat.cuddy2018$yi = dat.cuddy2018$yi*sqrt(dat.cuddy2018$vi)

# Add year and metafor-compliant authors.
dat.cuddy2018$year = c(2012, 2012,
         2010, 2010,
         2010, 2010,
         2014, 2015,
         2015, 2016,
         2011, 2015,
         2015, 2016,
         2015, 2015,
         1983, 1984,
         1982, 1982,
         2013, 2016,
         2016, 2015,
         2015, 2016,
         2016, 2016,
         2004, 2013,
         2013)

dat.cuddy2018$author = c("Arnette et al.", "Bohns et al.",
           "Carney et al.", "Carney et al.",
           "Carney et al.", "Carney et al.",
           "Ceunen", "Cuddy et al.",
           "Cuddy et al.", "Duffy et al.",
           "Fischer et al.", "Fuller et al.",
           "Kwon et al.", "Peper et al.",
           "Ranehill et al.", "Ranehill et al.",
           "Riskind", "Riskind",
           "Riskind et al.", "Riskind et al.",
           "Rotella et al.", "Teh et al.",
           "Teh et al.", "Turan",
           "Turan", "Veenstra et al.",
           "Veenstra et al.", "Wilkes et al.",
           "Wilson et al.", "Yap et al.",
           "Yap et al.")

# Filter out chisq
dat.cuddy2018 = dplyr::filter(dat.cuddy2018, test != "chi2")


dat.cuddy2018 = dplyr::transmute(dat.cuddy2018,
                                 author = author ,
                                 year = year,
                                 subset = subset,
                                 yi = yi,
                                 vi = vi)

# Remove outlier
dat.cuddy2018 = dplyr::filter(dat.cuddy2018, yi < 1.5)

save(dat.cuddy2018, file = "data/dat.cuddy2018.rda")
load("data/dat.cuddy2018.rda")
rm(dat.cuddy2018)

#
# dat.cuddy2018 %>%
#   dplyr::filter(!is.na(`(Main) Results`)) %>%
#   dplyr::select(study = Study, results = `(Main) Results`) ->
#   dat.cuddy2018
#
# splited = matrix(unlist(strsplit(dat.cuddy2018$results, "=")), nrow = 2)
# z = as.numeric(splited[2, ])
# z = sqrt(z)*(substr(splited[1, ], 1, 1) != "t") +
#     z*(substr(splited[1, ], 1, 1) == "t")
#
# df = c(83,  83,  40,  83,  26,  67,  39, Inf, Inf, Inf,
#        164, 60, Inf,  34,  95, Inf, 147, 126,  73, Inf,
#        53,  68,  79,  39,  28,  70, 101,  79,  58, 196,
#        20,  37,  37,  18,  16,  18,  51, Inf,  37, Inf,
#        79,  58,  84, Inf, 118,  87,  80,  58, Inf, Inf,
#        29,  66,  Inf)
#
# dat.cuddy2018 %>%
#   dplyr::filter(!is.infinite(df)) ->
#   dat.cuddy2018
#
# dat.cuddy2018$vi = 2/(df[!is.infinite(df)])
# dat.cuddy2018$yi = z[!is.infinite(df)]*sqrt(dat.cuddy2018$vi)
#
# dat.cuddy2018 = dplyr::transmute(dat.cuddy2018,
#                                  author = study,
#                                  yi = yi,
#                                  vi = vi)
# rm(splited, df, z)

