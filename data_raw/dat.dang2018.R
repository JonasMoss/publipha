# This file massages the data from Dang's 2018 meta-analysis.
# The xlsx file is from
#  https://link.springer.com/article/10.1007%2Fs00426-017-0862-x
# in the supplementaty material.
#
# @article{dang2018updated,
#  title={An updated meta-analysis of the ego depletion effect},
#  author={Dang, Junhua},
#  journal={Psychological Research},
#  volume={82},
#  number={4},
#  pages={645--651},
#  year={2018},
#  publisher={Springer}
# }
#
# @article{carter2015series,
#  title={A series of meta-analytic tests of the depletion effect: self-control does not seem to rely on a limited resource.},
#  author={Carter, Evan C and Kofler, Lilly M and Forster, Daniel E and McCullough, Michael E},
#  journal={Journal of Experimental Psychology: General},
#  volume={144},
#  number={4},
#  pages={796},
#  year={2015},
#  publisher={American Psychological Association}
# }

dat.dang2018 = readxl::read_xlsx(path = "data_raw/dat.dang2018.xlsx",
                                 range = readxl::cell_rows(5:155))

# Use the metafor conventions.
dat.dang2018 = dplyr::rename(dat.dang2018,
                             yi = g,
                             vi = v,
                             study = Exp,
                             year = Year,
                             dv = DV,
                             iv = IV,
                             n1i = n1,
                             n2i = n2,
                             author = Authors)

# Indices for studies not in Carter et al's analysis.
new_indices = c(20:28, 42:48, 100:103, 117, 134:140, 154:155) - 5
dat.dang2018$in_carter = TRUE
dat.dang2018$in_carter[new_indices] = FALSE

dat.dang2018 = dplyr::select(dat.dang2018,
                             author,
                             year,
                             in_carter,
                             study,
                             dv,
                             iv,
                             n1i,
                             n2i,
                             yi,
                             vi)

# This follows from the excel sheet.
dat.dang2018$author[54] = "Holmqvist"
dat.dang2018$author[56] = "Holmqvist"

# Save and remove.
save(dat.dang2018, file = "data/dat.dang2018.rda")
rm(dat.dang2018, new_indices)
