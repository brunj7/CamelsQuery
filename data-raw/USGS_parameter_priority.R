## code to prepare `USGS_parameter_priority` dataset goes here

USGS_parameter_priority <- readr::read_csv("inst/extdata/USGS_parameter_priority.csv")
usethis::use_data(USGS_parameter_priority, overwrite = TRUE)
