## code to prepare `USGS_parameter_priority` dataset goes here

USGS_parameter <- readr::read_csv("inst/extdata/USGS_parameter_codes.csv")
usethis::use_data(USGS_parameter, overwrite = TRUE)
