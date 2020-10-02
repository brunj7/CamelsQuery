## code to prepare `USGS_parameter_priority` dataset goes here

USGS_parameter <- read.csv("inst/extdata/USGS_parameter_codes.csv", stringsAsFactors = FALSE)
usethis::use_data(USGS_parameter, overwrite = TRUE)
