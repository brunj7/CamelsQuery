## code to prepare `chem_fields_drop.csv` dataset goes here

chem_fields_drop <- read.csv("inst/extdata/chem_fields_drop.csv", stringsAsFactors = FALSE)
usethis::use_data(chem_fields_drop, overwrite = TRUE)