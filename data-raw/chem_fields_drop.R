## code to prepare `chem_fields_drop.csv` dataset goes here

chem_fields_drop <- readr::read_csv("inst/extdata/chem_fields_drop.csv")
usethis::use_data(chem_fields_drop, overwrite = TRUE)
