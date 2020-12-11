## code to prepare `chem_fields_drop.csv` dataset goes here

chem_fields_drop <- read.csv("inst/extdata/chem_fields_drop.csv")
chem_fields_drop <- tidyr::separate(chem_fields_drop,field_drop, c("id", "fieldname"), sep = "-") %>%
  dplyr::select(fieldname) %>%
  dplyr::mutate(fieldname = trimws(fieldname))

# Write the file for the package
usethis::use_data(chem_fields_drop, overwrite = TRUE)
