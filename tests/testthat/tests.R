library(CamelsQuery)
library(testthat)
library(dplyr)


# test_that("WQ data error", {
#   ## want to make sure that the get_sample_data() function can handle incorrect site names and gives a message about an incorrect site rather than exiting the function
#    gauges_new <- trial_sites %>%
#     mutate(renamed_site = paste(SiteAgency, "-", SiteNumber, sep = ""))
#
#   sites_list <- c(gauges_new$renamed_site[1:5], "test")
#
#   expect_message(get_sample_data(sites_list))
#
# })


# example of another test that might be performed on the extract data function once we figure out the saved data/filepath issue
# test_that("correct list length", {
#
#   hucs <- c("01013500", "07290650", "07056000")
#
#   basin_dir <- "./inst/extdata/basin_dataset_public_v1p2"
#   attr_dir <- "./inst/extdata/camels_attributes_v2.0"
#
#   attr_list <- extract_huc_data(basin_dir, attr_dir, hucs)
#   expect_equal(length(attr_list), 9)
#
# })


