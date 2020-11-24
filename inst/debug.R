
library(tidyverse)
library(ggplot2)
library(lubridate) # for dates
library(janitor) # clean column names
# remotes::install_github("brunj7/CamelsQuery")
library(CamelsQuery)
library(dataRetrieval)


# Paths
main_dir <- "~/Data/camels/" #"/home/shares/camels/"
my_basin_dir <- file.path(main_dir, "basin_dataset_public_v1p2")
my_attr_dir <- file.path(main_dir, "camels_attributes_v2.0")


site_ids <- c("03010655","03011800","03015500","03021350","03026500","03028000","03049000","03049800","03050000","03066000","03069500",
         "03070500","03076600","03078000","03140000","03144000","03159540","03161000","03164000","03165000","03170000","03173000",
         "03180500","03182500","03186500","03187500","03213700","03237280","03237500","03238500","03241500","03280700","03281100",
         "03281500","03285000","03291780","03300400","03338780","03340800","03346000","03357350","03364500","03366500","03368000","03384450")


camels_data <- extract_huc_data(basin_dir = my_basin_dir,
                                attr_dir = my_attr_dir,
                                huc8_names = site_ids)

### Get the USGS data for those sites----
message("<---- Getting USGS flow and chemistry ---->")

usgs_data <- get_sample_data(site_ids) # takes some time
