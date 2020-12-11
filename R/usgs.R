#' get sample data from dataRetrieval pacakge
#'
#' this is mainly a helper function for using the readWQPdata() function from the dataRetrieval package in that it helps the user identify if any of the station of interest don't have sample data associated with them, or simply don't exist.
#'
#' @param site_names a vector of site names
#' @param chem_codes a data frame with the USGS water chemistry codes of the parameters you want to query
#' @param drop_fields a logical to set if only a subset of chemistry columns should be kept
#'
#' @return water quality sample data for sites
#' @export
#'
#' @examples
#'\dontrun{
#' site_names <- c("USEPA-440432070255401","test", "010158001", "01011100", "test2")
#' sample_data <- get_sample_data(site_names)
#'}
get_sample_data <- function(site_names, chem_codes=USGS_parameter_priority, drop_fields = TRUE) {

  ## the goal of this function is make the dataRetrieval::readWQPdata() function a bit easier to use
  #~ if site codes are incorrect readWQPdata() gives a vague error and also stops the function entirely,
  # also it doesn't tell the user which sites dont have any qater quality data
  #~~ this function identifies site codes that aren't present/don't have data tells the user about those,
  # and then uses only sites with data to run the readWQPdata() function

  ### create empty logical variables to be used for indexing working from non-working site codes
  err <- logical(length(site_names))
  no_data <- logical(length(site_names))
  working <- logical(length(site_names))

  message("<---- Checking for USGS chemistry data availability ---->")

  for (i in 1:length(site_names)) {
    ## look for sites that have sample data associated with them
    ## we use trycatch() because if a site doesn't exist the function is aborted/exited
    water_quality <- tryCatch(
      dataRetrieval::whatWQPsamples(siteid = paste0("USGS-", site_names[i])), # need to add a check on this
      error=function(e) e
    )


    ## if try catch received and error, ID this site code as being an error
    if (inherits(water_quality, "error")) {
      err[i] <- TRUE
      next

    ## if there was no sample data present for this site, ID this as a "no data" site
    } else if (length(water_quality) == 1) {
      no_data[i] <- TRUE
      next
    }

    ## if neither of the past two if statements did not trigger, ID this site code as working
    working[i] <- TRUE

  }

  ## throw a message letting the user know about incorrect site codes, or sites that don't have any data.
  if(any(err)) cat("The API returned an error for following site. Please check that these IDs are correct: \n", sprintf("%s \n", site_names[err]))
  if(any(no_data)) cat("no chemistry sample data found for sites: \n", sprintf(" %s \n", site_names[no_data]))


  # run readWQPdata() using working site names,
  # then filter for only water quality data of interest based on the list of param_codes that is stored in the package data
  ##~~~ NOTE: may want to remove this feature/ add it as a T/F option in the function

  message("<---- Retrieving USGS chemistry data  ---->")

  # split gauges into chunks to be gentler on the API
  # site_ids_chunks <- split(site_names, ceiling(seq_along(site_names[working])/10))
  # n_chunks <- length(site_ids_chunks)
  wq_data <- dataRetrieval::readWQPdata(siteid = paste0("USGS-", site_names[working]))
  # wq_data <- purrr:: map_dfr(site_ids_chunks, ~dataRetrieval::readWQPdata(siteid = paste0("USGS-", .x)))
  wq_data <- wq_data %>%
    dplyr::filter(USGSPCode %in% chem_codes$`5_digit_code`)

  # create a gauge_id field
  wq_data <- wq_data %>%
    tidyr::separate(MonitoringLocationIdentifier, c("organization", "gauge_id"), sep = "-")

  # Keep only the columns we need
  if (isTRUE(drop_fields)) {
    wq_data <- wq_data %>%
      dplyr::select(-chem_fields_drop$fieldname) # drop columns we do not want (id)
  }


  ## Get the USGS stream flow time-series
  message("<---- Retrieving USGS stream flow daily average ---->")

  flow_code <- "00060"
  flow_daily <- dataRetrieval::readNWISdv(site_names[working], flow_code) # Assuming they all have a flow - to be tested
  # flow_daily <- purrr:: map_dfr(site_ids_chunks, ~readNWISdv(.x, flow_code))

  # Cleaning things
  flow_daily <- flow_daily %>%
    dplyr::select(site_no, Date, X_00060_00003, X_00060_00003_cd ) %>%
    dplyr::rename(gauge_id = site_no) %>%
    dplyr::rename(discharge_cfs = X_00060_00003) %>%
    dplyr::rename(discharge_cfs_qflag = X_00060_00003_cd)

  ## attach the stream flow from the chemistry measurement
  # Get the flow from chemistry data
  usgs_chem_q <- wq_data %>%
    dplyr::filter(stringr::str_detect(CharacteristicName,"flow"))     # keep some room for other potential types of flow

  # Sometimes there are no chemistry data accross and entire region
  if (nrow(usgs_chem_q) == 0) {
    all_q <- flow_daily %>%
      add_column(ActivityStartDate = as.character(NA),
                 ActivityStartTime.Time = as.character(NA) ,
                 ActivityStartTime.TimeZoneCode = as.character(NA),
                 HydrologicCondition = as.numeric(NA),
                 HydrologicEvent = as.numeric(NA),
                 CharacteristicName = as.character(NA),
                 ResultMeasureValue = as.numeric(NA),
                 ResultMeasure.MeasureUnitCode = as.character(NA),
                 flow_diff = as.numeric(NA)
                 )
  } else {
    # make sure this field is returned and is numeric
    # if (ResultMeasureValue %in% names(usgs_chem_q)) {
    #   usgs_chem_q$ResultMeasureValue <- as.numeric(usgs_chem_q$ResultMeasureValue)
    # }
    usgs_chem_q <- usgs_chem_q %>%
      dplyr::select(gauge_id, ActivityStartDate, ActivityStartTime.Time,
             ActivityStartTime.TimeZoneCode, HydrologicCondition, HydrologicEvent, CharacteristicName,
             ResultMeasureValue, ResultMeasure.MeasureUnitCode) %>%
      mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) # making field numeric

    # join the flow data
    # Combine it with the flow
      all_q <- dplyr::left_join(flow_daily, usgs_chem_q, by = c("gauge_id", "Date" = "ActivityStartDate")) %>%
        dplyr::mutate(flow_diff = discharge_cfs - ResultMeasureValue)
}

  ## Get the corresponding site information
  message("<---- Retrieving USGS site information ---->")

  site_info <- dataRetrieval::readNWISsite(site_names[working]) %>%
    dplyr::select(agency_cd, site_no, station_nm, dec_lat_va, dec_long_va, dec_coord_datum_cd, state_cd, county_cd,
                  alt_va, huc_cd, drain_area_va, contrib_drain_area_va, tz_cd) %>% # select the columns of interest
    dplyr::rename(gauge_id = site_no)

  # Put all the tables into a list (for now)
  usgs_data <- list(discharge = all_q,
                    water_q = wq_data,
                    sites = site_info)


  return(usgs_data)

}



# # Slice the site list by 10 to ease the API query
# site_ids_chunks <- split(site_names, ceiling(seq_along(site_names)/10))
# n_chunks <- length(site_ids_chunks)
#
# for (c in 1:n_chunks) {
#   Sys.sleep(5)  # to be friendly






