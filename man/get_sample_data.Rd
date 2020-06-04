% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CAMELS.R
\name{get_sample_data}
\alias{get_sample_data}
\title{get sample data from dataRetrieval pacakge}
\usage{
get_sample_data(site_names)
}
\arguments{
\item{site_names}{list of site names}
}
\value{

}
\description{
this is mainly a helper function for using the readWQPdata() function from the dataRetrieval package in that it helps the user identify if any of the station of interest dont have sample data associated with them, or simply dont exist.
}
\examples{

site_names <- c("USEPA-440432070255401","test", "USGS-010158001", "USGS-01011100", "test2")
sample_data <- get_sample_data(site_names)

}