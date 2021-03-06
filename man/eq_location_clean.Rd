% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helperFunctions.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{This function cleans the NOAA earthquake data frame.
It standardises country names according to some known naming inconsistencies in the file.}
\usage{
eq_location_clean(eqData)
}
\arguments{
\item{eqData}{A dataframe containing the unformatted NOAA earthquake data.}
}
\value{
A data frame containing all the original fields in the input data, with an additional "country" and "location" field.
}
\description{
NOAA earthquake data can be downloaded from downloaded from https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data.
}
\examples{
\dontrun{eq_location_clean(my_input_data_frame)}
}
