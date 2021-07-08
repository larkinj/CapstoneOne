
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Submission

This package is published as part of the Mastering Software Development with R Capstone coursera course.

## Installation

You can install the the development version of this package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("larkinj/CapstoneOne")
```

## Example

## Package Details

This capstone project will be centered around a dataset obtained from the U.S. National Oceanographic and Atmospheric Administration (NOAA) on significant earthquakes around the world. This dataset contains information about 5,933 earthquakes over an approximately 4,000 year time span. This dataset has a substantial amount of information embedded in it that may not be immediately accessible to people without knowledge of the intimate details of the dataset. This package provides the tools for processing and visualizing the data so that others may extract some use out of the information embedded within.

The NOAA earthquake data is held online on the NOAA website. See https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data for more details.

This package enables users to plot earthquake information as ggplot charts and as leaflet maps. To use the package, download a NOAA file (note that a version is included in the package) and run the eq_location_clean function to standardise country names:

``` r
library(CapstoneOne)
rawData <- read_tsv("inst/extData/earthquakes-2021-06-25_15-27-06_+0100.tsv")
eqData <- eq_location_clean(rawData)
```

You can plot a timeline for a set of countries using ggplot:

``` r
ggplot(eqData %>% filter(Country %in% c("China", "Greece") & year(Date) > 1990), 
       aes(x=Date, y=Country, label=Location, n_max=5, colour=Deaths, size=Mag), alpha=0.1) + 
  geom_timeline() +
  geom_timeline_label() +
  scale_shape_identity() +
  theme_quake +
  labs(size="Richter Scale", colour="Casualties") 
```

You can plot the earthquakes on a map using leaflet:

``` r
eq_map(eqData %>% filter(Country == "China" & year(Date) > 2000),
       annot_col = "Location")
```
