context("Testing GGplot2 Extension Functions")

rawData <- readr::read_tsv("C:/_coursera/MasteringSoftwareDevelopmentWithR/course5/inst/extData/earthquakes-2021-06-25_15-27-06_+0100.tsv")
standardisedNoaaData <- suppressMessages(suppressWarnings(eq_location_clean(rawData)))

mapChina <- eq_map(standardisedNoaaData %>% dplyr::filter(Country == "China" & lubridate::year(Date) > 2000),
                    annot_col = "Location")

mapUs <- eq_map_label(standardisedNoaaData %>% dplyr::filter(Country == "United States" & lubridate::year(Date) > 2000))

test_that("The eq_map function creates a leaflet map.",{
  expect_is(mapChina, 'leaflet')
  expect_is(mapChina, 'htmlwidget')
})

test_that("The eq_map_label function creates a leaflet map.",{
  expect_is(mapUs, 'leaflet')
  expect_is(mapUs, 'htmlwidget')
})
