context("Testing NOAA File Helper Functions")

countryList <- suppressMessages(suppressWarnings(build_country_list()))
test_that("The function to create a standardised list of countries works as expected.",{
  expect_is(countryList, 'data.frame')
  expect_true(nrow(countryList) > 0)
  expect_true(ncol(countryList) == 2)
})

rawData <- readr::read_tsv("C:/_coursera/MasteringSoftwareDevelopmentWithR/course5/inst/extData/earthquakes-2021-06-25_15-27-06_+0100.tsv")
standardisedNoaaData <- suppressMessages(suppressWarnings(eq_location_clean(rawData)))

test_that("The function to create a standardised list of earthquakes works as expected.",{
  expect_is(standardisedNoaaData, 'data.frame')
  expect_true(nrow(standardisedNoaaData) > 0)
  expect_true("Location" %in% colnames(standardisedNoaaData))
  expect_true("Country" %in% colnames(standardisedNoaaData))
})
