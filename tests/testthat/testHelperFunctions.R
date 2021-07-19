context("Testing NOAA File Helper Functions")

basePath <- system.file('extData', package='CapstoneOne')
inputFile <- paste0(basePath,"/earthquakes-2021-06-25_15-27-06_+0100.tsv")

countryList <- suppressMessages(suppressWarnings(build_country_list()))
test_that("The function to create a standardised list of countries works as expected.",{
  expect_is(countryList, 'data.frame')
  expect_true(nrow(countryList) > 0)
  expect_true(ncol(countryList) == 2)
})

rawData <- read_noaa_file(inputFile)
test_that("The function to create a standardised list of countries works as expected.",{
  expect_is(rawData, 'data.frame')
  expect_true(nrow(rawData) > 0)
  expect_true(ncol(rawData) == 39)
})

standardisedNoaaData <- suppressMessages(suppressWarnings(eq_location_clean(rawData)))

test_that("The function to create a standardised list of earthquakes works as expected.",{
  expect_is(standardisedNoaaData, 'data.frame')
  expect_true(nrow(standardisedNoaaData) > 0)
  expect_true("Location" %in% colnames(standardisedNoaaData))
  expect_true("Country" %in% colnames(standardisedNoaaData))
})
