context("Testing GGplot2 Extension Functions")

basePath <- system.file('extData', package='CapstoneOne')
inputFile <- paste0(basePath,"/earthquakes-2021-06-25_15-27-06_+0100.tsv")

rawData <- read_noaa_file(inputFile)
standardisedNoaaData <- suppressMessages(suppressWarnings(eq_location_clean(rawData)))

graphChina <- ggplot2::ggplot(standardisedNoaaData %>% dplyr::filter(Country == "China" & lubridate::year(Date) > 1800), 
                              ggplot2::aes(x=Date, colour=Deaths, label=Location, n_max=5, size=Mag), alpha=0.1) + 
                geom_timeline() +
                geom_timeline_label() +
                ggplot2::scale_shape_identity() +
                theme_quake +
                ggplot2::labs(size="Richter Scale", colour="Casualties")

test_that("The ggplot extensions geom_timeline and geom_timeline_label create a ggplot class.",{
  expect_is(graphChina, 'gg')
  expect_is(graphChina, 'ggplot')
})
