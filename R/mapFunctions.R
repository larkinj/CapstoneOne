#' Plot earthquakes on a map.
#'
#' @param Data The data frame containing the earthquake data
#' @param annot_col The name of the column in Data to use to annotate the earthquakes on the map.
#'
#' @return A leaflet map showing the plotted earthquakes.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr rename
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{eq_map(Data=input_data, annot_col = "Location")}
eq_map <- function(Data, annot_col = "Date"){
  
  mapData <- Data %>% dplyr::rename(popup = !!annot_col)
  
  leaflet::leaflet(mapData) %>% 
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = ~Longitude, 
                              lat = ~Latitude, 
                              popup = ~popup)
}

#' Plot earthquakes on a map and includes custom labeling based on location, magnitude, and deaths.
#'
#' @param Data The data frame containing the earthquake data
#'
#' @return A leaflet map showing the plotted earthquakes.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr rename mutate case_when
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{eq_map(Data=input_data, annot_col = "Location")}
eq_map_label <- function(Data){
  
  mapData <- Data %>% dplyr::mutate(
    popup = dplyr::case_when(
      !is.na(Location) & !is.na(Mag) & !is.na(Deaths) 
            ~ paste0("<b>Location: </b>", Location, "<br />",
                    "<b>Magnitude: </b>", Mag, "<br />",
                    "<b>Total Deaths: </b>",Deaths),
      
      !is.na(Location) & !is.na(Mag) & is.na(Deaths) 
            ~paste0("<b>Location: </b>", Location, "<br />",
                    "<b>Magnitude: </b>", Mag),
      
      !is.na(Location) & is.na(Mag) & !is.na(Deaths) 
            ~paste0("<b>Location: </b>", Location, "<br />",
                    "<b>Total Deaths: </b>",Deaths),
      
      is.na(Location) & !is.na(Mag) & !is.na(Deaths) 
            ~paste0("<b>Magnitude: </b>", Mag, "<br />",
                    "<b>Total Deaths: </b>",Deaths),
      
      
      !is.na(Location) & is.na(Mag) & is.na(Deaths) 
            ~paste0("<b>Location: </b>", Location),
      
      is.na(Location) & !is.na(Mag) & is.na(Deaths) 
            ~paste0("<b>Magnitude: </b>", Mag),
      
      
      is.na(Location) & is.na(Mag) & !is.na(Deaths) 
            ~paste0("<b>Total Deaths: </b>",Deaths),
      
      T ~ ""
      )
  )
  
  leaflet::leaflet(mapData) %>% 
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = ~Longitude, 
                              lat = ~Latitude, 
                              popup = ~popup)
}
