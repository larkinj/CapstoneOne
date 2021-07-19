#' This function reads a NOAA earthquake file.
#' 
#' NOAA earthquake data can be downloaded from downloaded from https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data.
#'
#' @param input The location of a NOAA earthquake data file in tsv format.
#'
#' @return A data frame with a standardised list of NOAA earthquakes.
#' @export
#' @importFrom readr read_tsv cols col_character col_double
#'
#' @examples
#' \dontrun{read_noaa_file(earthquakes)}
read_noaa_file <- function(input){
  rawData <- readr::read_tsv(input,
                             col_names = c("Search Parameters","Year","Mo","Dy","Hr","Mn","Sec","Tsu","Vol",                             
                                           "Location Name","Latitude","Longitude","Focal Depth (km)","Mag","MMI Int",
                                           "Deaths","Death Description","Missing","Missing Description","Injuries","Injuries Description",           
                                           "Damage ($Mil)","Damage Description","Houses Destroyed","Houses Destroyed Description",
                                           "Houses Damaged","Houses Damaged Description","Total Deaths","Total Death Description","Total Missing",                    
                                           "Total Missing Description","Total Injuries","Total Injuries Description","Total Damage ($Mil)",
                                           "Total Damage Description","Total Houses Destroyed","Total Houses Destroyed Description",
                                           "Total Houses Damaged","Total Houses Damaged Description"),
                             col_types = readr::cols(readr::col_character(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_character(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double(),
                                                     readr::col_double(),readr::col_double(),readr::col_double()),
                             na="",
                             skip=1)
}

#' This function downloads the list of countries from the UN website. 
#' The UN countries will be used as the basis for standardising country names in the NOAA file.
#' The function customises the UN list to better match the "countries" listed in the NOAA file. 
#'
#' @return A data frame with a standardised list of countries and their equivalent in the NOAA file.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename mutate bind_rows filter case_when
#' @importFrom readxl read_xlsx
#'
#' @examples
#' \dontrun{build_country_list()}
build_country_list <- function(){
  ################################
  # The UN keeps a list of countries on it's website. Download the file.
  ################################
  download.file(url = "https://untermportal.un.org/unterm/country/downloadfile?targetLanguage=fr", 
                destfile = "countries.xlsx", 
                mode="wb")
  
  ################################
  # Create a dataset based on the countries listed in the UN file.
  ################################
  countries <- readxl::read_xlsx(path="countries.xlsx") %>%
    dplyr::select(1) %>%
    dplyr::rename(NoaaName = 1) %>%
    dplyr::mutate(Country = NoaaName)
  
  file.remove("countries.xlsx")
  
  ################################
  # There are discrepancies between the UN country names and the NOAA country names. 
  # Load the UN country dataset and resolve these discrepancies
  ################################
  countries <- countries %>%
    dplyr::mutate(NoaaName = dplyr::case_when(
      Country == "Syrian Arab Republic (the)" ~ "Syria",
      Country == "Republic of Korea (the)" ~ "South Korea",
      Country == "Iran (Islamic Republic of)" ~ "Iran",
      Country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "United Kingdom",
      Country == "Democratic People's Republic of Korea (the)" ~ "North Korea",
      Country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
      Country == "Bosnia and Herzegovina" ~ "Bosnia-Herzegovina",
      Country == "Central African Republic (the)" ~ "Central African Republic",
      Country == "Democratic Republic of the Congo (the)" ~ "Congo",
      Country == "Czechia" ~ "Czech Republic",
      Country == "Philippines (the)" ~ "Philippines",
      Country == "Russian Federation (the)" ~ "Russia",
      Country == "Micronesia (Federated States of)" ~ "Micronesia, Fed. States of",
      Country == "Dominican Republic (the)" ~ "Dominican Republic",
      Country == "Myanmar (Burma)" ~ "Myanmar",
      Country == "Comoros (the)" ~ "Comoros",
      Country == "Fiji" ~ "Fiji Islands",
      Country == "Samoa" ~ "Samoa Islands",
      Country == "Tonga" ~ "Tonga Islands",
      Country == "Vanuatu" ~ "Vanuatu Islands",
      Country == "Sudan (the)" ~ "Sudan",
      Country == "United Republic of Tanzania (the)" ~ "Tanzania",
      Country == "Netherlands (the)" ~ "The Netherlands",
      Country == "Viet Nam" ~ "Vietnam",
      T ~ Country
    ))
  
  ################################
  # Build a list of US states
  # US earthquakes are logged by state instead of by country
  ################################
  usStates <- data.frame(NoaaName = c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut',
                                      'Delaware','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa',
                                      'Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan',
                                      'Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire',
                                      'New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio',
                                      'Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota',
                                      'Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming',
                                      'Alaska Peninsula','California-Nevada','California, Mexico','California; Mexico',
                                      'Hawaiian Islands','Nevada-California Border','Puerto Rico','Washington-Oregon Border'), 
                         Country = "United States")
  
  ################################
  # Build a list of Canadian provinces
  # Canadian earthquakes are logged by province instead of by country
  ################################
  canadianProvinces <- data.frame(
    NoaaName = c(
      "Alberta",
      "British Columbia",
      "Manitoba",
      "New Brunswick",
      "Newfoundland and Labrador",
      "Northwest Territories",
      "Nova Scotia",
      "Nunavut",
      "Ontario",
      "Prince Edward Island",
      "Quebec",
      "Saskatchewan",
      "Yukon"),
    Country = "Canada"
  )
  
  ################################
  # Add the US states and Canadian province lookups to the list of countries
  # There are also a number of locations in the file that can be trenslated to countries and added to the lookup table.
  ################################  
  
  countries <- countries %>% 
    dplyr::bind_rows(usStates) %>% 
    dplyr::bind_rows(canadianProvinces) %>% 
    dplyr::bind_rows(data.frame(NoaaName =c("Antarctica"), Country = c("Antarctica"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Azores"), Country = c("Portugal"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Kermadec Islands"), Country = c("New Zealand"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Gisbourne"), Country = c("New Zealand"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("N. New Zealand"), Country = c("New Zealand"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Se. New Zealand"), Country = c("New Zealand"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Laos"), Country = c("Laos"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Mynamar (Burma)"), Country = c("Myanmar"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("N. Mexico"), Country = c("Mexico")))%>%
    dplyr::bind_rows(data.frame(NoaaName =c("North Corinth Gulf"), Country = c("Greece"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("S. Mexico"), Country = c("Mexico"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Taiwan"), Country = c("Taiwan"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Taiwan Region"), Country = c("Taiwan")))%>%
    dplyr::bind_rows(data.frame(NoaaName =c("Trinidad"), Country = c("Trinidad and Tobago"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Turkey-Cis"), Country = c("Turkey"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("E. Awa, Tokushima Prefecture"), Country = c("Japan"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Off Coast Sw Avalon Peninsula, Newfoundland"), Country = c("Canada"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Lhoknga, Aceh"), Country = c("Indonesia"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Sw. Sumatra"), Country = c("Indonesia"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Enshunada"), Country = c("Japan"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Seikaido-Nankaido"), Country = c("Japan"))) %>%
    dplyr::bind_rows(data.frame(NoaaName =c("Shinano"), Country = c("Japan"))) %>%
    dplyr::filter(!is.na(NoaaName))
  
  return(countries)
}

#' This function cleans the NOAA earthquake data frame. 
#' It standardises country names according to some known naming inconsistencies in the file.
#' 
#' NOAA earthquake data can be downloaded from downloaded from https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data.
#'
#' @param eqData A dataframe containing the unformatted NOAA earthquake data.
#'
#' @return A data frame containing all the original fields in the input data, with an additional "country" and "location" field.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename mutate bind_rows filter left_join inner_join
#' @importFrom lubridate make_date
#' @importFrom tidyr separate
#' @importFrom stringr str_to_title str_trim
#'
#' @examples
#' \dontrun{eq_location_clean(my_input_data_frame)}
eq_location_clean <- function(eqData){
  
  countries <- build_country_list()
  
  locList <- eqData %>%
    dplyr::filter(!is.na(Latitude) & !is.na(Longitude) & !is.na(Year) & !is.na(Mo) & !is.na(Dy)) %>%
    dplyr::mutate(Date = lubridate::make_date(Year, Mo, Dy),
                  x=`Location Name`) %>%
    tidyr::separate(`Location Name`, c("Country_tmp","Location"), sep=":") %>%
    dplyr::mutate(Country_tmp = stringr::str_to_title(Country_tmp)) %>%
    dplyr::left_join(countries, by=c("Country_tmp"="NoaaName"))
  
  ################################
  # Seperate the list of locations into those where a country was assigned;
  # And those where a country was not assigned
  ################################  
  matches <- locList %>% dplyr::filter(!is.na(Country))
  mismatches <- locList %>% dplyr::filter(is.na(Country)) 
  
  ################################
  # Many of the discrepancies are related to naming conventions in the Balkans
  ################################  
  balkans <- mismatches %>% 
    dplyr::filter(Country_tmp == "Balkans Nw") %>%
    dplyr::mutate(Balkan = stringr::str_to_title(stringr::str_trim(Location)),
                  Location = NA_character_) %>%
    dplyr::select(-Country) %>%
    dplyr::left_join(countries, by=c("Balkan"="NoaaName")) %>%
    dplyr::select(-Balkan)
  
  matches <- matches %>% dplyr::bind_rows(balkans)
  
  mismatches <- mismatches %>%
    dplyr::filter(Country_tmp != "Balkans Nw")
  
  ################################
  # Many of the discrepancies are related to naming conventions in Japan
  ################################  
  japanCandidates <- mismatches %>% 
    tidyr::separate(x, c("Location","Country_tmp"), sep=",") %>%
    dplyr::select(-Country) %>%
    dplyr::mutate(Country_tmp = stringr::str_to_title(stringr::str_trim(Country_tmp)))
  
  japan <- japanCandidates %>%
    dplyr::inner_join(countries, by=c("Country_tmp"="NoaaName"))
  
  matches <- dplyr::bind_rows(matches, japan)
  
  mismatches <- japanCandidates %>%
    dplyr::left_join(countries, by=c("Country_tmp"="NoaaName")) %>%
    dplyr::filter(is.na(Country))
  
  ################################
  # Mark any remaining data as miscellaneous
  ################################  
  
  mismatches <- mismatches %>%
    dplyr::mutate(Country = "miscellaneous")
  
  matches <- matches %>% 
    dplyr::bind_rows(mismatches) %>% 
    dplyr::select(-Country_tmp) %>%
    dplyr::rename(NoaaName = x) %>%
    dplyr::mutate(Location = stringr::str_to_title(stringr::str_trim(Location)))
  
  return(matches )
}
