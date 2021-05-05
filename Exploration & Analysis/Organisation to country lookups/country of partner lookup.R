install.packages("geonames")
install.packages("RgoogleMaps")
install.packages("rworldmap")
install.packages("tidyverse")
install.packages("ggmap")

library(geonames) 
library(RgoogleMaps)
library(rworldmap)
library(tidyverse)
library(ggmap)

# CHECK IF PACKAGE IS INSTALLED
if(!require("jsonlite")){
  # IF PACKAGE NOT FOUND, INSTALL IT
  install.packages("jsonlite", dependencies = TRUE)
  # LOAD PACKAGE AFTER INSTALLETION
  library("jsonlite")
}
# CHECK IF PACKAGE IS INSTALLED
if(!require("rvest")){
  # IF PACKAGE NOT FOUND, INSTALL IT
  install.packages("rvest", dependencies = TRUE)
  # LOAD PACKAGE AFTER INSTALLETION
  library("rvest")
}
# CHECK IF PACKAGE IS INSTALLED
if(!require("stringi")){
  # IF PACKAGE NOT FOUND, INSTALL IT
  install.packages("stringi", dependencies = TRUE)
  # LOAD PACKAGE AFTER INSTALLETION
  library("stringi")
}

addr <- "Overseas Development Institute"
addr <- "PwC"
addr <- "Bangladesh Center for Advanced Studies"

ec_geocode <- function(addr){
  # NOMINATIM SEARCH API URL
  src_url <- "https://nominatim.openstreetmap.org/search?q="
  
  # CREATE A FULL ADDRESS
  #addr <- paste(address, city, state, zipcode, sep = "%2C")
  
  # CREATE A SEARCH URL BASED ON NOMINATIM API TO RETURN GEOJSON
  requests <- paste0(src_url, addr, "&format=geojson")
  
  # ITERATE OVER THE URLS AND MAKE REQUEST TO THE SEARCH API
  for (i in 1:length(requests)) {
    
    # QUERY THE API TRANSFORM RESPONSE FROM JSON TO R LIST
    response <- read_html(requests[i]) %>%
      html_node("p") %>%
      html_text() %>%
      fromJSON()
    
    # FROM THE RESPONSE EXTRACT LATITUDE AND LONGITUDE COORDINATES
    lon <- response$features$geometry$coordinates[[1]][1]
    lat <- response$features$geometry$coordinates[[1]][2]
    
    # CREATE A COORDINATES DATAFRAME
    if(i == 1) {
      loc <- tibble(name = addr[i], 
                    address = str_replace_all(addr[i], "%2C", ","),
                    latitude = lat, longitude = lon)
    }else{
      df <- tibble(name = addr[i], 
                   address = str_replace_all(addr[i], "%2C", ","),
                   latitude = lat, longitude = lon)
      loc <- bind_rows(loc, df)
    }
  }
  return(loc)
}


df <- geocode(name = data$DBA_Name,
              address = query,
              city = data$City, 
              state = data$State,
              zipcode = data$Zipcode)


# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  as.character(indices$ADMIN)  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

# Use OSM or Google Maps' Geocoding APIs
coords <- getGeoCode("university of surrey")
coords <- getGeoCode("university of birmingham")
coords <- getGeoCode("PwC", API = "google")
coords <- getGeoCode("London School of Hygiene and Tropical Medicine")
coords <- getGeoCode("Overseas Development Institute")
coords <- geocode("Overseas Development Institute")

coords <- getGeoCode("Save the Children Australia")
coords <- getGeoCode("Medecins sans Frontieres")
coords <- getGeoCode("Bangladesh Center for Advanced Studies", API = "google")

coords_data <- data.frame(long = coords[2], lat = coords[1])
row.names(coords_data) = NULL

coords2country(coords_data)
