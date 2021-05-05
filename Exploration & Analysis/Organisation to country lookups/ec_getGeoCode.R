
ec_getGeoCode <- function (gcStr, API = c("osm", "google")[1], JSON = FALSE, 
                        verbose = 0) 
{
  gcStr <- enc2utf8(gsub(" ", "%20", gcStr))
  if (API == "google") {
    if (JSON) {
      connectStr <- paste("http://maps.google.com/maps/api/geocode/json?sensor=false&address=", 
                          gcStr, sep = "")
      if (verbose) 
        cat("fetching ", connectStr, "\n")
      con <- url(connectStr)
      data.json <- readLines(con)
      close(con)
      iLoc = grep("\"location\"", data.json, fixed = TRUE)
      iLat = grep("\"lat\"", data.json, fixed = TRUE)
      iLat = min(iLat[iLat > iLoc])
      lat = as.numeric(gsub(",", "", gsub("\"lat\" : ", 
                                          "", data.json[iLat])))
      iLng = grep("\"lng\"", data.json, fixed = TRUE)
      iLng = min(iLng[iLng > iLoc])
      lng = as.numeric(gsub(",", "", gsub("\"lng\" :", 
                                          "", data.json[iLng])))
    }
    else {
      connectStr <- paste("http://maps.google.com/maps/api/geocode/xml?sensor=false&address=", 
                          gcStr, sep = "")
      if (verbose) 
        cat("fetching ", connectStr, "\n")
      con <- url(connectStr)
      data.xml <- readLines(con)
      close(con)
      iLoc = grep("<location>", data.xml, fixed = TRUE)
      iLat = grep("<lat>", data.xml, fixed = TRUE)
      iLat = min(iLat[iLat > iLoc])
      lat = as.numeric(gsub("</lat>", "", gsub("<lat>", 
                                               "", data.xml[iLat])))
      iLng = grep("<lng>", data.xml, fixed = TRUE)
      iLng = min(iLng[iLng > iLoc])
      lng = as.numeric(gsub("</lng>", "", gsub("<lng>", 
                                               "", data.xml[iLng])))
    }
  }
  else if (API == "osm") {
    if (JSON) {
    }
    else {
      connectStr <- paste0("https://nominatim.openstreetmap.org/search?q=", 
                           gcStr, "&format=xml&limit=1")
      if (verbose) 
        cat("fetching ", connectStr, "\n")
      con <- url(connectStr)
      data.xml <- readLines(con, warn = FALSE)
      close(con)
      tmp = unlist(strsplit(data.xml, " "))
      iLat = grep("lat=", tmp, fixed = TRUE)
      lat = as.numeric(gsub("lat='|'", "", 
                            tmp[iLat]))
      iLng = grep("lon=", tmp, fixed = TRUE)
      lng = as.numeric(gsub("lon='|'", "", 
                            tmp[iLng]))
    }
  }
  gcodes <- as.numeric(c(lat, lng))
  
  if(length(gcodes) == 2) {
    names(gcodes) <- c("lat", "lon")
  } 
  
  return(gcodes)
}
