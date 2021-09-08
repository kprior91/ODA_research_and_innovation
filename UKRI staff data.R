# Testing UKRI API - extracting staff data

person_data <- data$personRole %>% 
  unnest(col = role)

person_id <- "B25D3849-3E67-4F12-ADBE-3166F305C609"
  
# extract_person_info

path <- paste0("http://gtr.ukri.org/person/", person_id)
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 

person_org_data <- ((response$personOverview)$organisation)$name
