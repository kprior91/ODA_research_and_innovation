# 1) Extract list of research sector codes from IATI -----------------------------

# Function to extract 5-digit sector codes

sector_extract <- function(page) {
  path <- paste0("https://iati.cloud/api/sectors/?fields=category,url,name,code&format=json&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # Condition to check when 5-digit codes stop being returned
  if(!("category" %in% names(response$results))) {
    sector_list <- rbind(sector_list_final, response$results)
  } else {
    sector_list <- sector_list_final
  }
  return(sector_list)
}

# Prepare results data frame and counters
sector_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(sector_list_final)
  sector_list_final <- sector_extract(page)
  page <- page + 1
  y <- nrow(sector_list_final)
  new_rows = y - x
}

# Keep research codes only (11)
sector_list_research <- sector_list_final %>% 
  filter(str_detect(str_to_lower(name), "research") | 
           str_detect(str_to_lower(name), "higher education") |
           str_detect(str_to_lower(name), "information and communication technology"))

sector_codes <- paste(sector_list_research$code, collapse=",")

# 2) Extract list of UK government publishers to IATI -----------------------------

# Function to extract all UK government reporters

gov_reporters <- function(page) {
  path <- paste0("https://iati.cloud/api/publishers/?q=GB-GOV&q_fields=reporting_org_identifier?format=json&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE)

  # Condition to check when 5-digit codes stop being returned
  if(!("category" %in% names(response$results))) {
    reporters <- rbind(reporter_list_final, response$results)
  } else {
    reporters <- reporter_list_final
  }
  return(reporters)
}

# Prepare results data frame and counters
reporter_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new reporter codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(reporter_list_final)
  reporter_list_final <- gov_reporters(page)
  page <- page + 1
  y <- nrow(reporter_list_final)
  new_rows = y - x
}
