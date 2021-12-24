# --------------------------------------------------------------- #
# Script 2
# Extract IATI partner activities linked to UK gov funders' IATI activities
# by an incoming fund transaction
# --------------------------------------------------------------- #

# 1) Extract linked R&I partner IATI activities -----

# Read in list of tagged RI UK gov funder activities from Script 1
ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities.rds")


# Extract linked partner activity IDs ----

transactions_dataset <- data.frame()

for (id in ri_iati_activities$iati_identifier) {
  new_rows <- 0
  page <- 1
  
  while (page == 1 | new_rows > 0) {
  
    print(paste0(id, "-", page))
    x <- nrow(transactions_dataset)
    
    path <- paste0("https://iati.cloud/api/transactions/?provider_activity=", id, "&format=json&page_size=20&page=", page)
    request <- GET(url = path)
    response <- content(request, as = "text", encoding = "UTF-8")
    response <- fromJSON(response, flatten = TRUE) 
    new_data <- response$results 
  
  if (length(new_data) > 0) {
    new_data <- new_data %>% 
      mutate(funding_iati_id = id) %>% 
      unique()
  }
  
  transactions_dataset <- rbind(transactions_dataset, new_data)
  
  page <- page + 1
  y <- nrow(transactions_dataset)
  new_rows = y - x
  }
}



# 3) Keep partner activity IDs only (not duplicate gov funder ones) ----

ri_linked_activites <- transactions_dataset %>% 
  filter(iati_identifier != funding_iati_id) %>% 
  select(funding_iati_id,
         iati_id = iati_identifier) %>% 
  unique()


# Save to Rdata file
saveRDS(ri_linked_activites, file = "Outputs/ri_linked_activites.rds")
# ri_linked_activites <- readRDS(file = "Outputs/ri_linked_activites.rds")

# Clear environment
rm(ri_iati_activities, new_rows, page, path, id, x, y, request, response, new_data, transactions_dataset, 
   ri_linked_activites)
