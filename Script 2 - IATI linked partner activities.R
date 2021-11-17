# --------------------------------------------------------------- #
# Script 1
# Extract IATI partner activities linked to RED programmes/components
# by an incoming fund transaction activity ID
# --------------------------------------------------------------- #

###
# A) Create list of RED programmes and components -----

# Read in list of tagged RI RED components

ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities.rds")


###
# B) Extract all linked activities ----

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
      mutate(programme_id = id) %>% 
      unique()
  }
  
  transactions_dataset <- rbind(transactions_dataset, new_data)
  
  page <- page + 1
  y <- nrow(transactions_dataset)
  new_rows = y - x
  }
}

rm(request)
rm(response)
rm(new_data)


###
# C) Remove FCDO duplicate activities ----

red_linked_activites <- transactions_dataset %>% 
  filter(iati_identifier != programme_id) %>% 
  select(programme_id,
         linked_activity = iati_identifier)

rm(transactions_dataset)

# Save to Rdata file
saveRDS(red_linked_activites, file = "Outputs/red_linked_activites.rds")
# Restore the object
# red_linked_activites <- readRDS(file = "Outputs/red_linked_activites.rds")