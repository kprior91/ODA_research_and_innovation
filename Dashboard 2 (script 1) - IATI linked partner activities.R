# --------------------------------------------------------------- #
# Script 1
# Extract IATI partner activities linked to RED programmes/components
# by an incoming fund transaction activity ID
# --------------------------------------------------------------- #

if (!("jsonlite" %in% installed.packages())) {
  install.packages("jsonlite")
}
if (!("httr" %in% installed.packages())) {
  install.packages("httr")
}
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")
}
if (!("writexl" %in% installed.packages())) {
  install.packages("writexl")
}

library(jsonlite)
library(httr)
library(tidyverse)
library(readxl)
library(writexl)

###
# A) Create list of RED programmes and components -----

# Read in list of tagged RI RED components

fcdo_ri_programmes <- readRDS(file = "Outputs/fcdo_ri_programmes.rds")


###
# B) Extract all linked activities ----

transactions_dataset <- data.frame()

for (id in fcdo_ri_programmes$iati_identifier) {
  
  print(id)
  path <- paste0("https://iati.cloud/api/transactions/?provider_activity=", id, "&format=json&page_size=20")
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