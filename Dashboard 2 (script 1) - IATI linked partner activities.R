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

# Read in list of RED programmes
red_programmes <- read_excel("Inputs/RED programme IDs.xlsx") %>% 
                        mutate(red_iati_id = if_else(ProjectID < 300000, 
                                                     paste0("GB-1-", ProjectID), paste0("GB-GOV-1-", ProjectID)))

# Extract components to RED programmes
component_list <- data.frame()

component_extract <- function(activity_id) {
  
  path <- paste0("https://iati.cloud/api/activities/?iati_identifier=", activity_id, "&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20")
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results
  
  if(length(new_data$related_activity) > 0) {
        new_data <- new_data %>% 
          unnest(related_activity) %>% 
          select(component_id = ref)
  } else {
        new_data <- data.frame()
  }
  
  return(new_data)

}


for (id in red_programmes$red_iati_id) {
  
  print(id)
  components <- component_extract(id)
  
  component_list <- rbind(component_list, components)
  
}

programme_list <- data.frame(component_id = red_programmes$red_iati_id)

all_red_activities <- rbind(programme_list, component_list)

# Remove intermediary datasets
rm(red_programmes)
rm(programme_list)
rm(component_list)
rm(components)
rm(id)


###
# B) Extract all linked activities ----

transactions_dataset <- data.frame()

for (id in all_red_activities$component_id) {
  
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