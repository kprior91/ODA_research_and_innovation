############
# Growth team projects - return all linked partner activities
############

growth_programmes <- c("GB-1-202168", "GB-1-202168-101", "GB-1-202168-103", "GB-1-202168-104", "GB-1-202168-105", "GB-1-202168-106",
                       "GB-1-202568", "GB-1-202568-101", "GB-1-202568-102", "GB-1-202568-104",
                       "GB-1-202835", "GB-1-202835-101", "GB-1-202835-104", "GB-1-202835-105",
                       "GB-1-202960", "GB-1-202960-102", "GB-1-202960-103", "GB-1-202960-104", "GB-1-202960-105",
                       "GB-1-203048", "GB-1-203048-101", "GB-1-203048-103", "GB-1-203048-104", "GB-1-203048-105", "GB-1-203048-106", "GB-1-203048-107",
                       "GB-1-203050", "GB-1-203050-101", "GB-1-203050-102", "GB-1-203050-103",
                       "GB-1-203051", "GB-1-203051-101", "GB-1-203051-103", "GB-1-203051-104",
                       "GB-1-203212", "GB-1-203212-101", "GB-1-203212-102", "GB-1-203212-103",
                       "GB-1-203844", "GB-1-203844-102", "GB-1-203844-103", "GB-1-203844-104",
                       "GB-1-204153", "GB-1-204153-101", "GB-1-204153-104", "GB-1-204153-105", "GB-1-204153-109", "GB-1-204153-110", "GB-1-204153-111", "GB-1-204153-112",
                       "GB-1-204773", "GB-1-204773-102", "GB-1-204773-103", "GB-1-204773-104", 
                       "GB-1-204931", "GB-1-204931-102",
                       "GB-GOV-1-300656", "GB-GOV-1-300656-101")


red_programmes <- read_excel("Inputs/RED programme IDs.xlsx") %>% 
                        mutate(red_iati_id_1 = paste0("GB-1-", ProjectID),
                               red_iati_id_2 = paste0("GB-GOV-1-", ProjectID))

red_ids <- c(red_programmes$red_iati_id_1, red_programmes$red_iati_id_2)

transactions_dataset <- data.frame()

# function to extract transactions linked to an activity

for (id in growth_programmes) {
  
  print(id)
  path <- paste0("https://iatidatastore.iatistandard.org/api/transactions/?provider_activity=", id, "&format=json&page_size=20")
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


for (id in red_ids) {
  
  print(id)
  path <- paste0("https://iatidatastore.iatistandard.org/api/transactions/?provider_activity=", id, "&format=json&page_size=20")
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

# Remove FCDO duplicate activities

transactions_dataset <- transactions_dataset %>% 
  filter(iati_identifier != programme_id) %>% 
  select(programme_id,
         linked_activity = iati_identifier)
