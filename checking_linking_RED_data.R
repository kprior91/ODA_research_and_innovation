###checking whether RED data is correctly linked on IATI####
#K Prior

# Example Datastore API usage to fetch one IATI identifier
list.of.packages <- c("data.table", "httr", "jsonlite", "dotenv", "dplyr","openxlsx","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

API_KEY = "cb77cc509def49f9b37a00aefe5ee99f"
authentication = add_headers(`Ocp-Apim-Subscription-Key` = API_KEY)


RED_data <- readRDS("Data checks/Inputs/RED_data_funder_060423.rds")

# function that takes RED activity IDs and then looks for them in transactions of other activities
# this gets the partners that link back to RED

transactions_RED_extract <- function(page, activity_batch) {
  
  # Reformat ID if it contains spaces (for API)
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
                 'q=transaction_provider_org_provider_activity_id:',
                 activity_batch,
                 '&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,transaction_provider_org_provider_activity_id")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  } 
  
  return(new_data)
}

RED_transaction_extract <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- transactions_RED_extract(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

RED_activity_ids = unique(RED_data$iati_identifier)

batch_size = 10
batches = c()
current_batch = c()
for(i in 1:length(RED_activity_ids)){
  current_id = RED_activity_ids[i]
  if(i %% batch_size == 0){
    current_batch_str = paste0('("', paste(current_batch, collapse = '" OR "'), '")')
    batches = c(batches, current_batch_str)
    current_batch = c(current_id)
  } else {
    current_batch = c(current_batch, current_id)
  }
}

REDs_transaction_extract <- lapply(batches, RED_transaction_extract)
REDs_transaction_extract = rbindlist(REDs_transaction_extract, fill=T)

REDs_transaction_extract$transaction_provider_org_provider_activity_id <- as.character(REDs_transaction_extract$transaction_provider_org_provider_activity_id)


# 3) Keep partner activity IDs only (not duplicate gov funder ones) ----

RED_linked_activites <- REDs_transaction_extract %>% 
  filter(iati_identifier != transaction_provider_org_provider_activity_id) %>% 
  unique() %>% rename(activity_id = transaction_provider_org_provider_activity_id)


RED_list <- as.data.frame(unique(RED_linked_activites$activity_id))

# filter the RED data to get the implementing partner IDs submitted by the RED side

FCDO_RED_list <- RED_data %>% 
  select(iati_identifier, participating_org_ref, participating_org_role) %>%
  unnest(c(participating_org_ref,participating_org_role)) %>%
  filter(participating_org_role == "4") %>%
  select(-participating_org_role) %>%
  unique()


# How to check for non-linking FCDO programmes:
# Get the "FCDO_RED_list" dataset with the iati_identifier col of RED programme IDs (as on IATI).
# Using the "RED_linked_activities" dataset do a lookup into the "FCDO_RED_list" dataset to get 
# the partner activity ID (under the column iati_identifier). Then get the "ri_linked_activities" dataset,
# doing a lookup into "FCDO_RED_list" dataset to get the activity id of the linked partners (under the 
# column iati_identifier). Filter out the NA's from the column C (which came from "RED_linked_activities"), 
# and filter for the NA's in column D (which came from "ri_linked_activities"). Double check these on IATI 
# and then add to the "IATI partner activities_kp" document.

write.xlsx(as.data.frame(RED_linked_activites),"Data checks/Outputs/RED_linked_activities.xlsx")
write.xlsx(FCDO_RED_list,"Data checks/Outputs/FCDO_RED_list.xlsx")
write.xlsx(ri_linked_activites,"Data checks/Outputs/ri_linked_activites.xlsx")