# --------------------------------------------------------------- #
# Script 2
# Extract IATI partner activities linked to UK gov funders' IATI activities
# by an incoming fund transaction
# --------------------------------------------------------------- #

# 1) Extract linked R&I partner IATI activities -----

# Read in list of tagged RI UK gov funder activities from Script 1
ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities_kp.rds")


transactions_activity_extract <- function(page, activity_batch) {
  
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

specific_transaction_extract <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- transactions_activity_extract(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

uk_activity_ids = unique(ri_iati_activities$iati_identifier)

batch_size = 10
batches = c()
current_batch = c()
for(i in 1:length(uk_activity_ids)){
  current_id = uk_activity_ids[i]
  if(i %% batch_size == 0){
    current_batch_str = paste0('("', paste(current_batch, collapse = '" OR "'), '")')
    batches = c(batches, current_batch_str)
    current_batch = c(current_id)
  } else {
    current_batch = c(current_batch, current_id)
  }
}

linked_transaction_extract <- lapply(batches, specific_transaction_extract)
linked_transaction_extract = rbindlist(linked_transaction_extract, fill=T)

linked_transaction_extract$transaction_provider_org_provider_activity_id <- as.character(linked_transaction_extract$transaction_provider_org_provider_activity_id)


# 3) Keep partner activity IDs only (not duplicate gov funder ones) ----

ri_linked_activites <- linked_transaction_extract %>% 
  filter(iati_identifier != transaction_provider_org_provider_activity_id) %>% 
  unique() %>% rename(activity_id = transaction_provider_org_provider_activity_id)


# Save to Rdata file
# saveRDS(ri_linked_activites, file = "Outputs/ri_linked_activites_kp.rds")
ri_linked_activites <- readRDS(file = "Outputs/ri_linked_activites_kp.rds")
#ri_linked_activites_ec <- readRDS(file = "Outputs/ri_linked_activites.rds")

# Clear environment
rm(ri_iati_activities, new_rows, page, path, id, x, y, request, response, new_data, transactions_dataset, 
   ri_linked_activites)
