# --------------------------------------------------------------- #
# Script 2
# Extract IATI partner activities linked to UK gov funders' IATI activities
# by an incoming fund transaction
# --------------------------------------------------------------- #

# 1) Extract linked R&I partner IATI activities -----

# Read in list of tagged RI UK gov funder activities from Script 1
ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities_Jul24update.rds")
#ri_iati_activities_new <- setdiff(ri_iati_activities,ri_iati_activities_test)

uk_activity_ids = unique(ri_iati_activities$iati_identifier)
#uk_activity_ids_new = unique(ri_iati_activities_new$iati_identifier)

transactions_activity_extract <- function(page, activity_id) {
  
  rows = 1000
  start <- (page - 1) * rows
  path  <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
                  'q=transaction_provider_org_provider_activity_id:"',
                  activity_id,
                  '"&wt=json',
                  '&rows=',rows,
                  '&start=',start,
                  "&fl=iati_identifier,transaction_provider_org_provider_activity_id")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- unique(response$response$docs)
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  } 
  
  return(new_data)
}

specific_transaction_extract <- function(activity_id) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- transactions_activity_extract(page, activity_id)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

uk_activity_ids <- gsub(" ", "%", uk_activity_ids)
uk_activity_ids_new <- URLencode(uk_activity_ids_new)


linked_transaction_extract <- lapply(uk_activity_ids, specific_transaction_extract)
linked_transaction_extract = rbindlist(linked_transaction_extract, fill=T)
#linked_transaction_extract_new <- lapply(uk_activity_ids_new, specific_transaction_extract)
#linked_transaction_extract_new = rbindlist(linked_transaction_extract_new, fill=T)

# 3) Keep partner activity IDs only (not duplicate gov funder ones) ----

ri_linked_activites <- as.data.frame(linked_transaction_extract) %>% 
  filter(iati_identifier != transaction_provider_org_provider_activity_id) %>% 
  unique() %>% rename(activity_id = transaction_provider_org_provider_activity_id)

# ri_linked_activites_new <- as.data.frame(linked_transaction_extract_new) %>% 
#   filter(iati_identifier != transaction_provider_org_provider_activity_id) %>% 
#   unique() %>% rename(activity_id = transaction_provider_org_provider_activity_id)
# 
# ri_linked_activites <- rbind(ri_linked_activites,ri_linked_activites_new)

# Save to Rdata file
# saveRDS(ri_linked_activites, file = "Outputs/ri_linked_activites_Jan24update.rds")
ri_linked_activites <- readRDS(file = "Outputs/ri_linked_activites_Jan24update.rds")
# ri_linked_activites_ec <- readRDS(file = "Outputs/ri_linked_activites.rds")

# Clear environment
rm(ri_iati_activities, new_rows, page, path, id, x, y, request, response, new_data, transactions_dataset, 
   ri_linked_activites)