transactions_activity_extract <- function(page, activity_batch) {
  
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
                 'q=transaction_provider_org_provider_activity_id:',
                 activity_batch,
                 '&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier")
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
