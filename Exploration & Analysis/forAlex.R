# Selecting the first 10 rows for identifier "GB-GOV-7", see new_data dataframe
# numb_data gives me the number of rows that there should be in total (88) - is this a correct interpretation?

path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q=iati_identifier:"',
               "GB-GOV-7",
               '"&wt=json',
               "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs
numb_data <- response$response$numFound


# This is me trying to incorporate row/page cycling code and make it into the function

org_activity_extract <- function(page, org_code, blank_test_df) {
  rows = 10
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=iati_identifier:"',
                 org_code,
                 '"&wt=json',
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  # Condition to check when to stop
  if(start < numb_data) {
    blank_test_df <- c(blank_test_df, new_data)
  } else {
    blank_test_df <- blank_test_df
  }
  return(blank_test_df)
}

# This code gives me a list of 396, not sure how this relates to numb_data = 88 from above

blank_test_df <- c()
new_rows <- 0
page <- 1 # Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  message(page)
  x <- length(blank_test_df)
  blank_test_df <- org_activity_extract(page, "GB-GOV-7",blank_test_df)
  page <- page + 1
  y <- length(blank_test_df)
  new_rows = y - x
}