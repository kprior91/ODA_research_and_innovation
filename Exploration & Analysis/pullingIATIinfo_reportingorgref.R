
# Example Datastore API usage to fetch one IATI identifier
list.of.packages <- c("data.table", "httr", "jsonlite", "dotenv", "dplyr","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

API_KEY = "cb77cc509def49f9b37a00aefe5ee99f"
authentication = add_headers(`Ocp-Apim-Subscription-Key` = API_KEY)


# Selecting the first 10 rows for identifier "GB-GOV-7", see new_data dataframe
# numb_data gives me the number of rows that there should be in total (88) - is this a correct interpretation?

path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q=reporting_org_ref:"',
               "GB-GOV-1",
               '"&wt=json',
               "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs
numb_data <- response$response$numFound


# This is me trying to incorporate row/page cycling code and make it into the function

org_activity_extract <- function(page, org_code) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 org_code,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
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

page_list <- list()
page <- 1
page_df = data.frame()
while (!is.null(page_df)) {
  Sys.sleep(1)
  message(page)
  page_df <- org_activity_extract(page, "GB-GOV-1")
  if(!is.null(page_df)){
    page_list[[page]] = page_df
  }
  page = page + 1
}

all_data = rbindlist(page_list, fill=T)

setwd("C:/Users/KimPrior/OneDrive - FCDO/Documents/Git/ODA_research_and_innovation/Exploration & Analysis/IATI API testing")

write.xlsx(all_data,"FCDO_IATIoutput.xlsx")
