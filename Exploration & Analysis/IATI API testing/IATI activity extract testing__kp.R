####
# Script to test individual organisation IATI data extracts
####

# Example Datastore API usage to fetch one IATI identifier
list.of.packages <- c("data.table", "httr", "jsonlite", "dotenv", "dplyr","xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

API_KEY = "cb77cc509def49f9b37a00aefe5ee99f"
authentication = add_headers(`Ocp-Apim-Subscription-Key` = API_KEY)

activity_id_test <- "GB-1-203166"

# As flat JSON
path <- paste0(
  'https://api.iatistandard.org/datastore/activity/select?',
  'q=iati_identifier:"',
  activity_id_test,
  '"&wt=json',
  "&fl=other_identifier*,reporting_org*,location*,default_flow_type*,activity_date*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*"
)
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE)
new_data <- response$response$docs

# As hierarchical JSON
path <- paste0(
  'https://api.iatistandard.org/datastore/activity/iati_json?',
  'q=iati_identifier:"',activity_id,'"'
)
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE)
activity_json <- response$response$docs$`iati_json.iati-activity`[[1]]


# check British Council linked activity

activity_id <- "GB-CHC-209131-A03172"

# As flat JSON
path <- paste0(
  'https://api.iatistandard.org/datastore/activity/select?',
  'q=iati_identifier:"',
  activity_id,
  '"&wt=json',
  "&fl=other_identifier*,reporting_org*,location*,default_flow_type*,activity_date*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*"
)
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE)
new_data <- response$response$docs


# Look for DFID linked transactions

activity_id <- "GB-1-203166-103"
activity_id_dfid <- "GB-1-203166"
org_id <- "GB-GOV-1"

# As flat JSON
path <- paste0(
  'https://api.iatistandard.org/datastore/transaction/select?',
  'q=reporting_org_ref:"',
  activity_id_dfid,
  '"&wt=json'
  )
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE)
new_data <- response$response$docs


# Look for aggregated DFID funds
org_id <- "GB-GOV-1"

# As flat JSON
path <- paste0(
  'https://api.iatistandard.org/datastore/transaction/select?',
  'q=iati_identifier:"',
  org_id,
  '"&wt=json',
  "?group_by=provider_org*&aggregations=count"
)
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE)
new_data <- response$response$docs


# Extract location data for British Council

activity_id_BC <- "GB-CHC-209131"

# As flat JSON
path <- paste0(
  'https://api.iatistandard.org/datastore/activity/select?',
  'q=iati_identifier:"',
  activity_id_BC,
  '"&wt=json',
  "&fl=other_identifier*,reporting_org*,location*,description*"
)
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE)
new_data <- response$response$docs


path <- paste0(
  'https://api.iatistandard.org/datastore/activity/select?',
                 'q=iati_identifier:"',
                 org_id,
                 '"&wt=json',
                 "&fl=sector*"
                 )
  request <- GET(url = path, authentication)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE)
  new_data <- response$response$docs
  
  
  
  
# Function to extract IATI activity IDs for a specified org code

  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=iati_identifier:"',
                 org_id,
                 '"&wt=json&start=1&rows=1000',
                 "&fl=iati_identifier*,other_identifier*,activity_date*,reporting_org*,sector*,location*,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  

  org_activity_extract <- function(org_id, org_activity_list) {
    path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                   'q=iati_identifier:"',
                   org_id,
                   '"&wt=json',
                   "&fl=iati_identifier*,other_identifier*,activity_date*,reporting_org*,sector*,location*,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
    request <- GET(url = path, authentication)
    response <- content(request, as = "text", encoding = "UTF-8")
    response <- fromJSON(response, flatten = TRUE) 
    new_data <- response$response$docs
    
    # Ensure "default flow type" field exists for joining datasets
    if("default_flow_type.name" %in% names(new_data)) {
      new_data <- new_data %>% 
        mutate(default_flow_type = default_flow_type.name) %>% 
        select(-default_flow_type.name, -default_flow_type.code)
    } 
    
    results <- rbind(org_activity_list, new_data)
    
    return(results)
  }
  
  # Define UK government department IATI org IDs
  organisation_codes <- c("GB-GOV-1")
  
  # Prepare output data frame
  uk_gov_list_final <- data.frame()
  
  # Extract activity data for each government department
  for (org in organisation_codes) {
    uk_gov_list_final <- org_activity_extract(org, uk_gov_list_final)
  }
  
  

  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
  "q=*:*",
  '&wt=json',
  "&fl=sector_code*")
    request <- GET(url = path, authentication)
    response <- content(request, as = "text", encoding = "UTF-8")
    response <- fromJSON(response, flatten = TRUE) 
    new_data <- response$response$docs
    
    
recipient_country_code

org_id <- "GB-GOV"


path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q=iati_identifier:"',
               org_id,
               '"&wt=json',
               "&fl=sector_vocabulary,sector_vocabulary_uri,sector_narrative,sector_code")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs


results_list = list()
results_index = 1
docs = rep(0, 1000)
start_num = 0

path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q="GB-GOV"',
               '&wt=json',
               "&fl=sector_code*",
               "rows=1000&start="
               )

while(length(docs)==1000){
  message(start_num)
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs

new_data.df = rbindlist(new_data, fill=T)
new_data.df = new_data.df %>% mutate_all(function(x){
  x[which(sapply(x, length)==0)] = NA
  return(unlist(x))
})
results_index = results_index + 1
start_num = start_num + 1000

}


sector_extract <- function(page, sector_list) {
  path <- paste0("https://api.iatistandard.org/datastore/activity/select?",
  "q=*:*",
  "&wt=json&fl=sector_code*&page=", page)
  request <- GET(url = path, authentication)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # Condition to check when 5-digit codes stop being returned
  if(!("category" %in% names(response$results))) {
    sector_list <- rbind(sector_list, response$results)
  } else {
    sector_list <- sector_list
  }
  return(sector_list)
}



test_url = paste0(
  "https://api.iatistandard.org/datastore/activity/select?",
  "q=*:*",
  "&fl=*",
  "&wt=json",
  "&rows=0"
)

req = GET(
  URLencode(test_url),
  authentication
)
res = content(req)

available_columns = strsplit(res, split=",")[[1]]


path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q=iati_identifier:"',
               org_id,
               '"&wt=json',
               "&fl=iati_identifier,other_identifier,activity_date,reporting_org,sector_code,location,default_flow_type,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity,tag")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs


#this code collects IATI doc URLs####
REDprogs <- read.xlsx("C:/Users/k-prior/OneDrive - DFID/Documents/Onedrive spillover/RED/RED Management Information/RED_programmes_all_March2023.xlsx", sheetName = "allRED")

UK_code <- "GB-GOV-1-"
#activity_id <- c("300482")
#activity_id <- c("300482","300405")
activity_id <- unique(c(REDprogs$Programme.ID))
iati_id <- paste0(UK_code,activity_id[1])
iati_id <- paste0(iati_id, collapse=",")

#,document_link_url,result_document_link_url,result_indicator_document_link_url,result_indicator_baseline_document_link_url,result_indicator_period_target_document_link_url,result_indicator_period_actual_document_link_url

url_extract <- function(page, activity_list) {
  rows <- 1000
  start <- (page - 1) * rows
path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q=iati_identifier:',
               iati_id,
               '&wt=json',
               '&fl=iati_identifier,document_link_url&rows=', rows,
               '&start=', start)
request <- GET(url = path, authentication)
response <- content(request, encoding = "UTF-8")
numFound <- response$response$numFound
results <- unlist(response$response$docs)
# Condition to check when to stop
if(start < numFound) {
  activity_list <- c(activity_list, results)
} else {
    activity_list < activity_list
}
return(activity_list)
}

activity_list <- c()
new_rows <- 0
page <- 1
while(page ==1 | new_rows > 0) {
  message(page)
  x <- length(activity_list)
  activity_list <- url_extract(page, activity_list)
  page <- page + 1
  y <- length(activity_list)
  new_rows = y - x
}


#testing downloading urls
UK_code <- "GB-GOV-1-"
#activity_id <- c("300482")
#activity_id_a <- c("300482","300405")
activity_id <- unique(c(REDprogs$Programme.ID))
#iati_id_a <- paste0(UK_code,activity_id_a)
iati_id <- paste0(UK_code,activity_id)
#iati_id_a <- paste0(iati_id_a, collapse=",")
#iati_id <- paste0(iati_id, collapse=",")
#iati_id <- paste(c(iati_id), collapse = ', ')
#iati_id_apply <- sapply(strsplit(iati_id, '[, ]+'), function(x) toString(cat(dQuote(x, FALSE))))


path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q=iati_identifier:',
               iati_id,
               '&wt=json',
               "&fl=iati_identifier,document_link_url")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs




new_data_lookup <- new_data

urlfolder <- "Exploration & Analysis/IATI API testing/IATI url files"
#basename(urls)
pathforurl <- "C:/Users/k-prior/OneDrive - DFID/Documents/Git/ODA_research_and_innovation/Exploration & Analysis/IATI API testing/IATI url files/"

urls <- unlist(c(new_data$document_link_url))
for(url in urls){
  download.file(url, destfile = paste0(pathforurl,basename(url)))
}

path2 <- "https://api.iatistandard.org/datastore/activity/iati_json?q=iati_identifier:%22GB-GOV-1-201880,GB-GOV-1-300124%22&q.op=OR&sow=false&rows=20&fl=iati_identifier,document_link_url&wt=json&group=false&facet=false&facet.method=fc&facet.range.other=none"
path3 <- "https://api.iatistandard.org/datastore/activity/iati_json?q=iati_identifier:%22GB-GOV-1-201880,GB-GOV-1-300124%22&q.op=OR&sow=false&rows=20&fl=iati_identifier,document_link_url&wt=json"

request <- GET(url = path3, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs


#https://api.iatistandard.org/datastore/budget/select?q=reporting_org_type%3A22%20AND%20(recipient_country_code%3APH%20OR%20transaction_recipient_country_code%3APH)&rows=300&wt=json&fl=iati_identifier%2Creporting_org_type%2Crecipient_country_code

path <- paste0('https://api.iatistandard.org/datastore/budget/select?',
               'q=reporting_org_type:22 AND (recipient_country_code:PH OR transaction_recipient_country_code:PH)',
               '&rows=300',
               '&wt=json',
               "&fl=iati_identifier,reporting_org_type,recipient_country_code")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs


path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q=(sector_code:(11130 11230) OR transaction_sector_code:11130) AND (recipient_country_code:(UG KE) OR transaction_recipient_country_code:(UG KE)) AND activity_date_iso_date:[2021-01-01T00:00:00Z TO *]',
               '&rows=300',
               '&wt=json',
               "&fl=participating_org_ref,participating_org_narrative,sector_code")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs


path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               'q=iati_identifier:("GB-GOV-1-300482" "GB-GOV-1-300405")',
               '&rows=100',
               '&wt=json',
               "&fl=iati_identifier,document_link_url")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs





#takes a vector of character strings (e.g. "GB-GOV-1-203067"), and returns one thing with %22 added before and 
#after each character string

make_qstring <- function(iati_name) {
  a <- sapply(iati_name, function(x){
    paste0("%22",x,"%22")
  })
  paste0("q=iati_identifier:(",paste(a, collapse = " "),")")
}

make_qstring(iati_id)

# path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
#                'q=iati_identifier:("GB-GOV-1-301290")',
#                '&rows=100',
#                '&wt=json',
#                "&fl=iati_identifier,document_link_url")
# request <- GET(url = path, authentication)
# response <- content(request, as = "text", encoding = "UTF-8")
# response <- fromJSON(response, flatten = TRUE) 
# new_data <- response$response$docs
# numb_data <- response$response$numFound

#this is querying the IATI API using the function that adds %22 (make_qstring) to all the RED programme IDs

path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               make_qstring(iati_id),
               '&rows=1000',
               '&wt=json',
               "&fl=iati_identifier,document_link_url")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$response$docs
numb_data <- response$response$numFound

#this does TRUE/FALSE whether there are urls or not
data_lines <- !sapply(new_data$document_link_url, is.null)
#this then selects only the ids that have urls (it will automatically only select the values where it = TRUE)
new_data_lines <- new_data[data_lines,]

#this is how you select a single url
#new_data_lines[1,2][[1]][6]

#this function works out the length of the vector in the list for each iati id (i.e. the number of urls per iati id)
#this is because when i unlist the urls to download each file, i lose the iati id name associated with it
numb_url <- 
  sapply(new_data_lines[,2], function(x){
  length(unlist(x))
})

#this then uses rep to repeat the each id the same number of times as the number of urls associated with that id
prefix_id <- rep(new_data_lines[,1], numb_url)

#this sets the path to download the files in to
pathforurl <- "C:/Users/k-prior/OneDrive - DFID/Documents/Git/ODA_research_and_innovation/Exploration & Analysis/IATI API testing/IATI url files/"

#this then unlists the urls to download the files, but pastes the path and prefix iati id so i can identify the files
urls <- unlist(c(new_data_lines$document_link_url))
for(i in 1:length(urls[1:2])){
  download.file(urls[i], destfile = paste0(pathforurl,prefix_id[i],'_',basename(urls[i])))
}

# data_list <- list()
# numb_list <- numeric()



# Does not quite work. Lngth of data_list and numb_list differ. There might be a limit of 100 requests
# for(i in iati_id){
#   print(paste0("Processing ", i))
#   path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
#                  make_qstring(i),
#                  '&rows=1000',
#                  '&wt=json',
#                  "&fl=iati_identifier,document_link_url")
#   request <- GET(url = path, authentication)
#   response <- content(request, as = "text", encoding = "UTF-8")
#   response <- fromJSON(response, flatten = TRUE) 
#   data_list <- c(data_list, list(response$response$docs))
#   num <- response$response$numFound
#   numb_list <- c(numb_list, ifelse(length(num)==0, NA, num))
# }
# hist(numb_list)
# table(numb_list)
# iati_id[numb_list!=0]
# data_list[numb_list!=0]
# data_list[[107]]
# length(data_list)
# length(numb_list)
# iati_id[1]
# data_list[1]
# data_list[[2]]
