####
# Script to test individual organisation IATI data extracts
####

# Example Datastore API usage to fetch one IATI identifier
list.of.packages <- c("data.table", "httr", "jsonlite", "dotenv", "dplyr","openxlsx","tidyverse")
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

org_id <- "GB-GOV-1"


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


path <- paste0('https://api.iatistandard.org/datastore/activity/iati_json?',
               'q=iati_identifier:"',
               org_id,
               '"&wt=json',
               "&fl=iati_identifier,other_identifier,activity_date,reporting_org,sector_code,location,default_flow_type,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity,tag&facet=true&facet.field=iati_identifier&facet.limit=1000000")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
#response <- fromJSON(response, flatten = TRUE) 
#new_data <- response$response$docs
activity_iati_facets <- response$facet_counts$facet_fields$iati_identifier

activity_sector_facets = response$facet_counts$facet_fields$sector_code
activity_sector_counts = data.frame(
  sector_code=unlist(activity_sector_facets[c(TRUE, FALSE)]),
  activity_sector_count=unlist(activity_sector_facets[c(FALSE, TRUE)])
)


#this code collects IATI doc URLs####
REDprogs <- read.xlsx("C:/Users/KimPrior/OneDrive - FCDO/Documents/RED bits/Management Info/RED_programmes_all_March2023.xlsx", sheet = "allRED")

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



# Gets all FCDO activities from the IATI API (except it seems to miss some, need to work that out)

org_activity_extract <- function(page, org_code) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 org_code,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,participating_org*")
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

new_data_edit <- all_data

# This is now matching up (using grepl) the RED prog IDs teams gave me, because the API loses this (they are buried in the iati_identifier)

activity_id_char <- as.character(activity_id)
results <- character(nrow(new_data_edit))

for (activity in activity_id_char) {
  results[grepl(activity, new_data_edit$iati_identifier)] <- paste0(results[grepl(activity, new_data_edit$iati_identifier)], activity, collapse = NULL)
}
new_data_edit$RED_activity <- results
length(unique(new_data_edit$RED_activity))

# Subset the RED activities
new_data_edit_red <- new_data_edit[new_data_edit$RED_activity != "",]


# Some leftovers that don't seem to get picked up by my API query - shame! will have to run a separate one with these. solved for this exercise but not long term (or modari potentially)
# Actually all except "205219", the others were not active, i searched for each one on d portal

leftovers <- activity_id_char[!activity_id_char %in% unique(new_data_edit$RED_activity)]
length(leftovers) # this time 16, with 5 that should be on IATI

# This gets the IATI data which was missing from the initial API run

path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
               #make_qstring(activity_id_char),
               'q=iati_identifier:(%22GB-1-204867%22 %22GB-1-204931%22 %22GB-1-205053%22 %22GB-1-205121%22)',
               '&rows=1000',
               '&wt=json',
               "&fl=iati_identifier,participating_org*")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE)
new_data <- response$response$docs
numb_data <- response$response$numFound

# This is now matching up (using grepl) the leftover RED prog IDs, because the API loses this (they are buried in the iati_identifier)

results <- character(nrow(new_data))

for (activity in leftovers) {
  results[grepl(activity, new_data$iati_identifier)] <- paste0(results[grepl(activity, new_data$iati_identifier)], activity, collapse = NULL)
}
new_data$RED_activity <- results
new_data <- new_data %>% select(!c(participating_org_crs_channel_code,participating_org_narrative_xml_lang))

RED_data <- rbind(new_data_edit_red, new_data)

# I had a bit of trouble getting all the information through the API, so I've saved the initial API search for all the RED programmes
# Can then read this file back in to search the API for the partner data
#saveRDS(RED_data, file = "Outputs/RED_data_funder_060423.rds")

unique(RED_data$RED_activity)

# define a function to match up elements of the list between the IATI ID and the name of the partner

my_func <- function(id, x, y) {
  data.frame(id = id, result = paste(x, y, sep = ";"))
}

result <- mapply(my_func, RED_data$iati_identifier, RED_data$participating_org_ref, RED_data$participating_org_narrative, SIMPLIFY = FALSE)
result_df <- do.call(rbind, result)

# this produces a data frame for IATI ref and narrative
result_df <- separate(result_df, col = "result", into = c("org_ref","org_narrative"), sep = ";")


activity_id_char <- as.character(activity_id)
results <- character(nrow(result_df))

for (activity in activity_id_char) {
  results[grepl(activity, result_df$id)] <- paste0(results[grepl(activity, result_df$id)], activity, collapse = NULL)
}
result_df$prog_no <- results

# Comparing the list of programmes that have a delivery name/ID on IATI, versus the list of programmes RED sent me
# Programmes with partner named on IATI
length(unique(result_df$prog_no))#99
# Total number of RED programmes
length(activity_id_char)#111
# Programmes with no partner on IATI
length(activity_id_char[!activity_id_char %in% unique(result_df$prog_no)])#12

partner_orgs <- c(unlist(RED_data$participating_org_ref))
partner_orgs <- unique(partner_orgs)
partner_orgs <- gsub("GB-GOV-1","", partner_orgs)
partner_orgs <- gsub("46004","XM-DAC-46004", partner_orgs)
partner_orgs <- gsub("41122","XM-DAC-41122", partner_orgs)
partner_orgs <- unique(partner_orgs)
partner_orgs <- partner_orgs[nzchar(partner_orgs)]

#takes a vector of character strings (e.g. "GB-GOV-1-203067"), and returns one thing with %22 added before and 
#after each character string

make_qstring <- function(iati_name) {
  a <- sapply(iati_name, function(x){
    paste0("%22",x,"%22")
  })
  paste0("q=reporting_org_ref:(",paste(a, collapse = " "),")")
}

# make_qstring(partner_orgs)

# this is querying the IATI API using the function that adds %22 (make_qstring) to all the RED programme IDs
# this works, but there are more than 100,000 rows of data, far too much to pull into R

# path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
#                make_qstring(partner_orgs),
#                '&rows=1000',
#                '&wt=json',
#                "&fl=iati_identifier,description*")
# request <- GET(url = path, authentication)
# response <- content(request, as = "text", encoding = "UTF-8")
# response <- fromJSON(response, flatten = TRUE) 
# #new_data <- response$response$docs
# numb_data <- response$response$numFound

# now i'm trying to loop through each org but just return the top 10 rows each time. should work because i have 98 orgs
data_list <- list()
numb_list <- numeric()

for(i in partner_orgs){
  print(paste0("Processing ", i))
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 make_qstring(i),
                 '&rows=10',
                 '&wt=json',
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,hierarchy*, sector_code*,title*,description*,recipient_country_code,default_flow_type*,policy_marker*,transaction*,budget*,location*,activity_status*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE)
  data_list <- c(data_list, list(response$response$docs))
  num <- response$response$numFound
  numb_list <- c(numb_list, ifelse(length(num)==0, NA, num))
}

combined_data <- bind_rows(data_list)

# Partners not reporting to IATI
non_reporters <- partner_orgs[!partner_orgs %in% c(unique(combined_data$reporting_org_ref))]# either not registered, or signed up but no activities (do we need to distinguish these?)

# combined_data are the reporters, now need to see how much they are reporting

# Unlist activity title and description
RED_list_unnest_1 <- combined_data %>%
  select(reporting_org_ref, title_narrative, description_narrative) %>%
  mutate()
RED_list_YN_1 <- data.frame(reporting_org_ref = RED_list_unnest_1$reporting_org_ref,title = "Y",description = "Y")
RED_list_YN_1 <- RED_list_YN_1 %>% distinct(reporting_org_ref, .keep_all=TRUE)

# Unlist recipient countries
RED_list_unnest_2 <- combined_data %>%
  # title
  unnest(cols = recipient_country_code)  %>% 
  select(reporting_org_ref, recipient_country_code) %>% 
  unique() %>% 
  group_by(reporting_org_ref) %>%
  summarise(recipient_country = paste(coalesce(recipient_country_code, ""), collapse = ", ")) %>% 
  ungroup()
RED_list_YN_2 <- data.frame(reporting_org_ref = RED_list_unnest_2$reporting_org_ref,country = "Y")


# Unlist participating org
RED_list_unnest_3 <- combined_data %>%
  # title
  unnest(cols = participating_org_narrative)  %>% 
  select(reporting_org_ref, participating_org_narrative) %>% 
  unique() %>% 
  group_by(reporting_org_ref) %>%
  summarise(participating_org_narrative = paste(coalesce(participating_org_narrative, ""), collapse = ", ")) %>% 
  ungroup()
RED_list_YN_3 <- data.frame(reporting_org_ref = RED_list_unnest_3$reporting_org_ref,participating_orgs = "Y")


# Unlist budget
RED_list_unnest_4 <- combined_data %>%
  # title
  unnest(cols = budget_value)  %>% 
  select(reporting_org_ref,
         budget_value,
         budget_period_start_iso_date,
         budget_period_end_iso_date) %>%
  group_by(reporting_org_ref)

RED_list_YN_4 <- data.frame(reporting_org_ref = unique(RED_list_unnest_4$reporting_org_ref),budget = "Y",budget_start = "Y",budget_end = "Y")

# Unlist start/end dates
RED_list_unnest_5 <- combined_data %>%
  unnest(cols = activity_date_iso_date) %>%
  select(reporting_org_ref, activity_date_iso_date) %>%
  group_by(reporting_org_ref)






write.xlsx(new_data_edit,"Outputs/RED_IATIoutput.xlsx")



# Trying to get the country detail from the commitment/expenditure descriptions

path <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
               #make_qstring(activity_id_char),
               'q=iati_identifier:(GB-GOV-1-203067)',
               '&rows=1000',
               '&wt=json',
               "&fl=iati_identifier,description*,provider*,receiver*,country*")
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE)
new_data <- response$response$docs
numb_data <- response$response$numFound

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
