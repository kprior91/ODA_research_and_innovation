####
# Script to test individual organisation IATI data extracts
####

# check DFID linked activity

activity_id <- "GB-1-203166-103"

path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?iati_identifier=", activity_id, "&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20")
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$results


# check British Council linked activity

activity_id <- "GB-CHC-209131-A03172"

path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?iati_identifier=", activity_id, "&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20")
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$results


# Look for DFID linked transactions

activity_id <- "GB-1-203166-103"
activity_id <- "GB-1-203166"
org_id <- "GB-GOV-1"

path <- paste0("https://iatidatastore.iatistandard.org/api/transactions/?provider_activity=", activity_id, "&format=json&page_size=20")
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$results


# Look for aggregated DFID funds

path <- "https://iatidatastore.iatistandard.org/api/transactions/aggregations/?group_by=provider_org&aggregations=count,value&format=json&provider_org=GB-GOV-1"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$results


# Extract location data for British Council

path <- "https://iatidatastore.iatistandard.org/api/locations/?format=json&fields=id,description,ref,reporting_org&organisation_iati_identifier=GB-CHC-209131&page_size=20"
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
new_data <- response$results






