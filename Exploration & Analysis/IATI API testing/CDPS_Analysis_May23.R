###############################################################

# CDPS Analysis - May 2023 ####

###############################################################

# Example Datastore API usage to fetch one IATI identifier
list.of.packages <- c("data.table", "httr", "jsonlite", "dotenv", "dplyr","openxlsx","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

API_KEY = "cb77cc509def49f9b37a00aefe5ee99f"
authentication = add_headers(`Ocp-Apim-Subscription-Key` = API_KEY)

RED_data <- readRDS("Outputs/RED_data_funder_060423.rds")
REDprogs <- read.xlsx("C:/Users/KimPrior/OneDrive - FCDO/Documents/RED bits/Management Info/RED_programmes_all_March2023.xlsx", sheet = "allRED")

RED_activity_ids = unique(RED_data$iati_identifier)
#transaction_recipient_country_code

countrycode_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Country.csv")
regioncode_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Region.csv")




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

linked_transaction_extract <- lapply(batches, specific_transaction_extract)
linked_transaction_extract = rbindlist(linked_transaction_extract, fill=T)

linked_transaction_extract$transaction_provider_org_provider_activity_id <- as.character(linked_transaction_extract$transaction_provider_org_provider_activity_id)
linked_transaction_extract <- unique(linked_transaction_extract)

transaction_extracts <- linked_transaction_extract %>% 
  filter(iati_identifier != transaction_provider_org_provider_activity_id) %>% 
  unique() %>% rename(activity_id = transaction_provider_org_provider_activity_id)


# now grabbing all the transactions in the countries of the linked partners

transactions_activity_extract2 <- function(page, activity_batch) {
  
  # Reformat ID if it contains spaces (for API)
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
                 'q=iati_identifier:',
                 activity_batch,
                 '&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,recipient_country_code,participating_org_ref,transaction_ref,transaction_transaction_type_code,transaction_description_narrative,transaction_provider_org_type,transaction_provider_org_ref,transaction_receiver_org_type,transaction_receiver_org_ref,transaction_flow_type_code,participating_org_role, participating_org_narrative, transaction_provider_org,transaction_provider_org_provider_activity_id,transaction_recipient_country_code")
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

specific_transaction_extract2 <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- transactions_activity_extract2(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

partners_RED_ids <- unique(transaction_extracts$iati_identifier)

batch_size = 10
batches = c()
current_batch = c()
for(i in 1:length(partners_RED_ids)){
  current_id = partners_RED_ids[i]
  if(i %% batch_size == 0){
    current_batch_str = paste0('("', paste(current_batch, collapse = '" OR "'), '")')
    batches = c(batches, current_batch_str)
    current_batch = c(current_id)
  } else {
    current_batch = c(current_batch, current_id)
  }
}

linked_transaction_extract2 <- lapply(batches, specific_transaction_extract2)
linked_transaction_extract2 = rbindlist(linked_transaction_extract2, fill=T)

linked_transaction_extract2$transaction_provider_org_provider_activity_id <- as.character(linked_transaction_extract2$transaction_provider_org_provider_activity_id)
linked_transaction_extract2$transaction_provider_org_ref <- as.character(linked_transaction_extract2$transaction_provider_org_ref)
linked_transaction_extract2$transaction_transaction_type_code <- as.character(linked_transaction_extract2$transaction_transaction_type_code)
linked_transaction_extract2$iati_identifier <- as.character(linked_transaction_extract2$iati_identifier)

incoming_funders <- linked_transaction_extract2 %>%
  select(transaction_provider_org_ref,iati_identifier,transaction_transaction_type_code,transaction_provider_org_provider_activity_id) %>%
  filter(transaction_transaction_type_code == "1") %>%
  unique()



linked_transaction_extract2$recipient_country <- countrycode_list$name[match(linked_transaction_extract2$transaction_recipient_country_code,countrycode_list$code)]
linked_transaction_extract2 <- na.omit(linked_transaction_extract2)
linked_transaction_extract2$transaction_recipient_country_code <- as.character(linked_transaction_extract2$transaction_recipient_country_code)
linked_transaction_extract2 <- unique(linked_transaction_extract2)

linked_transaction_extract2_country <- linked_transaction_extract2 %>% select(-transaction_recipient_country_code) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))


RED_id <- RED_data %>% 
  # title
  select(iati_identifier, participating_org_ref) %>%
  unnest(cols = participating_org_ref) %>%
  filter(participating_org_ref != "GB-GOV-1")

linked_transaction_extract2_country$RED_id <- RED_id$iati_identifier[match(linked_transaction_extract2$iati_identifier,RED_id$participating_org_ref)]

# RED ACTIVITIES #####
activity_extract <- function(page, activity_batch) {
  
  # Reformat ID if it contains spaces (for API)
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=iati_identifier:',
                 activity_batch,
                 '&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,hierarchy,reporting_org_narrative,title_narrative,description_narrative,location_name_narrative,location_description_narrative,location_activity_description_narrative,activity_status_code,recipient_country_code,recipient_region_code,participating_org_role,participating_org_narrative,participating_org_ref")
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

specific_activity_extract <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- activity_extract(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

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

activity_extract <- lapply(batches, specific_activity_extract)
activity_extract = rbindlist(activity_extract, fill=T)



# 1) Unnest activity information -----------

# Extract basic activity information - hierarchy and status
gov_list_base <- activity_extract %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status_code) %>% 
  unique() %>% 
  mutate(activity_status = str_replace(activity_status, "1", "Pipeline/identification"),
         activity_status = str_replace(activity_status, "2", "Implementation"),
         activity_status = str_replace(activity_status, "3", "Finalisation"),
         activity_status = str_replace(activity_status, "4", "Closed"),
         activity_status = str_replace(activity_status, "5", "Cancelled"),
         activity_status = str_replace(activity_status, "6", "Suspended"))

# A) Unlist activity title and description
gov_list_unnest_1 <- activity_extract %>% 
  # title
  filter(lengths(title_narrative) != 0) %>%
  unnest(cols = title_narrative) %>% 
  #rename(activity_title = text) %>% 
  # description
  unnest(cols = description_narrative) %>% 
  #mutate(type.name = coalesce(type.name, "General")) %>% 
  select(iati_identifier, title_narrative, description_narrative) %>% 
  #unnest(cols = narrative) %>%     
  unique() %>% rename(activity_title = title_narrative, activity_description = description_narrative)


# B) Unlist recipient countries
gov_list_unnest_2_country <- activity_extract %>%
  #filter(lengths(recipient_country_code) != 0) %>%
  unnest(c(recipient_country_code)) %>% 
  select(iati_identifier, recipient_country_code)

gov_list_unnest_2_country$recipient_country <- countrycode_list$name[match(gov_list_unnest_2_country$recipient_country_code,countrycode_list$code)]
gov_list_unnest_2_country <- gov_list_unnest_2_country %>% select(-recipient_country_code) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))


gov_list_unnest_2_region <- activity_extract %>%
  #filter(lengths(recipient_country_code) != 0) %>%
  unnest(c(recipient_region_code)) %>% 
  select(iati_identifier, recipient_region_code)

gov_list_unnest_2_region$recipient_region <- regioncode_list$name[match(gov_list_unnest_2_region$recipient_region_code,regioncode_list$code)]
gov_list_unnest_2_region <- gov_list_unnest_2_region %>% select(-recipient_region_code) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(recipient_region = paste(coalesce(recipient_region, ""), collapse = ", "))

gov_list_unnest_2 <- full_join(gov_list_unnest_2_country, gov_list_unnest_2_region, by = "iati_identifier")
gov_list_unnest_2$recipient_country_region <- coalesce(gov_list_unnest_2$recipient_country, gov_list_unnest_2$recipient_region)
gov_list_unnest_2 <- gov_list_unnest_2 %>% 
  mutate(across(c(recipient_country_region), na_if, "Developing countries, unspecified")) %>%
  select(-recipient_country, -recipient_region)

gov_list_unnest_3 <- activity_extract %>%
  select(iati_identifier,location_name_narrative) %>%
  unnest(location_name_narrative) %>%
  unique()

# C) Unlist implementing organisations
gov_list_unnest_4 <- activity_extract %>% 
  #filter(lengths(participating_org_ref) != 0) %>%
  #unnest(c(participating_org_ref, participating_org_narrative)) %>% 
  select(iati_identifier, participating_org_role, participating_org_narrative, participating_org_ref) %>% 
  unnest(c(participating_org_role, participating_org_narrative, participating_org_ref)) %>% 
  filter(participating_org_role == "4") %>% 
  unique()

# Add country locations based on IATI org references or lookup
# (takes ~5 mins to run)
gov_list_unnest_4 <- gov_list_unnest_4 %>%
  # Extract 2 digit country code from org references (where populated)
  mutate(country_code = if_else((!is.na(participating_org_ref) & substr(participating_org_ref,3,3) == "-" & !(substr(participating_org_ref,1,2) %in% c("XI", "XM"))), 
                                substr(participating_org_ref,1,2), ""))

# Function searches the org title (e.g. Kenya) and returns a country if its present in the name
gov_list_unnest_4$country_name2 <- map(gov_list_unnest_4$participating_org_narrative, org_country_lookup)
gov_list_unnest_4$country_name2 <- as.character(gov_list_unnest_4$country_name2)
gov_list_unnest_4$country_name2 <- ifelse(gov_list_unnest_4$country_name2 == "NA", "", gov_list_unnest_4$country_name2)


# check if input is a valid 2-digit country code and then does a lookup to the countrycode_list
gov_list_unnest_4$country_name <- ifelse(is.na(gov_list_unnest_4$country_code) | nchar(gov_list_unnest_4$country_code) < 2, NA, countrycode_list$name[match(gov_list_unnest_4$country_code,countrycode_list$code)])
gov_list_unnest_4$country_name <- ifelse(is.na(gov_list_unnest_4$country_name), "", gov_list_unnest_4$country_name)

gov_list_unnest_4[gov_list_unnest_4 == ""] <- NA

# This needs fixing because currently I don't know why it's only picking one of the country names when there might be several 
gov_list_unnest_4$partner_country <- coalesce(gov_list_unnest_4$country_name, gov_list_unnest_4$country_name2)

gov_list_unnest_4 <- gov_list_unnest_4%>% select(-country_name, -country_name2)

# Summarise partner org countries and names
gov_list_unnest_4_countries <- gov_list_unnest_4 %>% 
  select(iati_identifier, partner_country) %>% 
  unique() %>% 
  filter(!is.na(partner_country)) %>% 
  group_by(iati_identifier) %>% 
  summarise(partner_country = paste(partner_country, collapse = ", "))

gov_list_unnest_4_partners <- gov_list_unnest_4 %>% 
  select(iati_identifier, participating_org_narrative) %>% 
  unique() %>% 
  filter(!is.na(participating_org_narrative)) %>% 
  group_by(iati_identifier) %>% 
  summarise(partner = paste(coalesce(participating_org_narrative, ""), collapse = ", ")) 

# Add partner name and country info to master dataset
gov_list_unnest_4 <- gov_list_unnest_4 %>% 
  select(-participating_org_narrative, -participating_org_ref, -country_code, -participating_org_role, -partner_country) %>% 
  left_join(gov_list_unnest_4_partners, by = "iati_identifier") %>% 
  left_join(gov_list_unnest_4_countries, by = "iati_identifier") %>%
  unique()


# Join unnested info to original data
activity_list <- gov_list_base %>% 
  left_join(gov_list_unnest_1, by = "iati_identifier") %>%
  left_join(gov_list_unnest_2, by = "iati_identifier") %>%
  left_join(gov_list_unnest_4, by = "iati_identifier")


activity_list$RED_activity <- RED_data$RED_activity[match(activity_list$iati_identifier,RED_data$iati_identifier)]

# Fixing some of the lookups

activity_list[70,9] <- "300504"
activity_list[121,9] <- "300180"
activity_list[140,9] <- "300632"
activity_list[202,9] <- "300849"
activity_list[329,9] <- "300484"
activity_list[427,9] <- "301132"
activity_list[592,9] <- "300100"
activity_list[604,9] <- "300123"


recip_list <- activity_list %>%
  select(RED_activity,recipient_country_region) %>%
  filter(str_detect(recipient_country_region, "South Africa|Kenya|Nigeria|Ghana|Ethiopia|Egypt|India|Pakistan|Indonesia|Philippines (the)|Philippines|Thailand|Viet Nam|Vietnam|Bangladesh|Nepal|Laos|Cambodia|Myanmar|Brazil|Chile|Mexico")) %>%
  unique()

location_list <- activity_list %>%
  select(RED_activity,partner_country) %>%
  filter(str_detect(partner_country, "South Africa|Kenya|Nigeria|Ghana|Ethiopia|Egypt|India|Pakistan|Indonesia|Philippines (the)|Philippines|Thailand|Viet Nam|Vietnam|Bangladesh|Nepal|Laos|Cambodia|Myanmar|Brazil|Chile|Mexico")) %>%
  unique()

write.xlsx(linked_transaction_extract2, file = "Exploration & Analysis/IATI API testing/transaction_country_REDApr23.xlsx")
write.xlsx(recip_list, file = "Exploration & Analysis/IATI API testing/country_regions_REDApr23.xlsx")
write.xlsx(location_list, file = "Exploration & Analysis/IATI API testing/partnerlocations_REDApr23.xlsx")
write.xlsx(gov_list_unnest_3, file = "benefittinglocation_name_REDApr23.xlsx")
