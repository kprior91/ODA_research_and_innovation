list.of.packages <- c("data.table", "httr", "jsonlite", "dotenv", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

API_KEY = "cb77cc509def49f9b37a00aefe5ee99f"
authentication = add_headers(`Ocp-Apim-Subscription-Key` = API_KEY)


# Below is the original function

org_activity_extract <- function(page, org_code) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 org_code,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,recipient_country_code,recipient_region_code,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
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

# Below function works to separate out the participating org narratives

org_activity_extract <- function(page, org_code) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 org_code,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,recipient_country_code,recipient_region_code,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  }
  flat_data_list = list() 
  flat_data_index = 1
  # Define columns where there will only ever be one per org
  single_org_cols
  for(i in 1:nrow(new_data)){
    row = new_data[i,]
    participating_org_cols = which(startsWith(names(new_data),"participating_org"))
    participating_org_data = row[,participating_org_cols]
    row[,participating_org_cols] = NULL
    for(j in 1:length(participating_org_data[1,1][[1]])){
      org_row = row
      for(k in 1:length(participating_org_cols)){
        if(!is.null(participating_org_data[1,k][[1]])){
          org_row[1,names(participating_org_data)[k]] = participating_org_data[1,k][[1]][j]
        }
      }
      flat_data_list[[flat_data_index]] = org_row
      flat_data_index = flat_data_index + 1
    }
  }
  return(rbindlist(flat_data_list, fill=T))
  
}

org_code <- c(
  "XM-DAC-47015", # CGIAR
  "XM-DAC-301-2", # IDRC
  "DAC-1601",     # Bill & Melinda Gates Foundation
  "XI-IATI-AGR"   # AgResults (Consortium)
)

org_code <- "XI-IATI-AGR"
org_code <- "XM-DAC-47015"

# 1) Activity extract

specific_org_extract <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- org_activity_extract(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

org_activity_list <- lapply(org_code, specific_org_extract)
org_activity_list_all = rbindlist(org_activity_list, fill=T)


activity_list_test <- org_activity_list_all %>%
  #filter(lengths(participating_org_ref) != 0) %>%
  #unnest(c(participating_org_ref, participating_org_narrative)) %>%
  select(iati_identifier, participating_org_role, participating_org_narrative, participating_org_ref) %>%
  unnest(c(participating_org_role, participating_org_narrative, participating_org_ref)) %>%
  filter(participating_org_role == "4") %>%
  unique()



########DEBUGGING################################################
IDRC <- "XM-DAC-301-2"

org_activity_extract_IDRC <- function(page, IDRC) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 IDRC,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,recipient_country_code,recipient_region_code,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  }
  flat_data_list = list() 
  flat_data_index = 1
  for(i in 1:nrow(new_data)){
    row = new_data[i,]
    participating_org_cols = which(startsWith(names(new_data),"participating_org"))
    participating_org_data = row[,participating_org_cols]
    row[,participating_org_cols] = NULL
    for(j in 1:length(participating_org_data[1,1][[1]])){
      org_row = row
      for(k in 1:length(participating_org_cols)){
        if(!is.null(participating_org_data[1,k][[1]])){
          org_row[1,names(participating_org_data)[k]] = participating_org_data[1,k][[1]][j]
        }
      }
      flat_data_list[[flat_data_index]] = org_row
      flat_data_index = flat_data_index + 1
    }
  }
  return(rbindlist(flat_data_list, fill=T))
  
}

specific_org_extract_IDRC <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- org_activity_extract_IDRC(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

org_activity_list_IDRC <- lapply(IDRC, specific_org_extract_IDRC)
org_activity_list_IDRC = rbindlist(org_activity_list_IDRC, fill=T)
org_activity_list_IDRC <- org_activity_list_IDRC %>% filter(participating_org_narrative_xml_lang == "en")

org_activity_list_IDRC$participating_org_ref <- ifelse(org_activity_list_IDRC$participating_org_narrative %in% c("International Development Research Centre","Canada. Parliament"), "XM-DAC-301-2", "")
org_activity_list_IDRC$participating_org_ref <- ifelse(org_activity_list_IDRC$participating_org_narrative %in% c("Bill & Melinda Gates Foundation"), "DAC-1601", org_activity_list_IDRC$participating_org_ref)
org_activity_list_IDRC$participating_org_ref <- ifelse(org_activity_list_IDRC$participating_org_narrative %in% c("Norwegian Agency for Development Cooperation"), "NO-BRC-971277882", org_activity_list_IDRC$participating_org_ref)


org_activity_list_test <- plyr::rbind.fill(org_activity_list,org_activity_list_IDRC)

partner_activities_via_title_test <- org_activity_list_test %>%
  filter(reporting_org_ref == "DAC-1601") %>%  # Gates org ID
  filter(str_detect(title_narrative, "FCDO|DFID")) %>%
  mutate(gov_funder = "Foreign, Commonwealth and Development Office",
         fund = "FCDO Research - Partnerships") %>%
  select(iati_identifier, gov_funder, fund) %>%
  unique()

# 2.B) Identify UK gov funded activities from participating organisations

# i think below is including things it shouldn't be? because the participating_org_ref appears to be wrong

# partner_activities_via_funder2_test <- org_activity_list_test %>%
#   # restrict to UK gov funding
#   select(iati_identifier, participating_org_role, participating_org_narrative, participating_org_ref, participating_org_activity_id) %>%
#   unnest(cols = c(participating_org_role, participating_org_narrative, participating_org_ref)) %>%
#   filter(str_detect(participating_org_role, "1")) %>%
#   filter(str_detect(participating_org_ref, "GB-GOV-1") |
#            str_detect(participating_org_narrative, "Britain|DFID|FCDO|DHSC|Department of Health and Social Care") |
#            str_detect(iati_identifier, "DFID|FCDO") |
#            str_detect(iati_identifier, "XI-IATI-AGR")      # AgResults partially funded
#   )

# below does not use "GB-GOV-1" to search for programmes but includes more words to search the narratives

partner_activities_via_funder2_test <- org_activity_list_test %>%
  # restrict to UK gov funding
  select(iati_identifier, participating_org_role, participating_org_narrative, participating_org_ref, participating_org_activity_id) %>%
  unnest(cols = c(participating_org_role, participating_org_narrative, participating_org_ref)) %>%
  filter(str_detect(participating_org_role, "1")) %>%
  filter(str_detect(participating_org_narrative, "Britain|DFID|Department for International Development|Foreign, Commonwealth|FCDO|DHSC|Department of Health and Social Care") |
           str_detect(iati_identifier, "DFID|FCDO") |
           str_detect(iati_identifier, "XI-IATI-AGR")      # AgResults partially funded
  )

partner_activities_via_funder2_test <- partner_activities_via_funder2_test %>%
  mutate(gov_funder = if_else(str_detect(participating_org_narrative, "Health"), "Department of Health and Social Care",
                              "Foreign, Commonwealth and Development Office"),
         fund = case_when(
           # IDRC GAMRIF projects
           str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(participating_org_narrative, "Health") ~ "DHSC - Global Health Security - GAMRIF",
           # Other DHSC partnerships
           str_detect(participating_org_narrative, "Health") ~ "DHSC - Global Health Research - Partnerships",
           # FCDO funding
           TRUE ~ "FCDO Research - Partnerships"
         )) %>%
  select(iati_identifier, gov_funder, fund) %>%
  unique()


partnership_activities_test <- plyr::rbind.fill(partner_activities_via_title_test, partner_activities_via_funder2_test)

partnership_activities_test <- org_activity_list_test %>%
  inner_join(partnership_activities_test, by = "iati_identifier")

partner_activity_comb_test <- plyr::rbind.fill(as.data.frame(partner_activity_extract), as.data.frame(partnership_activities_test)) %>% 
  filter(default_flow_type_code == "10" | is.na(default_flow_type_code))


activity_list_unnest_4_test <- partner_activity_comb_test %>%
  #filter(lengths(participating_org_ref) != 0) %>%
  #unnest(c(participating_org_ref, participating_org_narrative)) %>%
  select(iati_identifier, participating_org_role, participating_org_narrative, participating_org_ref) %>%
  unnest(c(participating_org_role, participating_org_narrative, participating_org_ref)) %>%
  filter(participating_org_role == "4") %>%
  unique()

activity_list_unnest_4_test <- activity_list_unnest_4_test %>%
  # Extract 2 digit country code from org references (where populated)
  mutate(country_code = if_else((!is.na(participating_org_ref) & substr(participating_org_ref,3,3) == "-" & !(substr(participating_org_ref,1,2) %in% c("XI", "XM"))), 
                                substr(participating_org_ref,1,2), ""))

# Function searches the org title (e.g. Kenya) and returns a country if its present in the name
activity_list_unnest_4_test$country_name2 <- map(activity_list_unnest_4_test$participating_org_narrative, org_country_lookup)
activity_list_unnest_4_test$country_name2 <- as.character(activity_list_unnest_4_test$country_name2)
activity_list_unnest_4_test$country_name2 <- ifelse(activity_list_unnest_4_test$country_name2 == "NA", "", activity_list_unnest_4_test$country_name2)