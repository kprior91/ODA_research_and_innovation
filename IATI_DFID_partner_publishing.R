# DFID partner IATI reporting

library(jsonlite)
library(httr)
library(tidyverse)
library(writexl)

# Read in IATI activities from separate project
activities_all <- readRDS(file = "activity_list_final.rds")


# 1) Extract list of DFID partners (for research activities) ----

# Unlist reporting department name
reporting_orgs <- activities_all %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text)


# Unlist implementing organisations
implementing_orgs <- activities_all %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, 
         org_type = type.name, org_iati_ref = ref,
         narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  rename(partner = text) %>% 
  unique() %>% 
  filter(role.name == "Implementing")


# Join unnested info to original data
activities_dfid <- activities_all %>% 
      left_join(reporting_orgs, by = "iati_identifier") %>%
      unique() %>% 
      filter(reporting_org == "UK Department for International Development") %>% 
      left_join(implementing_orgs, by = "iati_identifier")

# Keep necessary fields
dfid_partners <- activities_dfid %>% 
                   select(iati_identifier, org_iati_ref, partner, org_type) 

# How many unique partners listed?
partner_list <- dfid_partners %>% 
                   select(-iati_identifier) %>% 
                   unique() %>% 
                   filter(!is.na(partner))

partner_names <-  data.frame(partner_name = unique(dfid_partners$partner)) %>% 
                       filter(!is.na(partner_name))  

org_ref_list <-  data.frame(org_iati_ref = unique(dfid_partners$org_iati_ref)) %>% 
                   filter(!is.na(org_iati_ref) & org_iati_ref != "NULL")



# 2) Check if these organisations have registered on IATI ----

# Out of 710 DFID partners (based on recording of partner name), 165 have an IATI organisation
# identifier





# 3) Extract all these organisations' ODA activities ----

# Set strings for API URL
organisation_codes <- paste(org_ref_list$org_iati_ref, collapse=",")

# A. Function to extract research projects by OECD sector code
partner_activity_extract <- function(page) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?format=json&reporting_org_identifier=", organisation_codes, "&fields=iati_identifier,other_identifier,activity_date,reporting_org,location,default_flow_type,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  new_data <- response$results
  
  # Condition to check when 5-digit codes stop being returned
  if("default_flow_type.name" %in% names(new_data)) {
    new_data <- new_data %>% 
         mutate(default_flow_type = default_flow_type.name) %>% 
         select(-default_flow_type.name, -default_flow_type.code)
  } 
  
  partner_activity_list <- rbind(partner_activity_list_final, new_data)
  
  return(partner_activity_list)
}

# Prepare results data frame and counters
partner_activity_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(partner_activity_list_final)
  partner_activity_list_final <- partner_activity_extract(page)
  page <- page + 1
  y <- nrow(partner_activity_list_final)
  new_rows = y - x
}

