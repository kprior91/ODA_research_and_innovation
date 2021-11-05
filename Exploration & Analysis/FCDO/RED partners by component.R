
#####################################################
# Summarise RED delivery chain data from AMP and IATI
#####################################################

library(readxl)
library(writexl)
library(tidyverse)
library(jsonlite)
library(httr)

# Manually read in list of RED programme with delivery partners from AMP
red_programmes_amp <- read_excel("Inputs/RED programme delivery partners (from SQL).xlsx") 

# 1) Summarise AMP delivery chain info by RED programme ID

# Collate countries for each programme and last update date on AMP
red_programmes_amp_countries <- red_programmes_amp %>% 
  select(ProjectID, Country, LastUpdate) %>% 
  group_by(ProjectID) %>% 
  summarise(Countries = paste(Country, collapse = ", "),
            LastUpdate = max(LastUpdate, na.rm = TRUE)) %>% 
  mutate(Countries = str_replace_all(Countries, "NA", ""))

# Summarise delivery partners by tier
red_programmes_amp_summarised <- red_programmes_amp %>% 
  select(-ChildID, -Country, -LastUpdate, -DivisionName) %>% 
  unique() %>% 
  group_by(ProjectID, Title, StageDescription, TierLevel) %>% 
  summarise(Partners = paste(coalesce(PartnerName, ""), collapse = ", ")) %>% 
  spread(TierLevel, Partners) %>% 
  select(1:9) %>% 
  left_join(red_programmes_amp_countries, by = "ProjectID") %>% 
  select(-Countries)


# 2) Complement with first-tier partner info from IATI ---------

# Add "-" character to allow ID to be searched in IATI identifier string
red_programmes <- red_programmes_amp %>% 
  select(ProjectID) %>% 
  unique() %>% 
  mutate(programme_id = as.character(ProjectID),
         IATI_ID = if_else(ProjectID < 300000, paste0("GB-1-", programme_id), paste0("GB-GOV-1-", programme_id))) %>% 
  select(-programme_id)


# Define function to use IATI activity API search
activity_extract <- function(programme_id) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?iati_identifier=", programme_id, "&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20")
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # Condition to check when 5-digit codes stop being returned
  # if(!("category" %in% names(response$results))) {
  activity_list <- rbind(activity_list_final, response$results)
  # } else {
  #   sector_list <- sector_list_final
  # }
  return(activity_list)
}

# Prepare empty activity data frame 
activity_list_final <- data.frame()

# Run extraction over all RED programme IDs
for (programme_id in red_programmes$IATI_ID) {
  
  print(programme_id)
  activity_list_final <- activity_extract(programme_id)
  
}

saveRDS(activity_list_final, "Outputs/activity_list_final.rds")
#activity_list_final <- readRDS("Outputs/activity_list_final.rds")


# Unlist implementing organisations
implementing_orgs <- activity_list_final %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, ref, role.name, narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name == "Implementing") %>% 
  mutate(fcdo_programme_id = as.numeric(substr(str_replace_all(iati_identifier, "GB-1-|GB-GOV-1-", ""), 1, 6))) %>%
  select(-iati_identifier, -ref) %>% 
  unique() 


implementing_org_by_programme <- implementing_orgs %>% 
  group_by(fcdo_programme_id) %>%
  summarise(implementers = paste(coalesce(text, ""), collapse = ", ")) %>% 
  select(fcdo_programme_id, implementers)


# 3) Join IATI partner info to AMP programme info -------

red_programmes_final <- red_programmes_amp_summarised %>% 
  left_join(implementing_org_by_programme, by = c("ProjectID" = "fcdo_programme_id")) %>% 
  ungroup() %>% 
  select(ProgrammeID = ProjectID,
         Title,
         LastUpdate,
         IATI = implementers,
         Tier1_AMP = '1',
         Tier2_AMP = '2',
         Tier3_AMP = '3',
         Tier4_AMP = '4',
         Tier5_AMP = '5',
         Tier6_AMP = '6')


# 4) Write to Excel ---------

write_xlsx(red_programmes_final, "Outputs/RED programmes with IATI and AMP delivery chain data.xlsx")
