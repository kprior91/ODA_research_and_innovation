# --------------------------------------------------------------- #
# Script 2
# Extract ODA research activities from public IATI data #
# --------------------------------------------------------------- #

if (!("jsonlite" %in% installed.packages())) {
  install.packages("jsonlite")
}
if (!("httr" %in% installed.packages())) {
  install.packages("httr")
}
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")
}
if (!("writexl" %in% installed.packages())) {
  install.packages("writexl")
}

library(jsonlite)
library(httr)
library(tidyverse)
library(readxl)
library(writexl)


### A) Activity extract by ID (manually identified) ---

# 1) Read in partner activity IATI list 
iati_activity_ids <- read_xlsx("Inputs/IATI partner activities.xlsx", sheet=1)

# 2) Combine with linked activities from script 1
red_linked_activites <- readRDS(file = "Outputs/red_linked_activites.rds") %>% 
  rename(iati_id = linked_activity,
         funding_iati_id = programme_id)

iati_activity_ids <- iati_activity_ids %>% 
  plyr::rbind.fill(red_linked_activites) %>% 
  # replace spaces with %20 so that they don't cause errors
  mutate(iati_id = str_replace_all(iati_id, " ", "%20"))

# 3) Extract specified partner activities from IATI Registry 
iati_activity_extract <- function(activity_id) {
  
  path <- paste0("https://iati.cloud/api/activities/?iati_identifier=", activity_id, "&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20")
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results
  
  # Ensure "default flow type" field exists for joining datasets
  if("default_flow_type.name" %in% names(new_data)) {
    new_data <- new_data %>% 
      mutate(default_flow_type = default_flow_type.name) %>% 
      select(-default_flow_type.name, -default_flow_type.code)
  } 
  
  return(new_data)
}

# Prepare results data frame and counters
partner_activity_extract <- data.frame()

# Run extraction, stopping when no new sector codes returned
for (id in iati_activity_ids$iati_id) {

    print(id)
    result <- iati_activity_extract(id)
    partner_activity_extract <- rbind(partner_activity_extract, result)

}

### B) Activity extract for specific partner organisations

# CGIAR, IDRC FCDO-funded activities 

org_code <- c(
              "XM-DAC-47015", # CGIAR
              "XM-DAC-301-2", # IDRC
              "XI-IATI-CABI", # CABI
              "XM-DAC-928",   # WHO
              "DAC-1601",     # Bill & Melinda Gates Foundation
              "XI-IATI-AGR",    # AgResults (Consortium)
              )   

# 1) Activity extract

# Function to extract large partner activities
org_activity_extract <- function(page, org_code) {
  path <- paste0("https://iati.cloud/api/activities/?format=json&reporting_org_identifier=", org_code, "&fields=iati_identifier,other_identifier,activity_date,reporting_org,sector,location,default_flow_type,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity,tag&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results
  
  # Ensure "default flow type" field exists for joining datasets
  if("default_flow_type.name" %in% names(new_data)) {
    new_data <- new_data %>% 
      mutate(default_flow_type = default_flow_type.name) %>% 
      select(-default_flow_type.name, -default_flow_type.code)
  } 
  
  results <- rbind(org_activity_list, new_data)
  
  return(results)
}

# Prepare results data frame and counters
org_activity_list <- data.frame()
new_rows <- 0
page <- 1

# Run extraction

for (org in org_code) {
  new_rows <- 0
  page <- 1
  
  while (page == 1 | new_rows > 0) {
    print(paste0(org, "-", page))
    x <- nrow(org_activity_list)
    org_activity_list <- org_activity_extract(page, org)
    page <- page + 1
    y <- nrow(org_activity_list)
    new_rows = y - x
  }
}

# 2.A) Unlist activity titles and subset for those that mention FCDO/DFID
# (Gates only)

partner_activities_via_title <- org_activity_list %>% 
  filter(reporting_org.ref == "DAC-1601") %>% 
  unnest(cols = title.narrative,
         keep_empty = TRUE) %>% 
  filter(str_detect(text, "FCDO|DFID")) %>% 
  mutate(gov_funder = "Foreign, Commonwealth and Development Office",
         fund = "FCDO fully funded") %>% 
  select(iati_identifier, gov_funder, fund) %>% 
  unique()

# 2.B) Unlist participating funding organisations and subset for FCDO

partner_activities_via_funder <- org_activity_list %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative, ref, activity_id) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name %in% c("Funding") | 
           str_detect(iati_identifier, "XI-IATI-AGR|Windows1and2") 
           ) %>%   
  unique() %>% 
  filter(ref == "GB-GOV-1" | 
           str_detect(text, "Britain|DFID|FCDO|DHSC|Department of Health and Social Care") |
           str_detect(iati_identifier, "DFID") |
           str_detect(iati_identifier, "Windows1and2") |             # CGIAR partially funded
           str_detect(iati_identifier, "XI-IATI-AGR")      # AgResults partially funded
         ) %>%   
  mutate(gov_funder = if_else(str_detect(text, "Health"), "Department of Health and Social Care",
                              "Foreign, Commonwealth and Development Office"),
         fund = if_else(str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(text, "Health"),
                        "Global Health Security - GAMRIF",  # IDRC GAMRIF projects
                   if_else(str_detect(text, "Health"), "Global Health Research - Partnerships", 
                           if_else(str_detect(iati_identifier, "XI-IATI-AGR|Windows1and2"),
                                   "FCDO partially funded", "FCDO fully funded")))) %>% 
  select(iati_identifier, activity_id, gov_funder, fund) %>% 
  unique()

# Combine 2A and 2B
partner_activities <- plyr::rbind.fill(partner_activities_via_title, partner_activities_via_funder)

# Join back to original data
partner_activities <- org_activity_list %>% 
  inner_join(partner_activities, by = "iati_identifier")

# Save to Rdata file
saveRDS(partner_activities, file = "Outputs/partner_activities.rds")
# Restore the object
# partner_activities <- readRDS(file = "Outputs/partner_activities.rds")


### C) Combine individual with partner activities (extractions A and B above) ---- 

partner_activity_comb <- plyr::rbind.fill(partner_activity_extract, partner_activities) %>% 
  rename(fcdo_activity_id = activity_id)

rm(partner_activity_extract)
rm(partner_activities_via_title)
rm(partner_activities_via_funder)
rm(partner_activities)


# D) Extract accompanying data ----------------------------------------------

# Extract base activity information - hierarchy and status
activity_list_base <- partner_activity_comb %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         flow_type = default_flow_type,
         fcdo_activity_id,
         gov_funder,
         fund) %>% 
  left_join(select(iati_activity_ids, iati_id, gov_funder), by = c("iati_identifier" = "iati_id")) %>% 
  mutate(gov_funder = coalesce(gov_funder.x, gov_funder.y)) %>% 
  select(-gov_funder.x, -gov_funder.y) %>% 
  unique()


# a) Unlist activity title and description
activity_list_unnest_1 <- partner_activity_comb %>% 
  unnest(cols = title.narrative,
         keep_empty = TRUE) %>% 
  filter(lang.name == "English") %>% 
  select(-lang.code, -lang.name) %>% 
  rename(activity_title = text) %>% 
  unnest(cols = description,
         keep_empty = TRUE) %>% 
  mutate(type.name = coalesce(type.name, "General")) %>% 
  select(iati_identifier, activity_title, type.name, narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>%  
  filter(lang.name == "English") %>% 
  unique()


# Fix records with multiple "General" descriptions

activities_to_fix <- activity_list_unnest_1 %>% 
                        group_by(iati_identifier, activity_title, type.name) %>% 
                        summarise(no_descriptions = n()) %>% 
                        filter(no_descriptions > 1)


activity_list_unnest_1 <- activity_list_unnest_1 %>% 
                              group_by(iati_identifier, activity_title, type.name) %>% 
                              summarise(text = paste(coalesce(text, ""), collapse = "; ")) %>% 
                              spread(key = type.name, value = text)



# 2) Unlist recipient countries
activity_list_unnest_2 <- partner_activity_comb %>% 
  unnest(cols = recipient_country,
         keep_empty = TRUE) %>% 
  select(iati_identifier, country.code, country.name) %>% 
  group_by(iati_identifier) %>%
  unique() %>% 
  summarise(country_code = paste(coalesce(country.code, ""), collapse = ", "),
            country_name = paste(coalesce(country.name, ""), collapse = ", "))


# 3) Unlist sectors
activity_list_unnest_3 <- partner_activity_comb %>% 
  unnest(cols = sector,
         keep_empty = TRUE) %>% 
  select(iati_identifier, sector.code, sector.name, percentage) %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  filter(sector.name != "Vocabulary 99 or 98") %>% 
  group_by(iati_identifier) %>%
  unique()

# Summarise all sector descriptions for each activity
activity_list_unnest_3 <- activity_list_unnest_3 %>% 
  summarise(sector_code = paste(coalesce(sector.code, ""), collapse = ", "),
            sector_name = paste(coalesce(sector.name, ""), collapse = ", "))


# 4) Unlist implementing organisations
activity_list_unnest_4 <- partner_activity_comb %>%
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative, ref) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  filter(role.name == "Implementing") %>% 
  filter(!(text %in% c("Centre de recherches pour le développement international",
                       "Centro Internacional de Investigaciones para el Desarrollo"))) %>% 
  #filter(lang.name == "English") %>% 
  select(-lang.code, -lang.name) %>% 
  unique() %>% 
  # Add simple country locations based on IATI references
  mutate(partner_country = case_when(        
    str_detect(ref, "GB") ~ "United Kingdom", 
    str_detect(ref, "US") ~ "United States", 
    str_detect(ref, "NL") ~ "Netherlands",
    str_detect(ref, "CA-") ~ "Canada",
    str_detect(ref, "IN-") ~ "India"
  )) 

# Match institutions to countries with GRID database
grid_institutes <- read.csv("Inputs/GRID tables/institutes.csv") %>% 
  select(grid_id, name) %>% 
  unique()

grid_addresses <- read.csv("Inputs/GRID tables/addresses.csv") %>% 
  select(grid_id, country, country_code) %>% 
  unique()

grid_aliases <- read.csv("Inputs/GRID tables/aliases.csv")

country_match <- activity_list_unnest_4 %>% 
  left_join(grid_institutes, by = c("text" = "name")) %>% 
  left_join(grid_addresses, by = "grid_id") %>% 
  group_by(iati_identifier, text, ref) %>% 
  top_n(1) %>% 
  mutate(partner_country = if_else(text == "International Development Research Centre", "Canada", 
                                   if_else(text == "CABI", "United Kingdom", partner_country))) %>% 
  mutate(partner_country = coalesce(partner_country, country)) %>% 
  select(iati_identifier, text, ref, partner_country, grid_id) %>% 
  ungroup()

# Extract countries mentioned in the partner organisation name
countries <- countrycode::codelist$country.name.en
countries_string <- paste0(countries, collapse = "|")

country_match_2 <- activity_list_unnest_4 %>% 
  mutate(countries = str_extract_all(text, countries_string)) %>% 
  unnest(cols = countries) %>% 
  unique() %>% 
  group_by(iati_identifier) %>% 
  summarise(countries = paste(coalesce(countries, ""), collapse = ", "))

activity_list_unnest_4_countries <- country_match %>% 
  select(iati_identifier, partner_country) %>% 
  unique() %>% 
  filter(!is.na(partner_country)) %>% 
  group_by(iati_identifier) %>% 
  summarise(partner_country = paste(partner_country, collapse = ", "))

activity_list_unnest_4_partners <- activity_list_unnest_4 %>% 
  select(iati_identifier, text, role.name, ref) %>% 
  unique() %>% 
  filter(!is.na(text)) %>% 
  group_by(iati_identifier) %>% 
  summarise(partner = paste(coalesce(text, ""), collapse = ", "),
            partner_role = paste(coalesce(role.name, ""), collapse = ", "),
            partner_ref = paste(coalesce(ref, ""), collapse = ", ")) 

activity_list_unnest_4 <- activity_list_unnest_4_partners %>% 
  left_join(activity_list_unnest_4_countries, by = "iati_identifier") %>% 
  left_join(country_match_2, by = "iati_identifier") %>% 
  mutate(partner_country = coalesce(partner_country, countries)) %>% 
  select(-countries)


# 4) Unlist extending organisations
activity_list_unnest_5 <- partner_activity_comb %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  filter(lang.name == "English") %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name %in% c("Extending")) %>% 
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(extending_org = paste(coalesce(text, ""), collapse = ", "))


# 5) Unlist reporting department
activity_list_unnest_6 <- partner_activity_comb %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text) %>% 
  filter(!(reporting_org_ref == "XM-DAC-301-2") | reporting_org == "	International Development Research Centre") %>% 
  unique()


# 6) Unlist and aggregate committments
activity_list_unnest_7 <- partner_activity_comb %>% 
  unnest(cols = budget,
         keep_empty = TRUE) %>% 
#  filter(value.date >= "2015-04-01" & value.date <= "2020-03-31") %>%   # restrict time window for spend
  select(iati_identifier, 
         budget_status = status.name, 
         amount = value.value, 
         currency = value.currency.code) %>% 
  group_by(iati_identifier, budget_status, currency) %>% 
  summarise(amount = sum(amount), 0) %>% 
  filter(!is.na(budget_status))


# 8) Unlist start/end dates
activity_list_unnest_9 <- partner_activity_comb %>% 
  unnest(cols = activity_date,
         keep_empty = TRUE) %>% 
  select(iati_identifier, 
         date = iso_date,
         date_type = type.name) %>%
  unique() %>% 
  spread(key = date_type, value = date) %>% 
  mutate(start_date = coalesce(`Actual start`, `Planned start`),
         end_date = coalesce(`Actual end`, `Planned End`)) %>% 
  select(iati_identifier, start_date, end_date)


# Join unnested info to original data
activity_list <- activity_list_base %>% 
  left_join(activity_list_unnest_1, by = "iati_identifier") %>%
  left_join(activity_list_unnest_2, by = "iati_identifier") %>%
  left_join(activity_list_unnest_3, by = "iati_identifier") %>%
  left_join(activity_list_unnest_4, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_5, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_6, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_7, by = "iati_identifier") %>% 
  #left_join(activity_list_unnest_8, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_9, by = "iati_identifier") 


# Assign a reporting org if the reporting partner is implementing 
# the activity themselves
activity_list <- activity_list %>% 
  mutate(reporting_org = coalesce(reporting_org, reporting_org_ref, extending_org, gov_funder))

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
  select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier,
         hierarchy, activity_status, flow_type, fcdo_activity_id,
         activity_title, General, Objectives, country_code, start_date, end_date,
         country_name, sector_code, sector_name,
        # policy_marker_code, policy_marker_name, policy_significance, climate_focus,
         partner, partner_role, partner_ref, partner_country, gov_funder, 
         extending_org, fund,
         budget_status, amount, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())

# Add Fund label
activity_list <- activity_list %>%  
    select(-extending_org) %>% 
    left_join(select(iati_activity_ids, gov_funder, iati_id, funding_iati_id), 
              by = c("iati_identifier" = "iati_id", "gov_funder")) %>% 
    mutate(programme_id = coalesce(funding_iati_id, fcdo_activity_id)) %>% 
    mutate(fund = coalesce(fund, "FCDO fully funded"),
           gov_funder = coalesce(gov_funder, "Foreign, Commonwealth and Development Office"))


activity_list <- activity_list %>% 
  mutate(all_countries = country_name) %>% 
  rename(activity_description = General)

# Remove WHO non-research/innovation activities
activity_list <- activity_list %>% 
  filter(is.na(reporting_org_ref) |
           !(reporting_org_ref == "XM-DAC-928") |
           str_detect(activity_title, "research|innovation"))

# Add missing FCDO activity IDs
activity_list <- activity_list %>% 
  mutate(programme_id = case_when(str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(activity_title, "CLARE") ~ "GB-GOV-1-300126",
                                  str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(activity_title, "CARIAA") ~ "GB-1-203506", 
                                  str_detect(iati_identifier, "XI-IATI-AGR") ~ "GB-1-203052",
                                  reporting_org_ref == "XM-DAC-47015" & str_detect(iati_identifier, "Windows1and2") ~ "GB-1-204764",
                                  TRUE ~ programme_id))


# Save to Rdata file
saveRDS(activity_list, file = "Outputs/partner_activity_list.rds")

# Check funds, funders
table(activity_list$fund)
table(activity_list$gov_funder)
table(activity_list$currency)

# Check specific partner
test1 <- filter(activity_list, str_detect(reporting_org, "AgResults"))
test2 <- filter(activity_list, str_detect(reporting_org, "Abdul"))

