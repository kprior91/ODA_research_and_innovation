# --------------------------------------------------------------- #
# Script to extract IATI data for selected R&I delivery partners
# 1) British Council
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


# 1) Extract list of research sector codes from IATI -----------------------------

# Function to extract 5-digit sector codes

sector_extract <- function(page) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/sectors/?fields=category,url,name,code&format=json&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # Condition to check when 5-digit codes stop being returned
  if(!("category" %in% names(response$results))) {
    sector_list <- rbind(sector_list_final, response$results)
  } else {
    sector_list <- sector_list_final
  }
  return(sector_list)
}

# Prepare results data frame and counters
sector_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(sector_list_final)
  sector_list_final <- sector_extract(page)
  page <- page + 1
  y <- nrow(sector_list_final)
  new_rows = y - x
}

# Keep research codes only (11)
sector_list_research <- sector_list_final %>% 
  filter(str_detect(str_to_lower(name), "research") | str_detect(str_to_lower(name), "higher education"))


# 2) Extract all research activities by delivery partners  -----------

# Set strings for API URL
sector_codes <- paste(sector_list_research$code, collapse=",")
organisation_codes <- "GB-CHC-209131"  # British Council

# A. Function to extract research projects by OECD sector code
research_extract <- function(page) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?format=json&reporting_org_identifier=", organisation_codes, "&sector=", sector_codes, "&fields=iati_identifier,other_identifier,activity_date,reporting_org,location,default_flow_type,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20&page=", page)
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
  
  research_list <- rbind(activity_list_final, new_data)
  
  return(research_list)
}

# Prepare results data frame and counters
activity_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(activity_list_final)
  activity_list_final <- research_extract(page)
  page <- page + 1
  y <- nrow(activity_list_final)
  new_rows = y - x
}


# 3) Extract accompanying data ----------------------------------------------

# Extract base activity information - hierarchy and status
activity_list_base <- activity_list_final %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         flow_type = default_flow_type) %>% 
  mutate(activity_id = "") %>% 
  unique()


# 1) Unlist activity title and description
activity_list_unnest_1 <- activity_list_final %>% 
  unnest(cols = title.narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  rename(activity_title = text) %>% 
  unnest(cols = description,
         keep_empty = TRUE) %>% 
  mutate(type.name = coalesce(type.name, "General")) %>% 
  select(iati_identifier, activity_title, type.name, narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>%     
  unique()


# Combine descriptions (General, Target Groups, Objectives)
activity_list_unnest_1 <- activity_list_unnest_1 %>% 
  group_by(iati_identifier, activity_title, type.name) %>% 
  summarise(text = paste(coalesce(text, ""), collapse = "; ")) %>% 
  spread(key = type.name, value = text)


# 2) Unlist recipient countries
activity_list_unnest_2 <- activity_list_final %>% 
  unnest(cols = recipient_country,
         keep_empty = TRUE) %>% 
  select(iati_identifier, percentage, country.code, country.name) %>% 
  group_by(iati_identifier) %>%
  unique() %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  summarise(country_code = paste(coalesce(country.code, ""), collapse = ", "),
            country_name = paste(coalesce(country.name, ""), collapse = ", "),
            country_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# 3) Unlist sectors
activity_list_unnest_3 <- activity_list_final %>% 
  unnest(cols = sector,
         keep_empty = TRUE) %>% 
  select(iati_identifier, sector.code, sector.name, percentage) %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  group_by(iati_identifier) %>%
  unique()

# Extract research sector percentages
sector_percentages <- activity_list_unnest_3 %>% 
                         filter(sector.name %in% sector_list_research$name, 
                                !is.na(percentage), percentage != 100) %>% 
                      select(iati_identifier, percentage) %>% 
                      group_by(iati_identifier) %>% 
                      summarise(research_pc = sum(percentage))

# Summarise all sector descriptions for each activity
activity_list_unnest_3 <- activity_list_unnest_3 %>% 
  summarise(sector_code = paste(coalesce(sector.code, ""), collapse = ", "),
            sector_name = paste(coalesce(sector.name, ""), collapse = ", "),
            sector_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# 4) Unlist implementing organisations
activity_list_unnest_4 <- activity_list_final %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name == "Implementing") %>% 
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(partner = paste(coalesce(text, ""), collapse = ", "),
            partner_role = paste(coalesce(role.name, ""), collapse = ", "))


# 5) Unlist extending organisations
activity_list_unnest_5 <- activity_list_final %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name == "Extending") %>% 
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(extending_org = paste(coalesce(text, ""), collapse = ", "))


# 6) Unlist funding organisations
activity_list_unnest_6 <- activity_list_final %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name == "Funding") %>% 
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(funding_org = paste(coalesce(text, ""), collapse = ", "))


# 7) Unlist reporting organisation
activity_list_unnest_7 <- activity_list_final %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text) %>% 
  unique()


# 8) Unlist and aggregate committments
activity_list_unnest_8 <- activity_list_final %>% 
  unnest(cols = budget,
         keep_empty = TRUE) %>% 
  filter(status.name == "Committed") %>%   # restrict to committed spend
  select(iati_identifier, 
         budget_status = status.name, 
         amount = value.value, 
         currency = value.currency.code) %>% 
  group_by(iati_identifier, budget_status, currency) %>% 
  summarise(amount = sum(amount), 0) %>% 
  filter(!is.na(budget_status))


# Join on research percentages
activity_list_unnest_8 <- activity_list_unnest_8 %>% 
      left_join(sector_percentages, by = "iati_identifier") %>% 
      mutate(research_pc = coalesce(research_pc, 100)) %>% 
      mutate(adjusted_amount = (research_pc/100)*amount)


# 9) Unlist start/end dates
activity_list_unnest_9 <- activity_list_final %>% 
  unnest(cols = activity_date,
         keep_empty = TRUE) %>% 
  select(iati_identifier, 
         date = iso_date,
         date_type = type.name) %>%
  unique() %>% 
  spread(key = date_type, value = date) %>% 
  mutate(start_date = coalesce(`Actual start`, `Planned start`),
         end_date = coalesce(`Planned End`)) %>%   # all open activities
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
  left_join(activity_list_unnest_8, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_9, by = "iati_identifier")

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
  select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier,
         hierarchy, activity_status, flow_type, activity_id,
         activity_title, General, Objectives, country_code, start_date, end_date,
         country_name, country_percentage, sector_code, sector_name,
      #   policy_marker_code, policy_marker_name, policy_significance, climate_focus,
         sector_percentage, partner, partner_role, extending_org, funding_org,
         budget_status, amount = adjusted_amount, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())

# Add Fund label
activity_list <- activity_list %>%  
    mutate(fund = case_when(
      str_detect(funding_org, "DFID") ~ "FCDO Research",
      TRUE ~ "Other"
    ))

delivery_partner_list <- activity_list %>% 
  mutate(all_countries = country_name,
         activity_description = if_else(is.na(Objectives), General, 
                                        paste0(General, "\nObjectives: ", Objectives)))
           
           
# Save to Excel
saveRDS(delivery_partner_list, "Outputs/delivery_partner_activity_list.rds")

