# --------------------------------------------------------------- #
# Extract ODA research activitie from public IATI data #
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
  filter(str_detect(str_to_lower(name), "research") | 
           str_detect(str_to_lower(name), "higher education") |
           str_detect(str_to_lower(name), "information and communication technology"))


# 2) Extract list of UK government publishers to IATI -----------------------------

# Function to extract all UK government reporters

gov_reporters <- function(page) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/publishers/?q=GB-GOV&q_fields=reporting_org_identifier?format=json&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # Condition to check when 5-digit codes stop being returned
  if(!("category" %in% names(response$results))) {
    reporters <- rbind(reporter_list_final, response$results)
  } else {
    reporters <- reporter_list_final
  }
  return(reporters)
}

# Prepare results data frame and counters
reporter_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(reporter_list_final)
  reporter_list_final <- gov_reporters(page)
  page <- page + 1
  y <- nrow(reporter_list_final)
  new_rows = y - x
}


# 3) Extract all research activities by BEIS, DFID, DHSC, FCO, Defra -----------

# Set strings for API URL
sector_codes <- paste(sector_list_research$code, collapse=",")
organisation_codes <- paste(reporter_list_final$publisher_iati_id, collapse=",")

organisation_codes <- "GB-GOV-1,GB-GOV-3,GB-GOV-7,GB-GOV-10,GB-GOV-12,GB-GOV-13,GB-GOV-14,GB-GOV-15,GB-GOV-50,GB-GOV-52"

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
  
  research_list <- rbind(research_list_final, new_data)
  
  return(research_list)
}

# Prepare results data frame and counters
research_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(research_list_final)
  research_list_final <- research_extract(page)
  page <- page + 1
  y <- nrow(research_list_final)
  new_rows = y - x
}

# B. Extract activities based on fund

fund_extract <- function(fund_string, page) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?q=", fund_string, "&q_fields=iati_identifier&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20&page=", page)
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
  
  fund_list <- rbind(fund_list_final, new_data)
  
  return(fund_list)
}

# Prepare results data frame and counters
fund_list_final <- data.frame()

# Run extraction, stopping when no new sector codes returned
for (fund in c("NEWT", "Newton", "GCRF", "NIHR", "GAMRIF", "UKVN")) {
  new_rows <- 0
  page <- 1
  
  while (page == 1 | new_rows > 0) {
    x <- nrow(fund_list_final)
    fund_list_final <- fund_extract(fund, page)
    page <- page + 1
    y <- nrow(fund_list_final)
    new_rows = y - x
  }
}


# C. Extract activities based on FCDO RED programme number 
# (manually transferred as not yet identifiable in IATI)

# Note: 100039 = "RED Admin", 100040 = "DFID Research - Admin Capital"

RED_programmes <- read_excel("Inputs/RED programme IDs.xlsx") %>% 
                      mutate(red_iati_id = paste0("GB-1-", ProjectID))

active_RED_programmes <- filter(RED_programmes, StageDescription == "Implementation")

RED_programme_extract <- function(programme_id) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?q=", programme_id, "&q_fields=iati_identifier&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity")
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
  
  programme_list_final <- rbind(programme_list_final, new_data)
  
  return(programme_list_final)
}

# Prepare results data frame and counters
programme_list_final <- data.frame()

# Run extraction over each RED programme ID
for (programme_id in active_RED_programmes$red_iati_id) {
    print(programme_id)
    programme_list_final <- RED_programme_extract(programme_id)

  }

# Combine the three extracts and deduplicate records
activity_list_final <- rbind(research_list_final, fund_list_final, programme_list_final) %>% 
                            unique()

# Save to Rdata file
saveRDS(activity_list_final, file = "Outputs/activity_list_final.rds")
# Restore the object
# activity_list_final <- readRDS(file = "activity_list_final.rds")


# 4) Extract accompanying data ----------------------------------------------

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


# 4) Unlist extending organisations
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


# 5) Unlist reporting department
activity_list_unnest_6 <- activity_list_final %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text) %>% 
  unique()


# 6) Unlist and aggregate committments
activity_list_unnest_7 <- activity_list_final %>% 
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

# Join on research percentages
activity_list_unnest_7 <- activity_list_unnest_7 %>% 
      left_join(sector_percentages, by = "iati_identifier") %>% 
      mutate(research_pc = coalesce(research_pc, 100)) %>% 
      mutate(adjusted_amount = (research_pc/100)*amount)


# 7) Unlist and aggregate policy markers
activity_list_unnest_8 <- activity_list_final %>% 
  unnest(cols = policy_marker,
         keep_empty = TRUE) %>% 
  select(iati_identifier, 
         policy_marker_code = policy_marker.code, 
         policy_marker_name = policy_marker.name, 
         policy_significance = significance.name) %>% 
  filter(!is.na(policy_marker_name) & policy_significance != "not targeted") %>% 
  mutate(climate_marker = if_else(str_detect(policy_marker_name, "Climate Change"), policy_significance, "")) %>% 
  group_by(iati_identifier) %>% 
  summarise(policy_marker_code = paste(coalesce(policy_marker_code, ""), collapse = ", "),
            policy_marker_name = paste(coalesce(policy_marker_name, ""), collapse = ", "),
            policy_significance = paste(coalesce(policy_significance, ""), collapse = ", "),
            climate_focus = paste(coalesce(climate_marker, ""), collapse = ", ")) %>% 
  mutate(climate_focus = if_else(str_detect(climate_focus, "significant"), "Significant",
                                 if_else(str_detect(climate_focus, "principal"), "Principal", "")))

# 8) Unlist start/end dates
activity_list_unnest_9 <- activity_list_final %>% 
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
  left_join(activity_list_unnest_8, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_9, by = "iati_identifier")

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
  select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier,
         hierarchy, activity_status, flow_type, activity_id,
         activity_title, General, Objectives, country_code, start_date, end_date,
         country_name, country_percentage, sector_code, sector_name,
         policy_marker_code, policy_marker_name, policy_significance, climate_focus,
         sector_percentage, partner, partner_role, extending_org,
         budget_status, amount = adjusted_amount, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())

# Add Fund label
activity_list <- activity_list %>%  
    mutate(fund = case_when(
      (str_detect(iati_identifier, "Newton") | str_detect(iati_identifier, "NEWT")) ~ "Newton",
      str_detect(iati_identifier, "GCRF") ~ "GCRF",
      str_detect(iati_identifier, "UKVN") ~ "UK Vaccine Network",
      str_detect(iati_identifier, "GAMRIF") ~ "GAMRIF",
      (str_detect(iati_identifier, "NIHR") | str_detect(activity_title, "NIHR")) ~ "Global Health Research",
      str_detect(iati_identifier, "ICF") ~ "ICF",
      str_detect(iati_identifier, "Chevening") ~ "Chevening",
      str_detect(iati_identifier, paste(RED_programmes$ProjectID, collapse = '|')) ~ "FCDO Research",
      TRUE ~ "Other"
    ))

# Correct Funder names
activity_list <- activity_list %>%  
  mutate(reporting_org = case_when(
    str_detect(reporting_org, "DFID") ~ "Foreign, Commonwealth & Development Office",
    str_detect(reporting_org, "Health") ~ "Department of Health & Social Care",
    str_detect(reporting_org, "Culture") ~ "Department for Digital, Culture, Media & Sport",
    TRUE ~ reporting_org
  )) %>% 
  mutate(reporting_org = str_replace_all(reporting_org, "UK - ", ""))

# Extract countries mentioned in abstract or title
countries <- countrycode::codelist$country.name.en
countries_string <- paste0(countries, collapse = "|")

countries_in_description <- activity_list %>% 
  mutate(text = paste0(activity_title, " ", General)) %>% 
  select(iati_identifier, text) %>% 
  mutate(countries = str_extract_all(text, countries_string)) %>% 
  unnest(cols = countries) %>% 
  unique() %>% 
  group_by(iati_identifier) %>% 
  summarise(countries = paste(coalesce(countries, ""), collapse = ", "))


activity_list <- activity_list %>% 
  left_join(countries_in_description, by = "iati_identifier") %>% 
  mutate(all_countries = country_name) 
# mutate(all_countries = paste0(country_name, ", ", countries)) # this combines IATI country with those taken from description for searchability in Power BI

# Extract detail at child activity level (if available) and ensure
# spend is not being double-counted e.g. for DFID

components_only <- activity_list %>% 
                      mutate(programme_id = if_else(hierarchy == 2, 
                                                    substr(iati_identifier, 1, nchar(iati_identifier)-4), iati_identifier)) %>% 
                      filter(((reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10") & hierarchy == 2) | # FCDO - keep components only
                              str_detect(iati_identifier, "GB-GOV-3") | # ex-FCO activities
                             reporting_org_ref %in% c("GB-GOV-7", "GB-GOV-12", "GB-GOV-13", "GB-GOV-50") |   # Defra, DCMS, BEIS, Prosperity Fund do not use child hierarchies
                             (reporting_org_ref %in% c("GB-GOV-8") & budget_status == "Committed") | # MOD - remove duplicate activity with "indicative" budget
                             (reporting_org_ref %in% c("GB-GOV-10") & str_detect(iati_identifier, "GAMRIF|UKVN")) |
                             (reporting_org_ref %in% c("GB-GOV-10") & fund == "Other" & !is.na(amount))), # DHSC non-NIHR spend
                             !(hierarchy == 1 & str_detect(iati_identifier, "AMS|BA")),
                             flow_type == "ODA") %>% 
                      select(-climate_focus) %>% 
                      left_join(select(activity_list, 
                                       iati_identifier, climate_focus, 
                                       programme_title = activity_title,
                                       programme_description = General), 
                                by = c("programme_id" = "iati_identifier")) %>% 
                      mutate(activity_description = if_else(reporting_org_ref == "GB-GOV-1",
                                                            programme_description,
                                                            General))

# Save to Rdata file
saveRDS(components_only, file = "Outputs/activity_list.rds")


# Save to Excel
write_xlsx(x = list(`IATI research` = components_only), 
           path = "Outputs/IATI research activities.xlsx")
