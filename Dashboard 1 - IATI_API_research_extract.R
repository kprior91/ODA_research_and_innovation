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

# gov_reporters <- function(page) {
#   path <- paste0("https://iatidatastore.iatistandard.org/api/publishers/?q=GB-GOV&q_fields=reporting_org_identifier?format=json&page_size=20&page=", page)
#   request <- GET(url = path)
#   response <- content(request, as = "text", encoding = "UTF-8")
#   response <- fromJSON(response, flatten = TRUE) 
#   
#   # Condition to check when 5-digit codes stop being returned
#   if(!("category" %in% names(response$results))) {
#     reporters <- rbind(reporter_list_final, response$results)
#   } else {
#     reporters <- reporter_list_final
#   }
#   return(reporters)
# }
# 
# # Prepare results data frame and counters
# reporter_list_final <- data.frame()
# new_rows <- 0
# page <- 1
# 
# # Run extraction, stopping when no new reporter codes returned
# while (page == 1 | new_rows > 0) {
#   x <- nrow(reporter_list_final)
#   reporter_list_final <- gov_reporters(page)
#   page <- page + 1
#   y <- nrow(reporter_list_final)
#   new_rows = y - x
# }


# 3) Extract all activities by government publishers -----------

# Set strings for API URL
sector_codes <- paste(sector_list_research$code, collapse=",")
#organisation_codes <- paste(reporter_list_final$publisher_iati_id, collapse=",")
organisation_codes <- c("GB-GOV-1", "GB-GOV-7", "GB-GOV-10", "GB-GOV-13", "GB-GOV-15", "GB-GOV-50", "GB-GOV-52")

# Function to extract all UK government department activities
uk_gov_extract <- function(page, org_code) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?format=json&reporting_org_identifier=", org_code, "&fields=iati_identifier,other_identifier,activity_date,reporting_org,sector,location,default_flow_type,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity,tag&page_size=20&page=", page)
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
  
  results <- rbind(uk_gov_list_final, new_data)
  
  return(results)
}

# Prepare results data frame and counters
uk_gov_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned

for (org in organisation_codes) {
  new_rows <- 0
  page <- 1

  while (page == 1 | new_rows > 0) {
    print(paste0(org, "-", page))
    x <- nrow(uk_gov_list_final)
    uk_gov_list_final <- uk_gov_extract(page, org)
    page <- page + 1
    y <- nrow(uk_gov_list_final)
    new_rows = y - x
  }
}

saveRDS(uk_gov_list_final, file = "Outputs/uk_gov_list_final.rds")
# Restore the object
# uk_gov_list_final <- readRDS(file = "Outputs/uk_gov_list_final.rds")


# 4) Restrict to ODA R&I activities -----------
# (eventually via the "tag" field)
# This step removes BEIS ICF and DFID non-RED programmes

# Filter activities by fund (BEIS GCRF/Newton, DHSC GHS/GHR, FCDO RED)
RED_programmes <- read_excel("Inputs/RED programme IDs.xlsx") %>% 
  mutate(red_iati_id_1 = paste0("GB-1-", ProjectID),
         red_iati_id_2 = paste0("GB-GOV-1-", ProjectID))


RED_programme_codes <- c(RED_programmes$red_iati_id_1, RED_programmes$red_iati_id_2)

RED_programme_codes <- paste(RED_programme_codes, collapse="|")

uk_gov_list_filtered <- uk_gov_list_final %>% 
  filter((reporting_org.ref %in% c("GB-GOV-7", "GB-GOV-15", "GB-GOV-50", "GB-GOV-52", "GB-GOV-10") | str_detect(iati_identifier, "GB-GOV-3")) |  # Other UK gov deps (including ex-FCO)
         (reporting_org.ref == "GB-GOV-1" & str_detect(iati_identifier, RED_programme_codes)) |   # FCDO RED programmes
         str_detect(iati_identifier, "NEWT|Newton|NF|GCRF|NIHR|GAMRIF|UKVN"),   # Keep BEIS Newton/GCRF and DHSC GHS/GHR
         default_flow_type == "ODA")                                            # ODA only


# 5) Unnest activity information -----------

# Extract base activity information - hierarchy and status
gov_list_base <- uk_gov_list_filtered %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         flow_type = default_flow_type) %>% 
  mutate(activity_id = "") %>% 
  unique()


# A) Unlist activity title and description
gov_list_unnest_1 <- uk_gov_list_filtered %>% 
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

activities_to_fix <- gov_list_unnest_1 %>% 
  group_by(iati_identifier, activity_title, type.name) %>% 
  summarise(no_descriptions = n()) %>% 
  filter(no_descriptions > 1)


gov_list_unnest_1 <- gov_list_unnest_1 %>% 
  group_by(iati_identifier, activity_title, type.name) %>% 
  summarise(text = paste(coalesce(text, ""), collapse = "; ")) %>% 
  spread(key = type.name, value = text)


# B) Unlist recipient countries
gov_list_unnest_2 <- uk_gov_list_filtered %>% 
  unnest(cols = recipient_country,
         keep_empty = TRUE) %>% 
  select(iati_identifier, percentage, country.code, country.name) %>% 
  group_by(iati_identifier) %>%
  unique() %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  summarise(country_code = paste(coalesce(country.code, ""), collapse = ", "),
            country_name = paste(coalesce(country.name, ""), collapse = ", "),
            country_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# C) Unlist sectors
gov_list_unnest_3 <- uk_gov_list_filtered %>% 
  unnest(cols = sector,
         keep_empty = TRUE) %>% 
  select(iati_identifier, sector.code, sector.name, percentage) %>% 
  filter(sector.name %in% sector_list_research$name) %>%  # keep research sectors only
  mutate(percentage = as.numeric(percentage)) %>% 
  group_by(iati_identifier) %>%
  unique()

# Extract research sector percentages
sector_percentages <- gov_list_unnest_3 %>% 
  filter(!is.na(percentage), percentage != 100) %>% 
  select(iati_identifier, percentage) %>% 
  group_by(iati_identifier) %>% 
  summarise(research_pc = sum(percentage))

# Summarise all sector descriptions for each activity
gov_list_unnest_3 <- gov_list_unnest_3 %>% 
  summarise(sector_code = paste(coalesce(sector.code, ""), collapse = ", "),
            sector_name = paste(coalesce(sector.name, ""), collapse = ", "),
            sector_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# D) Unlist implementing organisations
gov_list_unnest_4 <- uk_gov_list_filtered %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative, ref) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name == "Implementing") %>% 
  unique() %>% 
  # Add simple country locations based on IATI references
  mutate(partner_country = case_when(        
    str_detect(ref, "GB-") ~ "United Kingdom", 
    str_detect(ref, "US-") ~ "United States", 
    str_detect(ref, "NL-") ~ "Netherlands",
    str_detect(ref, "CA-") ~ "Canada",
    str_detect(ref, "IN-") ~ "India"
  ))

gov_list_unnest_4_countries <- gov_list_unnest_4 %>% 
  select(iati_identifier, partner_country) %>% 
  unique() %>% 
  filter(!is.na(partner_country)) %>% 
  group_by(iati_identifier) %>% 
  summarise(partner_country = paste(partner_country, collapse = ", "))

gov_list_unnest_4_partners <- gov_list_unnest_4 %>% 
  select(iati_identifier, text, role.name, ref) %>% 
  unique() %>% 
  filter(!is.na(text)) %>% 
  group_by(iati_identifier) %>% 
  summarise(partner = paste(coalesce(text, ""), collapse = ", "),
            partner_role = paste(coalesce(role.name, ""), collapse = ", "),
            partner_ref = paste(coalesce(ref, ""), collapse = ", ")) 

gov_list_unnest_4 <- gov_list_unnest_4_partners %>% 
  left_join(gov_list_unnest_4_countries, by = "iati_identifier")


# E) Unlist extending organisations
gov_list_unnest_5 <- uk_gov_list_filtered %>% 
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


# F) Unlist reporting department
gov_list_unnest_6 <- uk_gov_list_filtered %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text) %>% 
  unique()


# G) Unlist and aggregate committments
gov_list_unnest_7 <- uk_gov_list_filtered %>% 
  unnest(cols = budget,
         keep_empty = TRUE) %>% 
  #  filter(value.date >= "2015-04-01" & value.date <= "2020-03-31") %>%   # restrict time window for spend
  select(iati_identifier, 
         budget_status = status.name, 
         amount = value.value, 
         currency = value.currency.code) %>% 
  group_by(iati_identifier, budget_status, currency) %>% 
  summarise(amount = sum(amount)) %>% 
  filter(!is.na(budget_status)) %>% 
  ungroup()

# Find activities with multiple budgets
multiple_budgets <- gov_list_unnest_7 %>% 
  group_by(iati_identifier) %>% 
  summarise(count = n()) %>% 
  filter (count > 1)

# Keep only the committed budget in these cases
gov_list_unnest_7_dedup <- gov_list_unnest_7 %>% 
  filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
         budget_status == "Committed")

# Join on research percentages
gov_list_unnest_7_dedup <- gov_list_unnest_7_dedup %>% 
  left_join(sector_percentages, by = "iati_identifier") %>% 
  mutate(research_pc = coalesce(research_pc, 100)) %>% 
  mutate(adjusted_amount = (research_pc/100)*amount)


# H) Unlist and aggregate policy markers
gov_list_unnest_8 <- uk_gov_list_filtered %>% 
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

# I) Unlist start/end dates
gov_list_unnest_9 <- uk_gov_list_filtered %>% 
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
gov_list <- gov_list_base %>% 
  left_join(gov_list_unnest_1, by = "iati_identifier") %>%
  left_join(gov_list_unnest_2, by = "iati_identifier") %>%
  left_join(gov_list_unnest_3, by = "iati_identifier") %>%
  left_join(gov_list_unnest_4, by = "iati_identifier") %>% 
  left_join(gov_list_unnest_5, by = "iati_identifier") %>% 
  left_join(gov_list_unnest_6, by = "iati_identifier") %>% 
  left_join(gov_list_unnest_7_dedup, by = "iati_identifier") %>% 
  left_join(gov_list_unnest_8, by = "iati_identifier") %>% 
  left_join(gov_list_unnest_9, by = "iati_identifier") %>% 
  filter(reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10", "GB-GOV-13") & !str_detect(iati_identifier, "GB-GOV-3") |
           !is.na(sector_name))   # removes non-R&I activities for Defra, ex-FCO (GB-GOV-3) and Prosperity Fund

# Reorder columns and add date of refresh
gov_list <- gov_list %>% 
  select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier,
         hierarchy, activity_status, flow_type, activity_id,
         activity_title, General, Objectives, country_code, start_date, end_date,
         country_name, country_percentage, sector_code, sector_name,
         policy_marker_code, policy_marker_name, policy_significance, climate_focus,
         sector_percentage, partner, partner_role, partner_ref, partner_country, extending_org,
         budget_status, amount = adjusted_amount, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())

# Add Fund label
gov_list <- gov_list %>%  
  mutate(fund = case_when(
    (str_detect(iati_identifier, "Newton") | str_detect(iati_identifier, "NEWT") | str_detect(iati_identifier, "NT")) ~ "Newton Fund",
    str_detect(iati_identifier, "GCRF") ~ "Global Challenges Research Fund (GCRF)",
    str_detect(iati_identifier, "UKVN") ~ "Global Health Security - UK Vaccine Network",
    str_detect(iati_identifier, "GAMRIF") ~ "Global Health Security - GAMRIF",
    (str_detect(iati_identifier, "NIHR") | str_detect(activity_title, "NIHR")) ~ "Global Health Research - Programmes",
    str_detect(iati_identifier, "ICF") ~ "International Climate Finance (ICF)",
    str_detect(iati_identifier, "Chevening") ~ "Chevening",
    str_detect(iati_identifier, RED_programme_codes) ~ "FCDO Research & Innovation",
    TRUE ~ "Other"
  ))

# Correct Funder names
gov_list <- gov_list %>%  
  mutate(reporting_org = case_when(
    reporting_org_ref == "GB-GOV-1" ~ "Foreign, Commonwealth and Development Office",
    str_detect(reporting_org, "Health") ~ "Department of Health and Social Care",
    str_detect(reporting_org, "Culture") ~ "Department for Digital, Culture, Media and Sport",
    TRUE ~ reporting_org
  )) %>% 
  mutate(reporting_org = str_replace_all(reporting_org, "UK - ", ""))


# Extract countries mentioned in abstract or title
countries <- countrycode::codelist$country.name.en
countries_string <- paste0(countries, collapse = "|")

countries_in_description <- gov_list %>% 
  mutate(text = paste0(activity_title, " ", General)) %>% 
  select(iati_identifier, text) %>% 
  mutate(countries = str_extract_all(text, countries_string)) %>% 
  unnest(cols = countries) %>% 
  unique() %>% 
  group_by(iati_identifier) %>% 
  summarise(countries = paste(coalesce(countries, ""), collapse = ", "))


gov_list <- gov_list %>% 
  left_join(countries_in_description, by = "iati_identifier") %>% 
  mutate(all_countries = country_name) 
# mutate(all_countries = paste0(country_name, ", ", countries)) # this combines IATI country with those taken from description for searchability in Power BI



# 5) Account for parent-child hierarchies -----------
# Extract detail at child activity level (if available) and ensure
# spend is not being double-counted e.g. for DFID

gov_list_final <- gov_list %>% 
  mutate(programme_id = if_else(hierarchy == 2, 
                                substr(iati_identifier, 1, nchar(iati_identifier)-4), iati_identifier)) %>% # FCDO programme ID
  filter(((reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10") & hierarchy == 2) | # FCDO - keep components only
            str_detect(iati_identifier, "GB-GOV-3") | # ex-FCO activities
            reporting_org_ref %in% c("GB-GOV-7", "GB-GOV-12", "GB-GOV-13", "GB-GOV-50") |   # Defra, DCMS, BEIS, Prosperity Fund do not use child hierarchies
            (reporting_org_ref %in% c("GB-GOV-10") & str_detect(iati_identifier, "GAMRIF|UKVN")) |
            (reporting_org_ref %in% c("GB-GOV-10") & fund == "Other" & !is.na(amount))), # DHSC non-NIHR spend
         !(hierarchy == 1 & str_detect(iati_identifier, "AMS|BA")), # BEIS DP activities
         flow_type == "ODA") %>% 
  select(-climate_focus) 

# Join on FCDO programme descriptions at component level
gov_list_final <- gov_list_final %>% 
  left_join(select(gov_list, 
                   iati_identifier, climate_focus, 
                   programme_title = activity_title,
                   programme_description = General), 
            by = c("programme_id" = "iati_identifier")) %>% 
  mutate(activity_description = if_else(reporting_org_ref == "GB-GOV-1",
                                        programme_description,
                                        General))


# --------------

# check list of ODA R&I funds
unique(gov_list_final$fund)

# check list of ODA R&I funders
unique(gov_list_final$reporting_org)

#---------------
  
# Save to Rdata file
saveRDS(gov_list_final, file = "Outputs/gov_list_final.rds")
# gov_list_final <- readRDS("Outputs/gov_list_final.rds") 

# Save to Excel
write_xlsx(x = list(`IATI research` = gov_list_final), 
           path = "Outputs/IATI research activities.xlsx")
