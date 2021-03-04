# --------------------------------------------------------------- #
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

# 1) Read in partner activity IATI list -------------------------
iati_activity_ids <- read_xlsx("Inputs/IATI partner activities.xlsx", sheet=1)


# 2) Extract all partner activities from IATI Registry -----------

partner_activity_extract <- function(activity_id) {
  
  path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?iati_identifier=", activity_id, "&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20")
  #path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?q=", activity_id, "&q_fields=iati_identifier&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20")
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
partner_activity_list <- data.frame()

# Run extraction, stopping when no new sector codes returned
for (id in iati_activity_ids$iati_id) {

    print(id)
    result <- partner_activity_extract(id)
    partner_activity_list <- rbind(partner_activity_list, result)

}


# Save to Rdata file
saveRDS(partner_activity_list, file = "Outputs/partner_activity_list.rds")
# Restore the object
# partner_activity_list <- readRDS(file = "Outputs/partner_activity_list.rds")


# 3) Extract accompanying data ----------------------------------------------

# Extract base activity information - hierarchy and status
activity_list_base <- partner_activity_list %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         flow_type = default_flow_type) %>% 
  mutate(activity_id = "") %>% 
  unique()


# a) Unlist activity title and description
activity_list_unnest_1 <- partner_activity_list %>% 
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
activity_list_unnest_2 <- partner_activity_list %>% 
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
activity_list_unnest_3 <- partner_activity_list %>% 
  unnest(cols = sector,
         keep_empty = TRUE) %>% 
  select(iati_identifier, sector.code, sector.name, percentage) %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  group_by(iati_identifier) %>%
  unique()

# Summarise all sector descriptions for each activity
activity_list_unnest_3 <- activity_list_unnest_3 %>% 
  summarise(sector_code = paste(coalesce(sector.code, ""), collapse = ", "),
            sector_name = paste(coalesce(sector.name, ""), collapse = ", "))


# 4) Unlist implementing organisations
activity_list_unnest_4 <- partner_activity_list %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative, ref) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  filter(role.name == "Implementing") %>% 
  select(-lang.code, -lang.name) %>% 
  unique() %>% 
  # Add simple country locations based on IATI references
  mutate(partner_country = case_when(        
    str_detect(ref, "GB") ~ "United Kingdom", 
    str_detect(ref, "US") ~ "United States", 
    str_detect(ref, "NL") ~ "Netherlands"
  )) %>% 
  group_by(iati_identifier) %>%
  # Summarise up
  summarise(partner = paste(coalesce(text, ""), collapse = ", "),
            partner_role = paste(coalesce(role.name, ""), collapse = ", "),
            partner_ref = paste(coalesce(ref, ""), collapse = ", "),
            partner_country = paste(coalesce(partner_country, ""), collapse = ", ")) 


# 4) Unlist extending organisations
activity_list_unnest_5 <- partner_activity_list %>% 
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
activity_list_unnest_6 <- partner_activity_list %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text) %>% 
  unique()


# 6) Unlist and aggregate committments
activity_list_unnest_7 <- partner_activity_list %>% 
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
activity_list_unnest_9 <- partner_activity_list %>% 
  unnest(cols = activity_date,
         keep_empty = TRUE) %>% 
  select(iati_identifier, 
         date = iso_date,
         date_type = type.name) %>%
  unique() %>% 
  spread(key = date_type, value = date) %>% 
  mutate(start_date = `Actual start`,
         end_date = `Planned End`) %>% 
  # mutate(start_date = coalesce(`Actual start`, `Planned start`),
  #        end_date = coalesce(`Actual end`, `Planned End`)) %>% 
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

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
  select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier,
         hierarchy, activity_status, flow_type, activity_id,
         activity_title, General, Objectives, country_code, start_date, end_date,
         country_name, country_percentage, sector_code, sector_name,
        # policy_marker_code, policy_marker_name, policy_significance, climate_focus,
         partner, partner_role, partner_ref, partner_country, extending_org,
         budget_status, amount, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())

# Add Fund label
activity_list <- activity_list %>%  
    select(-extending_org) %>% 
    left_join(iati_activity_ids, by = c("iati_identifier" = "iati_id")) %>% 
    rename(programme_id = funding_iati_id)

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

# components_only <- activity_list %>% 
#                       mutate(programme_id = if_else(hierarchy == 2, 
#                                                     substr(iati_identifier, 1, nchar(iati_identifier)-4), iati_identifier)) %>% 
#                       filter(((reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10") & hierarchy == 2) | # FCDO - keep components only
#                               str_detect(iati_identifier, "GB-GOV-3") | # ex-FCO activities
#                              reporting_org_ref %in% c("GB-GOV-7", "GB-GOV-12", "GB-GOV-13", "GB-GOV-50") |   # Defra, DCMS, BEIS, Prosperity Fund do not use child hierarchies
#                              (reporting_org_ref %in% c("GB-GOV-8") & budget_status == "Committed") | # MOD - remove duplicate activity with "indicative" budget
#                              (reporting_org_ref %in% c("GB-GOV-10") & str_detect(iati_identifier, "GAMRIF|UKVN")) |
#                              (reporting_org_ref %in% c("GB-GOV-10") & fund == "Other" & !is.na(amount))), # DHSC non-NIHR spend
#                              !(hierarchy == 1 & str_detect(iati_identifier, "AMS|BA")),
#                              flow_type == "ODA") %>% 
#                       select(-climate_focus) %>% 
#                       left_join(select(activity_list, 
#                                        iati_identifier, climate_focus, 
#                                        programme_title = activity_title,
#                                        programme_description = General), 
#                                 by = c("programme_id" = "iati_identifier")) %>% 
#                       mutate(activity_description = if_else(reporting_org_ref == "GB-GOV-1",
#                                                             programme_description,
#                                                             General))

# Save to Rdata file
saveRDS(activity_list, file = "Outputs/partner_activity_list.rds")

