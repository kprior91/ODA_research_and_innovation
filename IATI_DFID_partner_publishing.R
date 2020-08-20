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


# Unlist and aggregate policy markers
policy_marks <- activities_all %>% 
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
                                 if_else(str_detect(climate_focus, "principal"), "Principal", ""))) %>% 

  # filter to climate change activities only
  filter(climate_focus %in% c("Significant", "Principal")) %>% 
  select(iati_identifier, climate_focus)


# Join unnested info to original data
activities_dfid <- activities_all %>% 
      left_join(reporting_orgs, by = "iati_identifier") %>%
      unique() %>% 
      filter(reporting_org == "UK Department for International Development") %>% 
      mutate(programme_id = if_else(hierarchy == 2, 
                                substr(iati_identifier, 1, nchar(iati_identifier)-4), iati_identifier)) %>% 
      left_join(implementing_orgs, by = "iati_identifier") %>% 
      inner_join(policy_marks, by = "iati_identifier")

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
  
  print(paste0("adding page...", page))
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

test2 <- partner_activity_extract(1)
test3 <- partner_activity_extract(1120)

test <- rbind(test2, test3)

# Save to Rdata file
saveRDS(partner_activity_list_final, file = "partner_activity_list_raw.rds")
# partner_activity_list_final <- readRDS(file = "partner_activity_list_raw.rds")

# How many partners have published activities?
no_partners <- unique(partner_activity_list_final$reporting_org.ref)


# 4) Extract accompanying data on these activities ----

# Extract base activity information - hierarchy and status
activity_list_base <- partner_activity_list_final %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         flow_type = default_flow_type)

# 1) Unlist activity title and description
activity_list_unnest_1 <- partner_activity_list_final %>% 
  unnest(cols = title.narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  rename(activity_title = text) %>% 
  unnest(cols = description,
         keep_empty = TRUE) %>% 
  filter(coalesce(type.name, "General") == "General") %>% 
  select(iati_identifier, activity_title, narrative) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>%     
  group_by(iati_identifier, activity_title) %>%
  summarise(activity_description = paste(coalesce(text, ""), collapse = "; "))


# 2) Unlist recipient countries
activity_list_unnest_2 <- partner_activity_list_final %>% 
  unnest(cols = recipient_country,
         keep_empty = TRUE) %>% 
  select(iati_identifier, percentage, country.code, country.name) %>% 
  group_by(iati_identifier) %>%
  summarise(country_code = paste(coalesce(country.code, ""), collapse = ", "),
            country_name = paste(coalesce(country.name, ""), collapse = ", "),
            country_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# 3) Unlist sectors
activity_list_unnest_3 <- partner_activity_list_final %>% 
  unnest(cols = sector,
         keep_empty = TRUE) %>% 
  select(iati_identifier, sector.code, sector.name, percentage) %>% 
  filter(sector.code %in% sector_list_research$code) %>% 
  group_by(iati_identifier) %>%
  summarise(sector_code = paste(coalesce(sector.code, ""), collapse = ", "),
            sector_name = paste(coalesce(sector.name, ""), collapse = ", "),
            sector_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# 4) Unlist implementing organisations
activity_list_unnest_4 <- partner_activity_list_final %>% 
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

# 5) Unlist reporting organisation
activity_list_unnest_5 <- partner_activity_list_final %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text)


# 6) Unlist and aggregate committments
activity_list_unnest_6 <- partner_activity_list_final %>% 
  unnest(cols = budget,
         keep_empty = TRUE) %>% 
  #  filter(value.date >= "2015-04-01" & value.date <= "2020-03-31") %>% 
  select(iati_identifier, 
         budget_status = status.name, 
         amount = value.value, 
         currency = value.currency.code) %>% 
  group_by(iati_identifier, budget_status, currency) %>% 
  summarise(amount = sum(amount), 0) %>% 
  filter(!is.na(budget_status))


# 7) Unlist and aggregate policy markers
activity_list_unnest_7 <- partner_activity_list_final %>% 
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
activity_list_unnest_8 <- partner_activity_list_final %>% 
  unnest(cols = activity_date,
         keep_empty = TRUE) %>% 
  select(iati_identifier, 
         date = iso_date,
         date_type = type.name) %>%
  unique() %>% 
  group_by(iati_identifier, date_type) %>% 
  top_n(n = 1, wt = date) %>% 
  ungroup() %>% 
  spread(key = date_type, value = date) %>% 
  mutate(start_date = coalesce(`Actual start`, `Planned start`),
         end_date = coalesce(`Actual end`, `Planned End`)) %>% 
  select(iati_identifier, start_date, end_date)


# Join unnested info to original data
activity_list <- activity_list_base %>% 
  left_join(activity_list_unnest_1, by = "iati_identifier") %>%
  left_join(activity_list_unnest_2, by = "iati_identifier") %>%
  inner_join(activity_list_unnest_3, by = "iati_identifier") %>%
  left_join(activity_list_unnest_4, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_5, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_6, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_7, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_8, by = "iati_identifier")

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
  select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier,
         hierarchy, activity_status, flow_type,
         activity_title, activity_description, country_code, start_date, end_date,
         country_name, country_percentage, sector_code, sector_name,
         policy_marker_code, policy_marker_name, policy_significance, climate_focus,
         sector_percentage, partner, partner_role, 
         budget_status, amount, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date()) 

# Save to Rdata file
saveRDS(activity_list, file = "DFID_partner_activities_unnested.rds")


# Filter climate change activities
partner_climate_activities <- filter(activity_list, !is.na(climate_focus))
                                 



