# --------------------------------------------------------------- #
# Test extracting activities in South Africa from UK institutions #
# --------------------------------------------------------------- #

# install.packages("jsonlite")
# install.packages("httr")

library(jsonlite)
library(httr)
library(tidyverse)
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



# 2) Extract all research activities by BEIS, DFID, DHSC, FCO, Defra -----------

# Set strings for API URL
sector_codes <- paste(sector_list_research$code, collapse=",")
organisation_codes <- "GB-GOV-1,GB-GOV-3,GB-GOV-7,GB-GOV-10,GB-GOV-13,"


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

fund_strings <- "Newton,NEWT,GCRF,NIHR"

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
for (fund in c("NEWT", "Newton", "GCRF", "NIHR")) {
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

# Combine and dedup
activity_list_final <- rbind(research_list_final, fund_list_final) %>% 
                            unique()

# Save to Rdata file
saveRDS(activity_list_final, file = "activity_list_final.rds")
# Restore the object
# activity_list_final <- readRDS(file = "activity_list_final.rds")



# Extract accompanying data ----

# Extract base activity information - hierarchy and status
activity_list_base <- activity_list_final %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         flow_type = default_flow_type) %>% 
  mutate(activity_id = "")

# 1) Unlist activity title and description
activity_list_unnest_1 <- activity_list_final %>% 
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
activity_list_unnest_2 <- activity_list_final %>% 
  unnest(cols = recipient_country,
         keep_empty = TRUE) %>% 
  select(iati_identifier, percentage, country.code, country.name) %>% 
  group_by(iati_identifier) %>%
  summarise(country_code = paste(coalesce(country.code, ""), collapse = ", "),
            country_name = paste(coalesce(country.name, ""), collapse = ", "),
            country_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# 3) Unlist sectors
activity_list_unnest_3 <- activity_list_final %>% 
  unnest(cols = sector,
         keep_empty = TRUE) %>% 
  select(iati_identifier, sector.code, sector.name, percentage) %>% 
  group_by(iati_identifier) %>%
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

# 5) Unlist reporting department
activity_list_unnest_5 <- activity_list_final %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text)


# 6) Unlist and aggregate committments
activity_list_unnest_6 <- activity_list_final %>% 
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
activity_list_unnest_7 <- activity_list_final %>% 
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
activity_list_unnest_8 <- activity_list_final %>% 
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
  left_join(activity_list_unnest_8, by = "iati_identifier")

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
  select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier,
         hierarchy, activity_status, flow_type, activity_id,
         activity_title, activity_description, country_code, start_date, end_date,
         country_name, country_percentage, sector_code, sector_name,
         policy_marker_code, policy_marker_name, policy_significance, climate_focus,
         sector_percentage, partner, partner_role, 
         budget_status, amount, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date()) 

# Save to Rdata file
saveRDS(activity_list, file = "activity_list.rds")

# Save to Excel
write_xlsx(x = list(`IATI research` = activity_list), 
           path = "IATI research activities.xlsx")
          # path = "https:\\dfid.sharepoint.com\sites\ts-198\IATI research activities.xlsx")

