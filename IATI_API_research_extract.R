# --------------------------------------------------------------- #
# Test extracting activities in South Africa from UK institutions #
# --------------------------------------------------------------- #

# install.packages("jsonlite")
# install.packages("httr")

library(jsonlite)
library(httr)
library(tidyverse)
library(writexl)

# path <- "https://test-datastore.iatistandard.org/api/activities/?format=json&recipient_country=SL,MM&fields=iati_identifier,title"
#   "https://iati.cloud/search/activity?q=reporting_org_ref:(GB-GOV-1) AND recipient_country_code:(ZA)&wt=xslt&tr=activity-csv.xsl&rows=5000000"


# 1) Extract list of sector codes from IATI -----------------------------

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
      filter(str_detect(str_to_lower(name), "research"))


# 2) Extract all research activities by BEIS, DFID, DHSC, FCO, Defra -----------

# Set strings for API URL
sector_codes <- paste(sector_list_research$code, collapse=",")
organisation_codes <- "GB-GOV-1,GB-GOV-3,GB-GOV-7,GB-GOV-10,GB-GOV-13,"

# Function to extract research projects
activity_extract <- function(page) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/activities/?format=json&reporting_org_identifier=", organisation_codes, "&sector=", sector_codes, "&fields=iati_identifier,other_identifier,reporting_org,default_flow_type,budgets,policy_markers,activity_status,hierarchy,title,descriptions,participating_organisations,related_activities&page_size=20&page=", page)
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

# Prepare results data frame and counters
activity_list_final <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(activity_list_final)
  activity_list_final <- activity_extract(page)
  page <- page + 1
  y <- nrow(activity_list_final)
  new_rows = y - x
}

# Save to Rdata file
saveRDS(activity_list_final, file = "activity_list_final.rds")
# Restore the object
activity_list_final <- readRDS(file = "activity_list_final.rds")


# Extract base activity information - hierarchy and status
activity_list_base <- activity_list_final %>% 
                          select(iati_identifier, hierarchy, 
                                 activity_status = activity_status.name,
                                 flow_type = default_flow_type.name)



# 1) Unlist activity title and description
activity_list_unnest_1 <- activity_list_final %>% 
  unnest(cols = title.narratives,
         keep_empty = TRUE) %>% 
  select(-language.code, -language.name) %>% 
  rename(activity_title = text) %>% 
  unnest(cols = descriptions,
         keep_empty = TRUE) %>% 
  filter(coalesce(type.name, "General") == "General") %>% 
  select(iati_identifier, activity_title, narratives) %>% 
  unnest(cols = narratives,
         keep_empty = TRUE) %>%     
  group_by(iati_identifier, activity_title) %>%
  summarise(activity_description = paste(coalesce(text, ""), collapse = "; "))


# 2) Unlist recipient countries
activity_list_unnest_2 <- activity_list_final %>% 
  unnest(cols = recipient_countries,
         keep_empty = TRUE) %>% 
  select(iati_identifier, percentage, country.code, country.name) %>% 
  group_by(iati_identifier) %>%
  summarise(country_code = paste(coalesce(country.code, ""), collapse = ", "),
            country_name = paste(coalesce(country.name, ""), collapse = ", "),
            country_percentage = paste(coalesce(percentage, 100), collapse = ", "))

# 3) Unlist sectors
activity_list_unnest_3 <- activity_list_final %>% 
  unnest(cols = sectors,
         keep_empty = TRUE) %>% 
  select(iati_identifier, sector.code, sector.name, percentage) %>% 
  group_by(iati_identifier) %>%
  summarise(sector_code = paste(coalesce(sector.code, ""), collapse = ", "),
            sector_name = paste(coalesce(sector.name, ""), collapse = ", "),
            sector_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# 4) Unlist implementing organisations
activity_list_unnest_4 <- activity_list_final %>% 
  unnest(cols = participating_organisations,
         keep_empty = TRUE) %>% 
  select(iati_identifier, activity_id, role.name, narratives) %>% 
  unnest(cols = narratives,
         keep_empty = TRUE) %>% 
  select(-language.code, -language.name) %>% 
  filter(role.name == "Implementing") %>% 
  group_by(iati_identifier) %>%
  summarise(activity_id = paste(coalesce(activity_id, ""), collapse = ", "),
            partner = paste(coalesce(text, ""), collapse = ", "),
            partner_role = paste(coalesce(role.name, ""), collapse = ", "))

# 5) Unlist reporting department
activity_list_unnest_5 <- activity_list_final %>% 
  unnest(cols = reporting_org.narratives,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text)

# 6) Unlist and aggregate committments
activity_list_unnest_6 <- activity_list_final %>% 
  unnest(cols = budgets,
         keep_empty = TRUE) %>% 
  select(iati_identifier, 
         budget_status = status.name, 
         amount = value.value, 
         currency = value.currency.code) %>% 
  group_by(iati_identifier, budget_status, currency) %>% 
  summarise(amount = sum(amount), 0) %>% 
  filter(!is.na(budget_status))


# 7) Unlist and aggregate policy markers
activity_list_unnest_7 <- activity_list_final %>% 
  unnest(cols = policy_markers,
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


# Join unnested info to original data
activity_list <- activity_list_base %>% 
                    left_join(activity_list_unnest_1, by = "iati_identifier") %>%
                    left_join(activity_list_unnest_2, by = "iati_identifier") %>%
                    left_join(activity_list_unnest_3, by = "iati_identifier") %>%
                    left_join(activity_list_unnest_4, by = "iati_identifier") %>% 
                    left_join(activity_list_unnest_5, by = "iati_identifier") %>% 
                    left_join(activity_list_unnest_6, by = "iati_identifier") %>% 
                    left_join(activity_list_unnest_7, by = "iati_identifier")

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
                     select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier, activity_id,
                            hierarchy, activity_status, flow_type,
                            activity_title, activity_description, country_code,
                            country_name, country_percentage, sector_code, sector_name,
                            policy_marker_code, policy_marker_name, policy_significance, climate_focus,
                            sector_percentage, partner, partner_role, 
                            budget_status, amount, currency) %>% 
                      mutate(refresh_date = Sys.Date()) 


# Save to Excel
write_xlsx(x = list(`IATI research` = activity_list), 
           path = "IATI research activities.xlsx")


