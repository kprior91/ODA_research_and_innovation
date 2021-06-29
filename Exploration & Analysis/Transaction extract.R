
# Read in ex-FCO activities
uk_gov_list_final <- readRDS(file = "Outputs/uk_gov_list_final.rds") %>% 
  filter(str_detect(iati_identifier, "Chevening"))

# Extract base activity information - hierarchy and status
gov_list_base <- uk_gov_list_final %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         flow_type = default_flow_type) %>% 
  mutate(activity_id = "") %>% 
  unique()

# A) Unlist activity title and description
gov_list_unnest_1 <- uk_gov_list_final %>% 
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
gov_list_unnest_2 <- uk_gov_list_final %>% 
  unnest(cols = recipient_country,
         keep_empty = TRUE) %>% 
  select(iati_identifier, percentage, country.code, country.name) %>% 
  group_by(iati_identifier) %>%
  unique() %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  summarise(country_code = paste(coalesce(country.code, ""), collapse = ", "),
            country_name = paste(coalesce(country.name, ""), collapse = ", "),
            country_percentage = paste(coalesce(percentage, 100), collapse = ", "))


# D) Unlist implementing organisations
gov_list_unnest_3 <- uk_gov_list_final %>% 
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

gov_list_unnest_3_countries <- gov_list_unnest_3 %>% 
  select(iati_identifier, partner_country) %>% 
  unique() %>% 
  filter(!is.na(partner_country)) %>% 
  group_by(iati_identifier) %>% 
  summarise(partner_country = paste(partner_country, collapse = ", "))

gov_list_unnest_3_partners <- gov_list_unnest_3 %>% 
  select(iati_identifier, text, role.name, ref) %>% 
  unique() %>% 
  filter(!is.na(text)) %>% 
  group_by(iati_identifier) %>% 
  summarise(partner = paste(coalesce(text, ""), collapse = ", "),
            partner_role = paste(coalesce(role.name, ""), collapse = ", "),
            partner_ref = paste(coalesce(ref, ""), collapse = ", ")) 

gov_list_unnest_3 <- gov_list_unnest_4_partners %>% 
  left_join(gov_list_unnest_4_countries, by = "iati_identifier")


# G) Unlist and aggregate committments
gov_list_unnest_4 <- uk_gov_list_final %>% 
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

# I) Unlist start/end dates
gov_list_unnest_5 <- uk_gov_list_final %>% 
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
  left_join(gov_list_unnest_5, by = "iati_identifier") 

# Reorder columns and add date of refresh
gov_list <- gov_list %>% 
  select(iati_identifier,
         hierarchy, activity_status, flow_type, activity_id,
         activity_title, General, country_code, start_date, end_date,
         country_name, country_percentage, 
         partner, partner_role, partner_ref, partner_country,
         budget_status, amount, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())


# Extract spend by year

id <- "GB-GOV-3-CHEVENING-SCHOLARSHIPS-AF"
path <- "https://iatidatastore.iatistandard.org/api/transactions/?iati_identifier=GB-CHC-326859-6ZNR-CQ4P-NY&fields=value,transaction_date,transaction_type&format=json"
path <- "https://iatidatastore.iatistandard.org/api/transactions/?iati_identifier=GB-GOV-3-CHEVENING-SCHOLARSHIPS-AF&fields=value,transaction_date,transaction_type&format=json"

# Function to extract all UK government department activities
transactions_extract <- function(id) {
  path <- paste0("https://iatidatastore.iatistandard.org/api/transactions/?iati_identifier=", id, "&fields=value,transaction_date&format=json")
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
