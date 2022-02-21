# --------------------------------------------------------------- #
# Script 1
# Extract ODA research & innovation (R&I) activity information from 
# UK government departments' IATI data 
# --------------------------------------------------------------- #

# 1) Extract list of OECD research sector codes from IATI ------

# Prepare results data frame and counters
sector_list <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new sector codes returned
while (page == 1 | new_rows > 0) {
  x <- nrow(sector_list)
  sector_list <- sector_extract(page, sector_list)
  page <- page + 1
  y <- nrow(sector_list)
  new_rows = y - x
}

# Keep research/innovation/tech codes only (11)
sector_list_research <- sector_list %>% 
  filter(str_detect(str_to_lower(name), "research") | 
           str_detect(str_to_lower(name), "higher education") |
           str_detect(str_to_lower(name), "information and communication technology"))


# 2) Extract ALL activities from relevant UK government departments --------

# Define UK government department IATI org IDs
organisation_codes <- c("GB-GOV-1",  # FCDO
                        "GB-GOV-7",  # Defra
                        "GB-GOV-10", # DHSC
                        "GB-GOV-12", # DCMS
                        "GB-GOV-13", # BEIS
                        "GB-GOV-15", # DIT
                        "GB-GOV-50") # Prosperity Fund

# Prepare output data frame
uk_gov_list_final <- data.frame()

# Extract activity data for each government department
for (org in organisation_codes) {
  new_rows <- 0
  page <- 1

  while (page == 1 | new_rows > 0) {
    print(paste0(org, "-", page))
    x <- nrow(uk_gov_list_final)
    uk_gov_list_final <- org_activity_extract(page, org, uk_gov_list_final)
    page <- page + 1
    y <- nrow(uk_gov_list_final)
    new_rows = y - x
  }
}

# Save output data
saveRDS(uk_gov_list_final, file = "Outputs/uk_gov_list_final.rds")
# uk_gov_list_final <- readRDS(file = "Outputs/uk_gov_list_final.rds")


# 3) Filter to keep ODA R&I activities only ------
# (via the IATI "RI" tag field eventually - not all gov departments use this yet)

# Unnest tags
uk_gov_ri_programmes <- uk_gov_list_final %>%
  filter(lengths(tag) != 0) %>%
  unnest(col = tag) %>% 
  select(-narrative, -vocabulary_uri, -vocabulary.code, -vocabulary.name)

# Save list of tagged research & innovation activities
ri_iati_activities <- uk_gov_ri_programmes %>% 
  filter(code == "RI") %>% 
  select(iati_identifier) %>% 
  unique() %>% 
  mutate(tag = "RI")

saveRDS(ri_iati_activities, file = "Outputs/ri_iati_activities.rds")
# ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities.rds")


# Filter list of gov department IATI activities 

uk_gov_list_filtered <- uk_gov_list_final %>% 
  select(-tag) %>% 
  left_join(ri_iati_activities, by = "iati_identifier") %>% 
  filter((reporting_org.ref %in% c("GB-GOV-7", "GB-GOV-10", "GB-GOV-15", "GB-GOV-50") | # Include everything from these gov departments
          str_detect(iati_identifier, "GB-GOV-3") |                               # Include everything ex-FCDO
          !is.na(tag) |                                                           # Include tagged R&I programmes
          str_detect(iati_identifier, "NEWT|Newton|NF|GCRF|NIHR|GAMRIF|UKVN")),   # Include BEIS Newton/GCRF and DHSC GHS/GHR activities
          default_flow_type == "ODA")                                             # Restrict to ODA funding only


# 4) Unnest activity information -----------

# Extract basic activity information - hierarchy and status
gov_list_base <- uk_gov_list_filtered %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name) %>% 
  unique()


# A) Unlist activity title and description
gov_list_unnest_1 <- uk_gov_list_filtered %>% 
   # title
  filter(lengths(title.narrative) != 0) %>%
  unnest(cols = title.narrative) %>% 
  rename(activity_title = text) %>% 
   # description
  unnest(cols = description) %>% 
  mutate(type.name = coalesce(type.name, "General")) %>% 
  select(iati_identifier, activity_title, type.name, narrative) %>% 
  unnest(cols = narrative) %>%     
  unique()

      # Summarise records with multiple "General" descriptions
      gov_list_unnest_1 <- gov_list_unnest_1 %>% 
            group_by(iati_identifier, activity_title, type.name) %>% 
            summarise(text = paste(coalesce(text, ""), collapse = "\n\n")) %>% 
            spread(key = type.name, value = text) %>% 
            mutate(activity_description = if_else(!is.na(Objectives), paste0(General, "\n\n", Objectives), General)) %>% 
            ungroup()



# B) Unlist recipient countries
gov_list_unnest_2 <- uk_gov_list_filtered %>%
  filter(lengths(recipient_country) != 0) %>%
  unnest(cols = recipient_country) %>% 
  select(iati_identifier, country.name) %>% 
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(recipient_country = paste(coalesce(country.name, ""), collapse = ", ")) %>% 
  ungroup()



# C) Unlist research sectors
gov_list_unnest_3 <- uk_gov_list_filtered %>%
  filter(lengths(sector) != 0) %>%
  unnest(cols = sector) %>% 
  select(iati_identifier, sector.name) %>% 
  filter(sector.name %in% sector_list_research$name) %>%  # keep research sectors only
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(sector_name = paste(coalesce(sector.name, ""), collapse = ", ")) %>% 
  ungroup()
    


# D) Unlist implementing organisations
gov_list_unnest_4 <- uk_gov_list_filtered %>% 
  filter(lengths(participating_org) != 0) %>%
  unnest(cols = participating_org) %>% 
  select(iati_identifier, role.name, narrative, ref) %>% 
  unnest(cols = narrative) %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name == "Implementing") %>% 
  unique()

    # Add country locations based on IATI org references or lookup
    # (takes ~5 mins to run)
    gov_list_unnest_4 <- gov_list_unnest_4 %>%
         # Extract 2 digit country code from org references (where populated)
      mutate(country_code = if_else((!is.na(ref) & substr(ref,3,3) == "-" & !(substr(ref,1,2) %in% c("XI", "XM"))), 
                                    substr(ref,1,2), "")) %>% 
         # Look up country from both country code and organisation name
      mutate(org_country_iati = map(country_code, country_code_to_name),
             org_country_other = map(text, org_country_lookup)) %>% 
      mutate(org_country_iati = unlist(org_country_iati),
             org_country_other = unlist(org_country_other)) %>% 
        # Take best of both country lookup results
      mutate(partner_country = coalesce(org_country_iati, org_country_other)) %>% 
      select(-org_country_iati, -org_country_other)

    # Summarise partner org countries and names
    gov_list_unnest_4_countries <- gov_list_unnest_4 %>% 
      select(iati_identifier, partner_country) %>% 
      unique() %>% 
      filter(!is.na(partner_country)) %>% 
      group_by(iati_identifier) %>% 
      summarise(partner_country = paste(partner_country, collapse = ", "))
    
    gov_list_unnest_4_partners <- gov_list_unnest_4 %>% 
      select(iati_identifier, text) %>% 
      unique() %>% 
      filter(!is.na(text)) %>% 
      group_by(iati_identifier) %>% 
      summarise(partner = paste(coalesce(text, ""), collapse = ", ")) 
    
    # Add partner name and country info to master dataset
    gov_list_unnest_4 <- gov_list_unnest_4 %>% 
      select(-text, -partner_country) %>% 
      left_join(gov_list_unnest_4_partners, by = "iati_identifier") %>% 
      left_join(gov_list_unnest_4_countries, by = "iati_identifier")


# E) Unlist extending organisations
gov_list_unnest_5 <- uk_gov_list_filtered %>% 
  filter(lengths(participating_org) != 0) %>%
  unnest(cols = participating_org) %>% 
  select(iati_identifier, role.name, narrative) %>% 
  unnest(cols = narrative) %>% 
  filter(role.name == "Extending") %>% 
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(extending_org = paste(coalesce(text, ""), collapse = ", ")) %>% 
  ungroup()


# F) Unlist reporting department
gov_list_unnest_6 <- uk_gov_list_filtered %>% 
  filter(lengths(reporting_org.narrative) != 0) %>%
  unnest(cols = reporting_org.narrative) %>% 
  select(iati_identifier, 
         reporting_org_ref = reporting_org.ref, 
         reporting_org = text) %>% 
  unique()


# G) Unlist and aggregate budgets
gov_list_unnest_7 <- uk_gov_list_filtered %>% 
  filter(lengths(budget) != 0) %>%
  unnest(cols = budget) %>% 
  select(iati_identifier, 
         budget_status = status.name, 
         amount = value.value, 
         currency = value.currency.code,
         period_start,
         period_end)

    # Find activities with multiple budgets for same period (i.e. indicative and committed)
    multiple_budgets <- gov_list_unnest_7 %>% 
      select(iati_identifier, budget_status, period_start, period_end) %>%
      unique() %>% 
      group_by(iati_identifier, period_start, period_end) %>% 
      summarise(count = n()) %>% 
      filter (count > 1)
    
    # Keep only the committed budget in these cases
    gov_list_unnest_7 <- gov_list_unnest_7 %>% 
      filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
             budget_status == "Committed")
    
    # Sum to get total budget per activity
    gov_list_unnest_7 <- gov_list_unnest_7 %>% 
      group_by(iati_identifier, currency) %>% 
      summarise(period_start = min(period_start),
                period_end = max(period_end),
                amount = sum(amount))
    

# H) Unlist start/end dates
gov_list_unnest_8 <- uk_gov_list_filtered %>% 
  filter(lengths(activity_date) != 0) %>%
  unnest(cols = activity_date) %>% 
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
  left_join(gov_list_unnest_7, by = "iati_identifier") %>% 
  left_join(gov_list_unnest_8, by = "iati_identifier")

# Remove non-research activities for all departments apart from BEIS, DHSC, FCDO based on sector information
gov_list <- gov_list %>% 
  filter(reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10", "GB-GOV-13") & !str_detect(iati_identifier, "GB-GOV-3") |
           !is.na(sector_name))  

# Reorder columns and add date of refresh
gov_list <- gov_list %>% 
  select(reporting_org_ref, reporting_org, 
         iati_identifier, hierarchy, activity_status,
         activity_title, activity_description, start_date, end_date,
         recipient_country, sector_name,
         partner, partner_country, extending_org,
         amount, period_start, period_end, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())

# Add Fund label
gov_list <- gov_list %>%  
  mutate(fund = case_when(
    str_detect(iati_identifier, "Newton|NEWT|NF") ~ "BEIS - Newton Fund",
    str_detect(iati_identifier, "GCRF") ~ "BEIS - Global Challenges Research Fund (GCRF)",
    str_detect(iati_identifier, "UKVN") ~ "DHSC - Global Health Security - UK Vaccine Network",
    str_detect(iati_identifier, "GAMRIF") ~ "DHSC - Global Health Security - GAMRIF",
    (str_detect(iati_identifier, "NIHR") | str_detect(activity_title, "NIHR")) ~ "DHSC - Global Health Research - Programmes",
    str_detect(iati_identifier, "ICF") ~ "BEIS - International Climate Finance (ICF)",
    str_detect(iati_identifier, "Chevening") ~ "FCDO - Chevening Scholarships",
    str_detect(iati_identifier, "GB-1-|GB-GOV-1-") ~ "FCDO Research - Programmes",
    reporting_org_ref == "GB-GOV-10" ~ "DHSC - Global Health Research - Partnerships",
    TRUE ~ "Other"
  ))

# Correct Funder names
gov_list <- gov_list %>%  
  mutate(reporting_org = case_when(
    reporting_org_ref == "GB-GOV-1" ~ "Foreign, Commonwealth and Development Office",
    reporting_org_ref == "GB-GOV-10" ~ "Department of Health and Social Care",
    reporting_org_ref == "GB-GOV-12" ~ "Department for Digital, Culture, Media and Sport",
    TRUE ~ reporting_org
  )) %>% 
  mutate(reporting_org = str_replace_all(reporting_org, "UK - ", ""))


# 5) Account for parent-child hierarchies -----------
# Extract detail at child activity level (for DHSC and FCDO) and ensure
# spend is not being double-counted for FCDO

gov_list_final <- gov_list %>% 
  filter((reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10") & hierarchy == 2) | # FCDO, DHSC - keep child activities 
            str_detect(iati_identifier, "GB-GOV-3") |                           # keep ex-FCO activities
            reporting_org_ref %in% c("GB-GOV-7", "GB-GOV-12", "GB-GOV-13", "GB-GOV-50")) # Defra, DCMS, BEIS, Prosperity Fund - keep parent activities


# Join on FCDO parent (programme) descriptions to child (component) activities
gov_list_final <- gov_list_final %>%
     # Extract FCDO programme activity ID
  mutate(programme_id = if_else(hierarchy == 2 & reporting_org_ref == "GB-GOV-1",
                                substr(iati_identifier, 1, nchar(iati_identifier)-4), NA_character_)) %>%
     # Join on programme title
  left_join(select(gov_list_unnest_1,
                   iati_identifier,
                   programme_title = activity_title,
                   programme_description = activity_description),
            by = c("programme_id" = "iati_identifier")) %>%
  mutate(activity_description = if_else(reporting_org_ref == "GB-GOV-1",
                                        programme_description,
                                        activity_description))



# 6) Save to Rdata file ----
saveRDS(gov_list_final, file = "Outputs/gov_list_final.rds")
# gov_list_final <- readRDS("Outputs/gov_list_final.rds") 

# Clear environment variables
rm(uk_gov_list_filtered, gov_list, gov_list_base, gov_list_unnest_1, gov_list_unnest_2, gov_list_unnest_3, 
   gov_list_unnest_4, gov_list_unnest_4_partners, gov_list_unnest_4_countries, gov_list_unnest_5, gov_list_unnest_6, 
   gov_list_unnest_7, gov_list_unnest_8, multiple_budgets, sector_list, sector_list_research, uk_gov_ri_programmes,
   gov_list_final, organisation_codes, new_rows, page, x, y)

