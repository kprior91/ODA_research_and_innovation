# --------------------------------------------------------------- #
# Extract ODA research activities from UK gov funder IATI data #
# --------------------------------------------------------------- #

# 1) Extract list of research sector codes from IATI -----------------------------

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

# Keep research codes only (11)
sector_list_research <- sector_list %>% 
  filter(str_detect(str_to_lower(name), "research") | 
           str_detect(str_to_lower(name), "higher education") |
           str_detect(str_to_lower(name), "information and communication technology"))


# 2) Extract all activities by government publishers -----------

# Set strings for API URL
organisation_codes <- c("GB-GOV-1", "GB-GOV-7", "GB-GOV-10", "GB-GOV-13", "GB-GOV-15", "GB-GOV-50", "GB-GOV-52")

# Prepare results data frame and counters
uk_gov_list_final <- data.frame()

# Run extraction, stopping when no new sector codes returned

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

saveRDS(uk_gov_list_final, file = "Outputs/uk_gov_list_final.rds")
# uk_gov_list_final <- readRDS(file = "Outputs/uk_gov_list_final.rds")


# 3) Restrict to ODA R&I activities -----------
# (via the R&I "tag" field)

# Unnest tags
uk_gov_ri_programmes <- uk_gov_list_final %>% 
  unnest(col = tag,
         keep_empty = TRUE) %>% 
  select(-narrative, -vocabulary_uri, -vocabulary.code, -vocabulary.name)

# Save list of tagged research & innovation programmes
ri_iati_activities <- uk_gov_ri_programmes %>% 
  filter(code == "RI") %>% 
  select(iati_identifier) %>% 
  unique()

saveRDS(ri_iati_activities, file = "Outputs/ri_iati_activities.rds")
# ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities.rds")


# Filter out non-research programmes  

uk_gov_list_filtered <- uk_gov_ri_programmes %>% 
  filter((reporting_org.ref %in% c("GB-GOV-7", "GB-GOV-15", "GB-GOV-50", "GB-GOV-52", "GB-GOV-10") | 
           str_detect(iati_identifier, "GB-GOV-3") |  # Other UK gov deps (including ex-FCO)
           code == "RI" |   # tagged FCDO RED programmes
           str_detect(iati_identifier, "NEWT|Newton|NF|GCRF|NIHR|GAMRIF|UKVN")),   # Keep BEIS Newton/GCRF and DHSC GHS/GHR
         default_flow_type == "ODA")                                            # ODA only


# 4) Unnest activity information -----------

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
         currency = value.currency.code,
         period_start,
         period_end) %>% 
  # take first record for activities with multiple budgets for the same period
  group_by(iati_identifier, budget_status, currency, period_start, period_end) %>% 
  top_n(n = 1) %>% 
  # remove unpopulated budget status records
  filter(!is.na(budget_status)) %>% 
  ungroup()

    # Find activities with multiple budgets
    multiple_budgets <- gov_list_unnest_7 %>% 
      group_by(iati_identifier, period_start, period_end) %>% 
      summarise(count = n()) %>% 
      filter (count > 1)
    
    # Keep only the committed budget in these cases
    gov_list_unnest_7_dedup <- gov_list_unnest_7 %>% 
      filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
             budget_status == "Committed") %>% 
      group_by(iati_identifier, currency) %>% 
      summarise(period_start = min(period_start),
                period_end = max(period_end),
                amount = sum(amount))
    
    # Join on research percentages
    gov_list_unnest_7_dedup <- gov_list_unnest_7_dedup %>% 
      left_join(sector_percentages, by = "iati_identifier") %>% 
      mutate(research_pc = coalesce(research_pc, 100)) %>% 
      mutate(adjusted_amount = (research_pc/100)*amount)


# H) Unlist start/end dates
gov_list_unnest_8 <- uk_gov_list_filtered %>% 
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
  filter(reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10", "GB-GOV-13") & !str_detect(iati_identifier, "GB-GOV-3") |
           !is.na(sector_name))   # removes non-R&I activities for Defra, ex-FCO (GB-GOV-3) and Prosperity Fund

# Reorder columns and add date of refresh
gov_list <- gov_list %>% 
  select(reporting_org_ref, reporting_org_type, reporting_org, iati_identifier,
         hierarchy, activity_status, flow_type, activity_id,
         activity_title, General, Objectives, country_code, start_date, end_date,
         all_countries = country_name, country_percentage, sector_code, sector_name,
         sector_percentage, partner, partner_role, partner_ref, partner_country, extending_org,
         amount, period_start, period_end, currency) %>% 
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
    str_detect(iati_identifier, "Chevening") ~ "Chevening Scholarships",
    str_detect(iati_identifier, "GB-1-|GB-GOV-1-") ~ "FCDO Research - Programmes",
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


# 4) Account for parent-child hierarchies -----------
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
         flow_type == "ODA")

# Join on FCDO programme descriptions at component level
gov_list_final <- gov_list_final %>% 
  left_join(select(gov_list, 
                   iati_identifier, 
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

# Save to Excel (for Power BI dashboard)
write_xlsx(x = list(`IATI research` = gov_list_final), 
           path = "Outputs/IATI research activities.xlsx")

