# --------------------------------------------------------------- #
# Script 1
# Extract ODA research & innovation (R&I) activity information from 
# UK government departments' IATI data 
# --------------------------------------------------------------- #

# 1) Extract list of OECD research sector codes from IATI ------


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


specific_org_extract <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- org_activity_extract(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

# specific_org_extract("GB-GOV-15")

uk_gov_list_final <- lapply(organisation_codes, specific_org_extract)
uk_gov_list_final = rbindlist(uk_gov_list_final, fill=T)


# Save output data
# You can bypass the previous step by reading it in from the Outputs folder
#saveRDS(uk_gov_list_final, file = "Outputs/uk_gov_list_final_kp.rds")
uk_gov_list_final <- readRDS(file = "Outputs/uk_gov_list_final_kp.rds")


# 3) Filter to keep ODA R&I activities only ------
# (via the IATI "RI" tag field eventually - not all gov departments use this yet)

# Unnest tags
uk_gov_ri_programmes <- uk_gov_list_final %>%
  filter(lengths(tag_code) != 0) %>%
  unnest(col = tag_code)

# Save list of tagged research & innovation activities
ri_iati_activities <- uk_gov_ri_programmes %>% 
  filter(tag_code == "RI") %>% 
  select(iati_identifier) %>% 
  unique() %>% 
  mutate(tag_code = "RI")

# You can bypass the previous step by reading it in from the Outputs folder
#saveRDS(ri_iati_activities, file = "Outputs/ri_iati_activities_kp.rds")
ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities_kp.rds")


# Filter list of gov department IATI activities 

uk_gov_list_filtered <- uk_gov_list_final %>% 
  select(-tag_code) %>% 
  left_join(ri_iati_activities, by = "iati_identifier") %>% 
  filter((reporting_org_ref %in% c("GB-GOV-7", "GB-GOV-10", "GB-GOV-15", "GB-GOV-50") | # Include everything from these gov departments
          str_detect(iati_identifier, "GB-GOV-3") |                               # Include everything ex-FCDO
          !is.na(tag_code) |                                                      # Include tagged R&I programmes
          str_detect(iati_identifier, "NEWT|Newton|NF|GCRF|NIHR|GAMRIF|UKVN")),   # Include BEIS Newton/GCRF and DHSC GHS/GHR activities
          default_flow_type_code == "10")                                             # Restrict to ODA funding only


# 4) Unnest activity information -----------

# Extract basic activity information - hierarchy and status
gov_list_base <- uk_gov_list_filtered %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status_code) %>% 
  unique() %>% 
  mutate(activity_status = str_replace(activity_status, "1", "Pipeline/identification"),
         activity_status = str_replace(activity_status, "2", "Implementation"),
         activity_status = str_replace(activity_status, "3", "Finalisation"),
         activity_status = str_replace(activity_status, "4", "Closed"),
         activity_status = str_replace(activity_status, "5", "Cancelled"),
         activity_status = str_replace(activity_status, "6", "Suspended"))


# A) Unlist activity title and description
gov_list_unnest_1 <- uk_gov_list_filtered %>% 
   # title
  filter(lengths(title_narrative) != 0) %>%
  unnest(cols = title_narrative) %>% 
  #rename(activity_title = text) %>% 
   # description
  unnest(cols = description_narrative) %>% 
  #mutate(type.name = coalesce(type.name, "General")) %>% 
  select(iati_identifier, title_narrative, description_narrative) %>% 
  #unnest(cols = narrative) %>%     
  unique() %>% rename(activity_title = title_narrative, activity_description = description_narrative)

      # Summarise records with multiple "General" descriptions
      # gov_list_unnest_1 <- gov_list_unnest_1 %>% 
      #       group_by(iati_identifier, title_narrative, type.name) %>% 
      #       summarise(text = paste(coalesce(text, ""), collapse = "\n\n")) %>% 
      #       spread(key = type.name, value = text) %>% 
      #       mutate(activity_description = if_else(!is.na(Objectives), paste0(General, "\n\n", Objectives), General)) %>% 
      #       ungroup()



# B) Unlist recipient countries
gov_list_unnest_2_country <- uk_gov_list_filtered %>%
  #filter(lengths(recipient_country_code) != 0) %>%
  unnest(c(recipient_country_code)) %>% 
  select(iati_identifier, recipient_country_code)

gov_list_unnest_2_country$recipient_country <- countrycode_list$name[match(gov_list_unnest_2_country$recipient_country_code,countrycode_list$code)]
gov_list_unnest_2_country <- gov_list_unnest_2_country %>% select(-recipient_country_code) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))


gov_list_unnest_2_region <- uk_gov_list_filtered %>%
  #filter(lengths(recipient_country_code) != 0) %>%
  unnest(c(recipient_region_code)) %>% 
  select(iati_identifier, recipient_region_code)

gov_list_unnest_2_region$recipient_region <- regioncode_list$name[match(gov_list_unnest_2_region$recipient_region_code,regioncode_list$code)]
gov_list_unnest_2_region <- gov_list_unnest_2_region %>% select(-recipient_region_code) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(recipient_region = paste(coalesce(recipient_region, ""), collapse = ", "))

gov_list_unnest_2 <- full_join(gov_list_unnest_2_country, gov_list_unnest_2_region, by = "iati_identifier")
gov_list_unnest_2$recipient_country_region <- coalesce(gov_list_unnest_2$recipient_country, gov_list_unnest_2$recipient_region)
gov_list_unnest_2 <- gov_list_unnest_2 %>% 
  mutate(across(c(recipient_country_region), na_if, "Developing countries, unspecified")) %>%
  select(-recipient_country, -recipient_region)

# C) Unlist research sectors
gov_list_unnest_3 <- uk_gov_list_filtered %>%
  filter(lengths(sector_code) != 0) %>%
  unnest(cols = sector_code) %>% 
  select(iati_identifier, sector_code) %>% 
  filter(sector_code %in% sector_list_research$code) %>%  # keep research sectors only
  unique()

gov_list_unnest_3$sector_name <- sector_list_research$name[match(gov_list_unnest_3$sector_code,sector_list_research$code)]

gov_list_unnest_3 <- gov_list_unnest_3 %>% select(-sector_code) %>%
  unique() %>%
  group_by(iati_identifier) %>%
  summarise(sector_name = paste(coalesce(sector_name, ""), collapse = ", "))
    


# D) Unlist implementing organisations
gov_list_unnest_4 <- uk_gov_list_filtered %>% 
  #filter(lengths(participating_org_ref) != 0) %>%
  #unnest(c(participating_org_ref, participating_org_narrative)) %>% 
  select(iati_identifier, participating_org_role, participating_org_narrative, participating_org_ref) %>% 
  unnest(c(participating_org_role, participating_org_narrative, participating_org_ref)) %>% 
  filter(participating_org_role == "4") %>% 
  unique()

    # Add country locations based on IATI org references or lookup
    # (takes ~5 mins to run)
     gov_list_unnest_4 <- gov_list_unnest_4 %>%
      # Extract 2 digit country code from org references (where populated)
      mutate(country_code = if_else((!is.na(participating_org_ref) & substr(participating_org_ref,3,3) == "-" & !(substr(participating_org_ref,1,2) %in% c("XI", "XM"))), 
                                    substr(participating_org_ref,1,2), ""))
  
    # Function searches the org title (e.g. Kenya) and returns a country if its present in the name
    gov_list_unnest_4$country_name2 <- map(gov_list_unnest_4$participating_org_narrative, org_country_lookup)
    gov_list_unnest_4$country_name2 <- as.character(gov_list_unnest_4$country_name2)
    gov_list_unnest_4$country_name2 <- ifelse(gov_list_unnest_4$country_name2 == "NA", "", gov_list_unnest_4$country_name2)
    
    
    # check if input is a valid 2-digit country code and then does a lookup to the countrycode_list
    gov_list_unnest_4$country_name <- ifelse(is.na(gov_list_unnest_4$country_code) | nchar(gov_list_unnest_4$country_code) < 2, NA, countrycode_list$name[match(gov_list_unnest_4$country_code,countrycode_list$code)])
    gov_list_unnest_4$country_name <- ifelse(is.na(gov_list_unnest_4$country_name), "", gov_list_unnest_4$country_name)
    
    gov_list_unnest_4[gov_list_unnest_4 == ""] <- NA
    
    # This needs fixing because currently I don't know why it's only picking one of the country names when there might be several 
    gov_list_unnest_4$partner_country <- coalesce(gov_list_unnest_4$country_name, gov_list_unnest_4$country_name2)
    
    gov_list_unnest_4 <- gov_list_unnest_4%>% select(-country_name, -country_name2)
    
    # Summarise partner org countries and names
    gov_list_unnest_4_countries <- gov_list_unnest_4 %>% 
      select(iati_identifier, partner_country) %>% 
      unique() %>% 
      filter(!is.na(partner_country)) %>% 
      group_by(iati_identifier) %>% 
      summarise(partner_country = paste(partner_country, collapse = ", "))
    
    gov_list_unnest_4_partners <- gov_list_unnest_4 %>% 
      select(iati_identifier, participating_org_narrative) %>% 
      unique() %>% 
      filter(!is.na(participating_org_narrative)) %>% 
      group_by(iati_identifier) %>% 
      summarise(partner = paste(coalesce(participating_org_narrative, ""), collapse = ", ")) 
    
    # Add partner name and country info to master dataset
    gov_list_unnest_4 <- gov_list_unnest_4 %>% 
      select(-participating_org_narrative, -participating_org_ref, -country_code, -participating_org_role, -partner_country) %>% 
      left_join(gov_list_unnest_4_partners, by = "iati_identifier") %>% 
      left_join(gov_list_unnest_4_countries, by = "iati_identifier") %>%
      unique()


# E) Unlist extending organisations
gov_list_unnest_5 <- uk_gov_list_filtered %>% 
    #filter(lengths(participating_org_ref) != 0) %>%
    #unnest(c(participating_org_ref, participating_org_narrative)) %>% 
    select(iati_identifier, participating_org_role, participating_org_narrative) %>% 
    unnest(c(participating_org_role, participating_org_narrative)) %>% 
    filter(participating_org_role == "3") %>% 
    unique() %>%  select(-participating_org_role) %>% rename(extending_org = participating_org_narrative)


# F) Unlist reporting department
gov_list_unnest_6 <- uk_gov_list_filtered %>% 
  filter(lengths(reporting_org_narrative) != 0) %>%
  unnest(cols = reporting_org_narrative) %>% 
  select(iati_identifier, 
         reporting_org_ref,
         reporting_org_narrative) %>% 
  unique() %>% rename(reporting_org = reporting_org_narrative)


# G) Unlist and aggregate budgets
gov_list_unnest_7 <- uk_gov_list_filtered %>% 
  filter(lengths(budget_value) != 0) %>%
  unnest(c(budget_status,budget_value,budget_value_currency,budget_period_start_iso_date,budget_period_end_iso_date)) %>% 
  select(iati_identifier, 
         budget_status, 
         budget_value, 
         budget_value_currency,
         budget_period_start_iso_date,
         budget_period_end_iso_date)

    # Find activities with multiple budgets for same period (i.e. indicative and committed)
    multiple_budgets <- gov_list_unnest_7 %>% 
      select(iati_identifier, budget_status, budget_period_start_iso_date, budget_period_end_iso_date) %>%
      unique() %>% 
      group_by(iati_identifier, budget_period_start_iso_date, budget_period_end_iso_date) %>% 
      summarise(count = n()) %>% 
      filter (count > 1)
    
    # Keep only the committed budget in these cases
    gov_list_unnest_7 <- gov_list_unnest_7 %>% 
      filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
             budget_status == "2")
    
    # Sum to get total budget per activity
    gov_list_unnest_7 <- gov_list_unnest_7 %>% 
      group_by(iati_identifier, budget_value_currency) %>% 
      summarise(budget_period_start_iso_date = min(budget_period_start_iso_date),
                budget_period_end_iso_date = max(budget_period_end_iso_date),
                budget_value = sum(budget_value)) %>%
      rename(period_start = budget_period_start_iso_date, period_end = budget_period_end_iso_date, amount = budget_value, currency = budget_value_currency)
    

# H) Unlist start/end dates
gov_list_unnest_8 <- uk_gov_list_filtered %>% 
  filter(lengths(activity_date_iso_date) != 0) %>%
  unnest(cols = c(activity_date_iso_date,activity_date_type)) %>% 
  select(iati_identifier, 
         date = activity_date_iso_date,
         date_type = activity_date_type) %>%
  unique() %>% 
  mutate(date_type = str_replace(date_type, "1", "Planned start"),
         date_type = str_replace(date_type, "2", "Actual start"),
         date_type = str_replace(date_type, "3", "Planned end"),
         date_type = str_replace(date_type, "4", "Actual end")) %>%
  spread(key = date_type, value = date) %>% 
  mutate(start_date = coalesce(`Actual start`, `Planned start`),
         end_date = coalesce(`Actual end`, `Planned end`)) %>% 
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
         recipient_country_region, sector_name,
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
# saveRDS(gov_list_final, file = "Outputs/gov_list_final_kp.rds")
gov_list_final <- readRDS("Outputs/gov_list_final_kp.rds")

# Clear environment variables
rm(uk_gov_list_filtered, gov_list, gov_list_base, gov_list_unnest_1, gov_list_unnest_2, gov_list_unnest_2_country, gov_list_unnest_2_region, gov_list_unnest_3, 
   gov_list_unnest_4, gov_list_unnest_4_partners, gov_list_unnest_4_countries, gov_list_unnest_5, gov_list_unnest_6, 
   gov_list_unnest_7, gov_list_unnest_8, multiple_budgets, sector_list, sector_list_research, uk_gov_ri_programmes,
   gov_list_final, organisation_codes)

