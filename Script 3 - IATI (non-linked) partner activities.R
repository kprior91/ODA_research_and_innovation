# --------------------------------------------------------------- #
# Script 2
# Extract ODA research activities from public IATI data #
# --------------------------------------------------------------- #

### A) Extract data on all partner activities from IATI Registry ----

# Read in linked partner IATI activity info from script 1
red_linked_activites <- readRDS(file = "Outputs/red_linked_activites.rds") %>% 
  rename(iati_id = linked_activity,
         funding_iati_id = programme_id)

# Manually add on other (non-linked) partner activities from Excel
iati_activity_ids <- iati_activity_ids %>% 
  plyr::rbind.fill(red_linked_activites)

# Prepare results data frame and counters
partner_activity_extract <- data.frame()

# IDS example not working: id <- "GB-COH-877338-GV-GOV-1-300708-124" 

# Run extraction, stopping when no new sector codes returned
for (id in iati_activity_ids$iati_id) {

    print(id)
    result <- iati_activity_extract(id)
    partner_activity_extract <- rbind(partner_activity_extract, result)

}

# Save to Rdata file
saveRDS(partner_activity_extract, file = "Outputs/partner_activity_extract.rds")
# partner_activity_extract <- readRDS(file = "Outputs/partner_activity_extract.rds")


### B) Activity extract for specific partner organisations ----

# FCDO-funded partnership activities 

org_code <- c(
              "XM-DAC-47015", # CGIAR
              "XM-DAC-301-2", # IDRC
              "XI-IATI-CABI", # CABI
              "XM-DAC-928",   # WHO
              "DAC-1601",     # Bill & Melinda Gates Foundation
              "XI-IATI-AGR"   # AgResults (Consortium)
              )   

# 1) Activity extract

# Prepare results data frame and counters
org_activity_list <- data.frame()

# Run extraction
for (org in org_code) {
  new_rows <- 0
  page <- 1
  
  while (page == 1 | new_rows > 0) {
    print(paste0(org, "-", page))
    x <- nrow(org_activity_list)
      tryCatch({
      org_activity_list <- org_activity_extract(page, org, org_activity_list)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    page <- page + 1
    y <- nrow(org_activity_list)
    new_rows = y - x
  }
}

# 2.A) Unlist activity titles and subset for those that mention FCDO/DFID
# (Gates only)

partner_activities_via_title <- org_activity_list %>% 
  filter(reporting_org.ref == "DAC-1601") %>% 
  unnest(cols = title.narrative,
         keep_empty = TRUE) %>% 
  filter(str_detect(text, "FCDO|DFID")) %>% 
  mutate(gov_funder = "Foreign, Commonwealth and Development Office",
         fund = "FCDO Research - Partnerships") %>% 
  select(iati_identifier, gov_funder, fund) %>% 
  unique()

# 2.B) Unlist participating funding organisations and subset for FCDO

partner_activities_via_funder <- org_activity_list %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative, ref, activity_id) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  select(-lang.code, -lang.name) %>% 
  filter(role.name %in% c("Funding") | 
           str_detect(iati_identifier, "XI-IATI-AGR") 
           ) %>%   
  unique() %>% 
  filter(ref == "GB-GOV-1" | 
           str_detect(text, "Britain|DFID|FCDO|DHSC|Department of Health and Social Care") |
           str_detect(iati_identifier, "DFID") |
           str_detect(iati_identifier, "XI-IATI-AGR")      # AgResults partially funded
         ) %>%   
  mutate(gov_funder = if_else(str_detect(text, "Health"), "Department of Health and Social Care",
                              "Foreign, Commonwealth and Development Office"),
         fund = case_when(
                   # IDRC GAMRIF projects
                   str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(text, "Health") ~ "Global Health Security - GAMRIF",
                   # Other DHSC partnerships
                   str_detect(text, "Health") ~ "Global Health Research - Partnerships", 
                   # FCDO AgResults
                   str_detect(iati_identifier, "XI-IATI-AGR") ~ "FCDO Research - Partnerships", 
                   TRUE ~ "FCDO Research - Programmes"
                   )) %>% 
  select(iati_identifier, activity_id, gov_funder, fund) %>% 
  unique()

# Combine 2A and 2B
partner_activities <- plyr::rbind.fill(partner_activities_via_title, partner_activities_via_funder)

# Join back to original data
partner_activities <- org_activity_list %>% 
  inner_join(partner_activities, by = "iati_identifier")

# Save to Rdata file
saveRDS(partner_activities, file = "Outputs/partner_activities.rds")
# partner_activities <- readRDS(file = "Outputs/partner_activities.rds")


### C) Combine individual with partner activities (extractions A and B above) ---- 

partner_activity_comb <- plyr::rbind.fill(partner_activity_extract, partner_activities) %>% 
  rename(fcdo_activity_id = activity_id)

rm(partner_activity_extract)
rm(partner_activities)
rm(partner_activities_via_title)
rm(partner_activities_via_funder)
rm(result)
rm(red_linked_activites)

# D) Extract accompanying data ----------------------------------------------

# Extract base activity information - hierarchy and status
activity_list_base <- partner_activity_comb %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         flow_type = default_flow_type,
         fcdo_activity_id,
         gov_funder,
         fund) %>% 
  left_join(select(iati_activity_ids, iati_id, gov_funder), by = c("iati_identifier" = "iati_id")) %>% 
  mutate(gov_funder = coalesce(gov_funder.x, gov_funder.y)) %>% 
  select(-gov_funder.x, -gov_funder.y) %>% 
  unique()


# 1) Unlist activity title and description
activity_list_unnest_1 <- partner_activity_comb %>% 
  unnest(cols = title.narrative,
         keep_empty = TRUE) %>% 
  filter(lang.name == "English") %>% 
  select(-lang.code, -lang.name) %>% 
  rename(activity_title = text) %>% 
  filter(lengths(description) != 0) %>% 
  unnest(cols = description,
         keep_empty = TRUE) %>% 
  mutate(type.name = coalesce(type.name, "General")) %>% 
  select(iati_identifier, activity_title, type.name, narrative) %>% 
  filter(lengths(narrative) != 0) %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>%  
  filter(lang.name == "English") %>% 
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
activity_list_unnest_2 <- partner_activity_comb %>% 
  filter(lengths(recipient_country) != 0) %>% 
  unnest(cols = recipient_country,
         keep_empty = TRUE) %>% 
  select(iati_identifier, country.name) %>% 
  group_by(iati_identifier) %>%
  unique() %>% 
  summarise(country_name = paste(coalesce(country.name, ""), collapse = ", "))

  # Identify activities without recipient countries at activity level
  no_country_info <- partner_activity_comb %>% 
    filter(lengths(recipient_country) == 0)
  
  # Extract transactions for these activities
      
     # Prepare results data frame and counters
      transaction_list <- data.frame()
  
     # Run extraction, stopping when no new transactions are returned
      for (id in no_country_info$iati_identifier) {
        new_rows <- 0
        page <- 1
        
        while (page == 1 | new_rows > 0) {
          print(paste0(id, "-", page))
          x <- nrow(transaction_list)
          transaction_list <- transactions_extract(id, page, transaction_list)
          page <- page + 1
          y <- nrow(transaction_list)
          new_rows = y - x
        }
      }
      
      # Unnest information -----
      transactions_unnest <- transaction_list %>% 
        filter(lengths(recipient_countries) != 0) %>% 
        unnest(cols = recipient_countries,
               keep_empty = TRUE) %>%
        select(-country.url, -recipient_regions) %>% 
        rename(recipient_country = country.name) %>% 
  
        # remove transactions without a country
        filter(!is.na(recipient_country)) %>%      

        # summarise countries 
        select(iati_identifier, recipient_country) %>% 
        unique() %>% 
        group_by(iati_identifier) %>% 
        summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))
      
      
  # Join on transactions country info to rest of dataset
  activity_list_unnest_2_comp <- activity_list_unnest_2 %>% 
    left_join(transactions_unnest, by = "iati_identifier") %>% 
    mutate(recipient_country = coalesce(recipient_country, country_name)) %>% 
    select(-country_name)
  

# 3) Unlist sectors
activity_list_unnest_3 <- partner_activity_comb %>% 
  filter(lengths(sector) != 0) %>% 
  unnest(cols = sector,
         keep_empty = TRUE) %>% 
  select(iati_identifier, sector.code, sector.name, percentage) %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  filter(sector.name != "Vocabulary 99 or 98") %>% 
  group_by(iati_identifier) %>%
  unique()

    # Summarise all sector descriptions for each activity
    activity_list_unnest_3 <- activity_list_unnest_3 %>% 
      summarise(sector_code = paste(coalesce(sector.code, ""), collapse = ", "),
                sector_name = paste(coalesce(sector.name, ""), collapse = ", "))


# 4) Unlist organisations
activity_list_unnest_4 <- partner_activity_comb %>%
  filter(lengths(participating_org) != 0) %>% 
  unnest(cols = participating_org,
         keep_empty = TRUE) %>% 
  select(iati_identifier, role.name, narrative, ref) %>% 
  filter(lengths(narrative) != 0,
         role.name == "Implementing") %>% 
  unnest(cols = narrative,
         keep_empty = TRUE) %>% 
  filter(lang.name == "English") %>% 
  select(-lang.code, -lang.name) %>% 
  unique()

  # Add country locations based on IATI references or lookup
    activity_list_unnest_4 <- activity_list_unnest_4 %>%
      mutate(
        org_country_iati = case_when(        
          str_detect(ref, "GB-") ~ "United Kingdom", 
          str_detect(ref, "US-") ~ "United States", 
          str_detect(ref, "NL-") ~ "Netherlands",
          str_detect(ref, "CA-") ~ "Canada",
          str_detect(ref, "IN-") ~ "India",
          str_detect(ref, "KE-") ~ "Kenya",
          str_detect(ref, "ZA-") ~ "South Africa"),
        org_country_other = map(text, org_country_lookup)) %>% 
      mutate(org_country_other = unlist(org_country_other)) %>% 
      mutate(org_country = coalesce(org_country_iati, org_country_other)) %>% 
      select(-org_country_iati, -org_country_other)
      
  # Save implementing orgs with country to file
    org_names_and_locations_1 <- activity_list_unnest_4 %>% 
      select(project_id = iati_identifier,
             organisation_name = text,
             organisation_country = org_country) %>% 
      mutate(organisation_role = 2) 
    
  # Collapse implementing orgs
    activity_list_unnest_4_partner_names <- activity_list_unnest_4 %>% 
      select(iati_identifier, text) %>% 
      filter(!is.na(text)) %>% 
      unique() %>% 
      group_by(iati_identifier) %>% 
      summarise(partner = paste(coalesce(text, ""), collapse = ", "))
    
    activity_list_unnest_4_partner_countries <- activity_list_unnest_4 %>% 
      select(iati_identifier, org_country) %>% 
      filter(!is.na(org_country)) %>% 
      unique() %>% 
      group_by(iati_identifier) %>% 
      summarise(partner_country = paste(coalesce(org_country, ""), collapse = ", "))

  # Combine in single dataset
  activity_list_unnest_4_final <- activity_list_unnest_4 %>% 
    select(iati_identifier) %>% 
    unique() %>% 
    left_join(activity_list_unnest_4_partner_names, by = "iati_identifier") %>% 
    left_join(activity_list_unnest_4_partner_countries, by = "iati_identifier")


# 5) Unlist publishing org
activity_list_unnest_5 <- partner_activity_comb %>%
  filter(lengths(reporting_org.narrative) != 0) %>% 
  unnest(cols = reporting_org.narrative,
         keep_empty = TRUE) %>% 
  select(iati_identifier, reporting_org_ref = reporting_org.ref, 
         reporting_org_type = reporting_org.type.name,
         reporting_org = text) %>% 
  # Account for IDRC having names in 3 languages
  filter(reporting_org_ref != "XM-DAC-301-2" | reporting_org == "International Development Research Centre") %>% 
  unique()

    # Lookup country
    activity_list_unnest_5 <- activity_list_unnest_5 %>% 
        mutate(
          org_country_iati = case_when(        
            str_detect(reporting_org_ref, "GB-") ~ "United Kingdom", 
            str_detect(reporting_org_ref, "US-") ~ "United States", 
            str_detect(reporting_org_ref, "NL-") ~ "Netherlands",
            str_detect(reporting_org_ref, "CA-") ~ "Canada",
            str_detect(reporting_org_ref, "IN-") ~ "India",
            str_detect(reporting_org_ref, "KE-") ~ "Kenya",
            str_detect(reporting_org_ref, "ZA-") ~ "South Africa"),
          org_country_other = map(reporting_org, org_country_lookup)) %>% 
          mutate(org_country_other = unlist(org_country_other)) %>% 
          mutate(reporting_org_country = coalesce(org_country_iati, org_country_other)) %>% 
          select(-org_country_iati, -org_country_other)

    # Add on to org file to save
    org_names_and_locations_1 <- activity_list_unnest_5 %>% 
          select(project_id = iati_identifier,
                 organisation_name = reporting_org,
                 organisation_country = reporting_org_country) %>% 
          mutate(organisation_role = 1) %>% # leading
          rbind(org_names_and_locations_1) %>% 
          unique()
    
# 6) Unlist and aggregate committments
activity_list_unnest_6 <- partner_activity_comb %>% 
  filter(lengths(budget) != 0) %>% 
  unnest(cols = budget,
         keep_empty = TRUE) %>% 
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

    # Find activities with multiple budgets for the same period
    multiple_budgets <- activity_list_unnest_6 %>% 
      group_by(iati_identifier, period_start, period_end) %>% 
      summarise(count = n()) %>% 
      filter (count > 1)
    
    # Keep only the committed budget in these cases
    activity_list_unnest_6_dedup <- activity_list_unnest_6 %>% 
      filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
               budget_status == "Committed") %>% 
      group_by(iati_identifier, currency) %>%     # budget_status removed
      summarise(period_start = min(period_start),
                period_end = max(period_end),
                amount = sum(amount))


# 7) Unlist start/end dates
activity_list_unnest_7 <- partner_activity_comb %>% 
  unnest(cols = activity_date,
         keep_empty = TRUE) %>% 
  select(iati_identifier, 
         date = iso_date,
         date_type = type.name) %>%
  # take the first date in cases of two of the same time
  group_by(iati_identifier, date_type) %>%
  slice(1) %>%
  spread(key = date_type, value = date) %>% 
  mutate(start_date = coalesce(`Actual start`, `Planned start`),
         end_date = coalesce(`Actual end`, `Planned End`)) %>% 
  select(iati_identifier, start_date, end_date)


# Join unnested info to original data
activity_list <- activity_list_base %>% 
  left_join(activity_list_unnest_1, by = "iati_identifier") %>%
  left_join(activity_list_unnest_2_comp, by = "iati_identifier") %>%
  left_join(activity_list_unnest_3, by = "iati_identifier") %>%
  left_join(activity_list_unnest_4_final, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_5, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_6_dedup, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_7, by = "iati_identifier")


# Assign a reporting org if the reporting partner is implementing 
# the activity themselves
activity_list <- activity_list %>% 
  mutate(reporting_org = coalesce(reporting_org, reporting_org_ref, gov_funder))

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
  mutate(activity_description = coalesce(General, Objectives)) %>% 
  select(iati_identifier, reporting_org_ref, reporting_org, reporting_org_country,
         hierarchy, activity_status, flow_type, fcdo_activity_id,
         activity_title, activity_description, start_date, end_date,
         recipient_country, sector_code, sector_name,
         partner, partner_country, 
         gov_funder, fund,
         amount, period_start, period_end, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())

# Add Fund label
activity_list <- activity_list %>%  
    left_join(select(iati_activity_ids, gov_funder, iati_id, funding_iati_id), 
              by = c("iati_identifier" = "iati_id", "gov_funder")) %>% 
    mutate(programme_id = coalesce(funding_iati_id, fcdo_activity_id)) %>% 
    mutate(fund = coalesce(fund, "FCDO Research - Programmes"),
           gov_funder = coalesce(gov_funder, "Foreign, Commonwealth and Development Office"))

# Add missing FCDO activity IDs
activity_list <- activity_list %>% 
  mutate(programme_id = case_when(str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(activity_title, "CLARE") ~ "GB-GOV-1-300126",
                                  str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(activity_title, "CARIAA") ~ "GB-1-203506", 
                                  str_detect(iati_identifier, "XI-IATI-AGR") ~ "GB-1-203052",
                                  TRUE ~ programme_id))


# Save to Rdata file
saveRDS(activity_list, file = "Outputs/partner_activity_list.rds")
# activity_list <- readRDS(file = "Outputs/partner_activity_list.rds")

# Save org names and countries to file
saveRDS(org_names_and_locations_1, file = "Outputs/org_names_and_locations_1.rds")

# Check funds, funders
table(activity_list$fund)
table(activity_list$gov_funder)
table(activity_list$currency)

# Check specific partner
test1 <- filter(activity_list, str_detect(reporting_org, "World Health Organization"))
