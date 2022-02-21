# --------------------------------------------------------------- #
# Script 3
# Extract ODA research activities from public IATI data #
# --------------------------------------------------------------- #

### 1) Extract IATI data on RI partner activities (linked or manually identified) ----

# Read in linked partner IATI activity info from script 1
ri_linked_activites <- readRDS(file = "Outputs/ri_linked_activites.rds")

# Manually add on other (non-linked) partner activities from Excel
partner_iati_activity_ids <- unlinked_partner_iati_activity_ids %>% 
  plyr::rbind.fill(ri_linked_activites)

      # Prepare results data frame and counters
      partner_activity_extract <- data.frame()

      # Run extraction, stopping when no new sector codes returned
      for (id in partner_iati_activity_ids$iati_id) {

          print(id)
          result <- iati_activity_extract(id)
          partner_activity_extract <- rbind(partner_activity_extract, result)

      }
      
# Save to Rdata file
saveRDS(partner_activity_extract, file = "Outputs/partner_activity_extract.rds")
# partner_activity_extract <- readRDS(file = "Outputs/partner_activity_extract.rds")

# Join on funder and fund information
partner_activity_extract <- partner_activity_extract %>%       
      left_join(partner_iati_activity_ids, by = "iati_identifier") %>% 
      select(-extending_org)
      

### 2) Activity extract for specific partnership organisations ----

# FCDO (part-)funded partnership activities 

org_code <- c(
              "XM-DAC-47015", # CGIAR
              "XM-DAC-301-2", # IDRC
              "DAC-1601",     # Bill & Melinda Gates Foundation
              "XI-IATI-AGR"   # AgResults (Consortium)
              )   

# 1) Activity extract

        # Prepare results data frame and counters
        org_activity_list <- data.frame()

        # Run extraction (takes few hours to run)
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

        saveRDS(org_activity_list, "Outputs/org_activity_list.rds")
        # org_activity_list <- readRDS("Outputs/org_activity_list.rds")
        
        # 2.A) Unlist activity titles and subset for those that mention FCDO/DFID
        # (identifies FCDO-funded Gates Foundation activities)

        partner_activities_via_title <- org_activity_list %>%
          filter(reporting_org.ref == "DAC-1601") %>%  # Gates org ID
          unnest(cols = title.narrative) %>%
          filter(str_detect(text, "FCDO|DFID")) %>%
          mutate(gov_funder = "Foreign, Commonwealth and Development Office",
                 fund = "FCDO Research - Partnerships") %>%
          select(iati_identifier, gov_funder, fund) %>%
          unique()

        # 2.B) Identify UK gov funded activities from participating organisations

        partner_activities_via_funder <- org_activity_list %>%
          # unnest title
          unnest(cols = title.narrative) %>%
          rename(title = text) %>% 
          # unnest participating orgs
          filter(lengths(participating_org) != 0) %>%
          unnest(cols = participating_org) %>%
          # unnest org names
          select(iati_identifier, role.name, narrative, ref, activity_id) %>%
          unnest(cols = narrative) %>%
          select(-lang.code, -lang.name) %>%
          rename(org_name = text) %>% 
            # restrict to funding organisations and AgResults activities (pooled funding)
          filter(role.name %in% c("Funding") |
                   str_detect(iati_identifier, "XI-IATI-AGR")
                   ) %>%
          unique() %>%
            # restrict to UK gov funding
          filter(ref == "GB-GOV-1" |
                   str_detect(org_name, "Britain|DFID|FCDO|DHSC|Department of Health and Social Care") |
                   str_detect(iati_identifier, "DFID") |
                   str_detect(iati_identifier, "XI-IATI-AGR")      # AgResults partially funded
                 ) %>%
            # define fund and funder
          mutate(gov_funder = if_else(str_detect(org_name, "Health"), "Department of Health and Social Care",
                                      "Foreign, Commonwealth and Development Office"),
                 fund = case_when(
                           # IDRC GAMRIF projects
                           str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(org_name, "Health") ~ "DHSC - Global Health Security - GAMRIF",
                           # Other DHSC partnerships
                           str_detect(org_name, "Health") ~ "DHSC - Global Health Research - Partnerships",
                           # FCDO funding
                           TRUE ~ "FCDO Research - Partnerships"
                           )) %>%
          select(iati_identifier, gov_funder, fund) %>%
          unique()

        # Combine 2A and 2B
        partnership_activities <- plyr::rbind.fill(partner_activities_via_title, partner_activities_via_funder)
        
        # Filter original data
        partnership_activities <- org_activity_list %>%
          inner_join(partnership_activities, by = "iati_identifier")

# Save to Rdata file
saveRDS(partnership_activities, file = "Outputs/partnership_activities.rds")
# partnership_activities <- readRDS(file = "Outputs/partnership_activities.rds")


### C) Combine individual with partner activities (extractions A and B above) ---- 

partner_activity_comb <- plyr::rbind.fill(partner_activity_extract, partnership_activities) %>% 
  filter(default_flow_type == "ODA" | is.na(default_flow_type))


# D) Extract nested activity data ----------------------------------------------

# Extract base activity information - hierarchy and status
activity_list_base <- partner_activity_comb %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status.name,
         activity_id,
         gov_funder,
         fund) %>% 
  unique()


# 1) Unlist activity title and description
activity_list_unnest_1 <- partner_activity_comb %>% 
    # title
  filter(lengths(title.narrative) != 0) %>%
  unnest(cols = title.narrative) %>% 
  filter(lang.name == "English") %>% 
  select(-lang.code, -lang.name) %>% 
  rename(activity_title = text) %>% 
    # description
  filter(lengths(description) != 0) %>% 
  unnest(cols = description) %>% 
  mutate(type.name = coalesce(type.name, "General")) %>% 
  select(iati_identifier, activity_title, type.name, narrative) %>% 
  filter(lengths(narrative) != 0) %>% 
  unnest(cols = narrative) %>%  
  filter(lang.name == "English") %>% 
  unique()


    # Summarise records with multiple "General" descriptions
    activity_list_unnest_1 <- activity_list_unnest_1 %>% 
                                  group_by(iati_identifier, activity_title, type.name) %>% 
                                  summarise(text = paste(coalesce(text, ""), collapse = "\n\n")) %>% 
                                  spread(key = type.name, value = text) %>% 
                                  mutate(activity_description = if_else(!is.na(Objectives), paste0(General, "\n\n", Objectives), General)) %>% 
                                  ungroup()


# 2) Unlist recipient countries
activity_list_unnest_2 <- partner_activity_comb %>% 
  filter(lengths(recipient_country) != 0) %>% 
  unnest(cols = recipient_country) %>% 
  select(iati_identifier, country.name) %>% 
  group_by(iati_identifier) %>%
  unique() %>% 
  summarise(country_name = paste(coalesce(country.name, ""), collapse = ", "))

  
# 3) Unlist sectors
activity_list_unnest_3 <- partner_activity_comb %>% 
  filter(lengths(sector) != 0) %>% 
  unnest(cols = sector) %>% 
  select(iati_identifier, sector.name) %>% 
  filter(sector.name != "Vocabulary 99 or 98") %>% 
  group_by(iati_identifier) %>%
  unique() %>% 
  summarise(sector_name = paste(coalesce(sector.name, ""), collapse = ", ")) %>% 
  ungroup()


# 4) Unlist implementing organisations
activity_list_unnest_4 <- partner_activity_comb %>%
  select(-activity_id) %>%  # newly added?
  filter(lengths(participating_org) != 0) %>% 
  unnest(cols = participating_org) %>% 
  select(iati_identifier, role.name, narrative, ref) %>% 
  filter(lengths(narrative) != 0,
         role.name == "Implementing") %>% 
  unnest(cols = narrative) %>% 
  group_by(iati_identifier, role.name, ref) %>%
  slice(1) %>% 
  ungroup() %>% 
  select(-lang.code, -lang.name) %>% 
  unique()

  # Identify activities with no implementing partner info
    no_partner_info <- activity_list_unnest_4 %>% 
      select(iati_identifier) %>% 
      mutate(has_implementing_partner_info = "Yes") %>% 
      unique() %>% 
      right_join(partner_activity_comb, by = "iati_identifier") %>% 
      filter(is.na(has_implementing_partner_info)) %>% 
      select(iati_identifier) %>% 
      unique()

  # Add country locations based on IATI references or lookup
  # (takes ~10 mins to run)
    activity_list_unnest_4 <- activity_list_unnest_4 %>%
        # Extract 2 digit country code from org references (where populated)
      mutate(country_code = if_else((!is.na(ref) & substr(ref,3,3) == "-" & !(substr(ref,1,2) %in% c("XI", "XM"))), substr(ref,1,2), "")) %>% 
        # Look up country from both country code and organisation name
      mutate(org_country_iati = map(country_code, country_code_to_name),
             org_country_other = map(text, org_country_lookup)) %>% 
      mutate(org_country_iati = unlist(org_country_iati),
             org_country_other = unlist(org_country_other)) %>% 
        # Take best of both country lookup results
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
  activity_list_unnest_4 <- activity_list_unnest_4 %>% 
    select(iati_identifier) %>% 
    unique() %>% 
    left_join(activity_list_unnest_4_partner_names, by = "iati_identifier") %>% 
    left_join(activity_list_unnest_4_partner_countries, by = "iati_identifier")


# 5) Unlist publishing org
activity_list_unnest_5 <- partner_activity_comb %>%
  filter(lengths(reporting_org.narrative) != 0) %>% 
  unnest(cols = reporting_org.narrative) %>% 
  select(iati_identifier, 
         reporting_org_ref = reporting_org.ref, 
         reporting_org = text) %>% 
  # take top (English) name in cases of different languages
  group_by(iati_identifier, reporting_org_ref) %>%
  slice(1) %>% 
  unique() %>% 
  ungroup()

    # Lookup country (takes ~10 mins to run)
    activity_list_unnest_5 <- activity_list_unnest_5 %>% 
        # Extract 2 digit country code from org references (where populated)
      mutate(country_code = if_else((!is.na(reporting_org_ref) & substr(reporting_org_ref,3,3) == "-" & !(substr(reporting_org_ref,1,2) %in% c("XI", "XM"))), 
                                    substr(reporting_org_ref,1,2), "")) %>% 
        # Look up country from both country code and organisation name
      mutate(org_country_iati = map(country_code, country_code_to_name),
             org_country_other = map(reporting_org, org_country_lookup)) %>% 
      mutate(org_country_iati = unlist(org_country_iati),
             org_country_other = unlist(org_country_other)) %>% 
        # Take best of both country lookup results
      mutate(reporting_org_country = coalesce(org_country_iati, org_country_other)) %>% 
      select(-org_country_iati, -org_country_other)

    # Add on to org file to save
    org_names_and_locations_1 <- org_names_and_locations_1 %>% 
      rbind(activity_list_unnest_5 %>% 
                select(project_id = iati_identifier,
                       organisation_name = reporting_org,
                       organisation_country = reporting_org_country) %>% 
                mutate(organisation_role = 1) %>% # leading
                unique())
    
    
# 6) Unlist and aggregate budget
activity_list_unnest_6 <- partner_activity_comb %>% 
  select(-activity_id) %>% 
  filter(lengths(budget) != 0) %>% 
  unnest(cols = budget) %>% 
  select(iati_identifier, 
         budget_status = status.name, 
         amount = value.value, 
         currency = value.currency.code,
         period_start,
         period_end) 
 
      # Find activities with multiple budgets for same period (i.e. indicative and committed)
        multiple_budgets <- activity_list_unnest_6 %>% 
        select(iati_identifier, budget_status, period_start, period_end) %>%
        unique() %>% 
        group_by(iati_identifier, period_start, period_end) %>% 
        summarise(count = n()) %>% 
        filter (count > 1)
      
      # Keep only the committed budget in these cases
        activity_list_unnest_6 <- activity_list_unnest_6 %>% 
        filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
                 budget_status == "Committed")
      
      # Sum to get total budget per activity
        activity_list_unnest_6 <- activity_list_unnest_6 %>% 
        group_by(iati_identifier, currency) %>% 
        summarise(period_start = min(period_start),
                  period_end = max(period_end),
                  amount = sum(amount))

        
# 7) Unlist start/end dates
activity_list_unnest_7 <- partner_activity_comb %>% 
  unnest(cols = activity_date) %>% 
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


# 8) Extract transactions

# Prepare results data frame and counters
transaction_list <- data.frame()

# Run extraction, stopping when no new transactions are returned
# (takes ~5 mins)
for (id in partner_activity_comb$iati_identifier) {
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

# Save to Rdata file
saveRDS(transaction_list, file = "Outputs/transaction_list.rds")
# transaction_list <- readRDS(file = "Outputs/transaction_list.rds")


    # Extract recipient countries (where included in transactions)
        transaction_countries <- transaction_list %>% 
          select(iati_identifier, recipient_countries) %>% 
          unique() %>% 
            # unnest countries
          filter(lengths(recipient_countries) != 0) %>% 
          unnest(cols = recipient_countries) %>%
          select(-country.url, -country.code) %>% 
            # rename and remove blanks
          rename(recipient_country = country.name) %>% 
          filter(!is.na(recipient_country)) %>% 
          unique()
        
        # Summarise countries for joining to main dataset
        transaction_countries_summarised <- transaction_countries %>% 
          group_by(iati_identifier) %>% 
          summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))
        

    # Extract receiver organisations
        transaction_receiver_orgs <- transaction_list %>% 
          select(iati_identifier, receiver_organisation.narrative) %>% 
          unique() %>% 
            # unnest organisation names
          filter(lengths(receiver_organisation.narrative) != 0) %>% 
          unnest(cols = receiver_organisation.narrative) %>% 
          select(-lang.code, -lang.name) %>% 
          rename(transaction_receiver_name = text) %>% 
            # Exclude blanks, other text
          filter(!is.na(transaction_receiver_name), 
                 !str_detect(str_to_lower(transaction_receiver_name), "reimbursement"),
                 !str_detect(str_to_lower(transaction_receiver_name), "disbursement")) %>% 
          unique()
        
        # Add to organisation name and country database
        receiver_orgs_to_save <- transaction_receiver_orgs %>% 
          inner_join(no_partner_info, by = "iati_identifier") %>% 
          rename(project_id = iati_identifier,
                 organisation_name = transaction_receiver_name) %>% 
          # Look up country from both country code and organisation name
          mutate(organisation_country = map(organisation_name, org_country_lookup)) %>% 
          mutate(organisation_country = unlist(organisation_country)) %>% 
          mutate(organisation_role = 2) # partners
          
          # Add on to org file to save
          org_names_and_locations_1 <- org_names_and_locations_1 %>% 
          rbind(receiver_orgs_to_save)      
        
        
        # Summarise orgs for joining to main dataset
        transaction_orgs_summarised <- transaction_receiver_orgs %>% 
          group_by(iati_identifier) %>% 
          summarise(transaction_receiver_name = paste(coalesce(transaction_receiver_name, ""), collapse = ", "))
        
        
      # Join on transactions country and org info to relevant datasets
        activity_list_unnest_2 <- partner_activity_comb %>% 
          select(iati_identifier) %>% 
          left_join(activity_list_unnest_2, by = "iati_identifier") %>% 
          left_join(transaction_countries_summarised, by = "iati_identifier") %>% 
          mutate(recipient_country = coalesce(recipient_country, country_name)) %>% 
          select(-country_name)
        
        activity_list_unnest_4 <- partner_activity_comb %>% 
          select(iati_identifier) %>% 
          left_join(activity_list_unnest_4, by = "iati_identifier") %>%
          left_join(transaction_orgs_summarised, by = "iati_identifier") %>% 
          mutate(partner = coalesce(partner, transaction_receiver_name)) %>% 
          select(-transaction_receiver_name)
          

# Join unnested info to original data
activity_list <- activity_list_base %>% 
  left_join(activity_list_unnest_1, by = "iati_identifier") %>%
  left_join(activity_list_unnest_2, by = "iati_identifier") %>%
  left_join(activity_list_unnest_3, by = "iati_identifier") %>%
  left_join(activity_list_unnest_4, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_5, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_6, by = "iati_identifier") %>% 
  left_join(activity_list_unnest_7, by = "iati_identifier")


# Assign a reporting org name if missing 
activity_list <- activity_list %>% 
  mutate(reporting_org = coalesce(reporting_org, reporting_org_ref, gov_funder))

# Reorder columns and add date of refresh
activity_list <- activity_list %>% 
  select(iati_identifier, reporting_org_ref, reporting_org, reporting_org_country,
         hierarchy, activity_status, activity_id,
         activity_title, activity_description, start_date, end_date,
         recipient_country, sector_name,
         partner, partner_country, 
         gov_funder, fund,
         amount, period_start, period_end, currency) %>% 
  unique() %>% 
  mutate(refresh_date = Sys.Date())


activity_list <- activity_list %>%  
      # Add missing FCDO activity IDs (IDRC and AgResults)
    rename(programme_id = activity_id) %>% 
    mutate(programme_id = case_when(str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(activity_description, "CLARE") ~ "GB-GOV-1-300126",
                                    str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(activity_description, "CARIAA") ~ "GB-1-203506", 
                                    str_detect(iati_identifier, "XI-IATI-AGR") ~ "GB-1-203052",
                                    TRUE ~ programme_id),  
      # Add Fund and Funder label  
           fund = case_when(
                    str_detect(programme_id, "GB-GOV-1-|GB-1-") ~ "FCDO Research - Programmes",
                    str_detect(programme_id, "GB-GOV-10") ~ "DHSC - Global Health Research - Partnerships",
                    TRUE ~ fund),
           gov_funder = case_when(
             str_detect(programme_id, "GB-GOV-1-|GB-1-") ~ "Foreign, Commonwealth and Development Office",
             str_detect(programme_id, "GB-GOV-10") ~ "Department of Health and Social Care",
             TRUE ~ gov_funder))


# Save to Rdata file
saveRDS(activity_list, file = "Outputs/partner_activity_list.rds")
# activity_list <- readRDS(file = "Outputs/partner_activity_list.rds")

# Save org names and countries to file
saveRDS(org_names_and_locations_1, file = "Outputs/org_names_and_locations_1.rds")

# Clear environment
rm(partner_activity_extract, partnership_activities, partner_activities_via_title, partner_activities_via_funder,
   result, new_rows, x, y, page, ri_linked_activites,
   activity_list_base, activity_list_unnest_1, activity_list_unnest_2, activity_list_unnest_3, activity_list_unnest_4,
   activity_list_unnest_4_partner_countries, activity_list_unnest_4_partner_names, activity_list_unnest_5,
   activity_list_unnest_6, activity_list_unnest_7, unlinked_partner_iati_activity_ids)
