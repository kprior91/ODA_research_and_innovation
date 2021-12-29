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


### 2) Activity extract for specific partnership organisations ----

# FCDO (part-)funded partnership activities 

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
          select(iati_identifier, gov_funder, fund, text) %>%
          unique()

        # 2.B) Identify UK gov funded activities from participating organisations

        partner_activities_via_funder <- org_activity_list %>%
          unnest(cols = title.narrative) %>%
          filter(lengths(participating_org) != 0) %>%
          unnest(cols = participating_org) %>%
          select(iati_identifier, role.name, narrative, ref, activity_id) %>%
          unnest(cols = narrative) %>%
          select(-lang.code, -lang.name) %>%
            # restrict to funding organisations and AgResults activities (pooled funding)
          filter(role.name %in% c("Funding") |
                   str_detect(iati_identifier, "XI-IATI-AGR")
                   ) %>%
          unique() %>%
            # restrict to UK gov funding
          filter(ref == "GB-GOV-1" |
                   str_detect(text, "Britain|DFID|FCDO|DHSC|Department of Health and Social Care") |
                   str_detect(iati_identifier, "DFID") |
                   str_detect(iati_identifier, "XI-IATI-AGR")      # AgResults partially funded
                 ) %>%
            # define fund and funder
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
          select(iati_identifier, activity_id, gov_funder, fund, text) %>%
          unique()

        # Combine 2A and 2B
        partnership_activities <- plyr::rbind.fill(partner_activities_via_title, partner_activities_via_funder)

        # Remove World Health Organisation non-research/innovation activities
        partnership_activities <- partnership_activities %>% 
          filter(!(str_detect(iati_identifier, "XM-DAC-928")) |
                   str_detect(text, "research|innovation")) %>% 
          select(-text)
        
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
    # Join on fund/funder information
  left_join(select(partner_iati_activity_ids, iati_id, gov_funder), by = c("iati_identifier" = "iati_id")) %>% 
  mutate(gov_funder = coalesce(gov_funder.x, gov_funder.y)) %>% 
  select(-gov_funder.x, -gov_funder.y) %>% 
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

  # Identify activities without recipient countries at activity level
  no_country_info <- partner_activity_comb %>% 
    filter(lengths(recipient_country) == 0)
  
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
      
      # Subset fields
      transaction_list_filtered <- transaction_list %>% 
        unnest(cols = description.narrative,
               keep_empty = TRUE) %>% 
        select(iati_identifier, transaction_date, transaction_description = text,
               currency.code, value, receiver_organisation.ref, receiver_organisation.narrative,
               recipient_countries)
      
      # Unnest country information -----
      transactions_by_country <- transaction_list_filtered %>% 
          # unnest countries
        filter(lengths(recipient_countries) != 0) %>% 
        unnest(cols = recipient_countries) %>%
        select(-country.url, -country.code) %>% 
        rename(recipient_country = country.name) %>% 
        filter(!is.na(recipient_country)) %>% 
        unique()
      
      # Unnest receiver organisation info (where populated)
      transactions_by_country_and_org <- transactions_by_country %>% 
          # unnest orgs
        filter(lengths(receiver_organisation.narrative) != 0) %>% 
        unnest(cols = receiver_organisation.narrative) %>% 
        select(-lang.code, -lang.name) %>% 
        unique()

        saveRDS(transactions_by_country_and_org, "Outputs/transactions_by_country_and_org.R")
  
      # Summarise countries for joining to main dataset
      transactions_by_country <- transactions_by_country %>% 
        group_by(iati_identifier) %>% 
        summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))
      
# Join on transactions country info to rest of dataset
activity_list_unnest_2 <- partner_activity_comb %>% 
    select(iati_identifier) %>% 
    left_join(activity_list_unnest_2, by = "iati_identifier") %>% 
    left_join(transactions_by_country, by = "iati_identifier") %>% 
    mutate(recipient_country = coalesce(recipient_country, country_name)) %>% 
    select(-country_name)
  

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
  filter(lang.name == "English") %>% 
  select(-lang.code, -lang.name) %>% 
  unique()

  # Add country locations based on IATI references or lookup
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

    # Lookup country
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

# Add Fund label
activity_list <- activity_list %>%  
    left_join(select(partner_iati_activity_ids, gov_funder, iati_id, funding_iati_id), 
              by = c("iati_identifier" = "iati_id", "gov_funder")) %>% 
    mutate(programme_id = coalesce(funding_iati_id, activity_id)) %>% 
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

# Clear environment
rm(partner_activity_extract, partnership_activities, partner_activities_via_title, partner_activities_via_funder,
   result, new_rows, x, y, page, ri_linked_activites,
   activity_list_base, activity_list_unnest_1, activity_list_unnest_2, activity_list_unnest_3, activity_list_unnest_4,
   activity_list_unnest_4_partner_countries, activity_list_unnest_4_partner_names, activity_list_unnest_5,
   activity_list_unnest_6, activity_list_unnest_7)
