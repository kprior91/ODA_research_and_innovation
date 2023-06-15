# --------------------------------------------------------------- #
# Script 3
# Extract ODA research activities from public IATI data #
# --------------------------------------------------------------- #

### 1) Extract IATI data on RI partner activities (linked or manually identified) ----

# Read in linked partner IATI activity info from script 1
ri_linked_activites <- readRDS(file = "Outputs/ri_linked_activites_kp.rds")

# Manually add on other (non-linked) partner activities from Excel
partner_iati_activity_ids <- unlinked_partner_iati_activity_ids %>% 
  plyr::rbind.fill(ri_linked_activites)


specific_activity_extract <- function(o_code) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- iati_activity_extract(page, o_code)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

specific_activity_extract("GB-CHC-1177110-HIF")

partner_activity_extract <- lapply(partner_iati_activity_ids$iati_identifier, specific_activity_extract)
partner_activity_extract = rbindlist(partner_activity_extract, fill=T)

length(unique(partner_activity_extract$reporting_org_ref))
unique(partner_activity_extract$activity_id)
unique(partner_activity_extract$gov_funder)
nrow(partner_activity_extract[partner_activity_extract$gov_funder=="Foreign, Commonwealth and Development Office",])
partner_activity_extract[partner_activity_extract$reporting_org_ref=="GB-COH-877338",]

#       # Prepare results data frame and counters
#       partner_activity_extract <- data.frame()
# 
#       # Run extraction, stopping when no new sector codes returned
#       for (id in partner_iati_activity_ids$iati_id) {
# 
#           print(id)
#           result <- iati_activity_extract(id)
#           partner_activity_extract <- c(partner_activity_extract, result)
# 
#       }
# 
# partner_activity_extract <- bind_rows(partner_activity_extract)

# Save to Rdata file
# saveRDS(partner_activity_extract, file = "Outputs/partner_activity_extract_kp.rds")
partner_activity_extract <- readRDS(file = "Outputs/partner_activity_extract_kp.rds")

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

org_activity_list <- lapply(org_code, specific_org_extract)
org_activity_list = rbindlist(org_activity_list, fill=T)

# saveRDS(org_activity_list, "Outputs/org_activity_list_kp.rds")
org_activity_list <- readRDS("Outputs/org_activity_list_kp.rds")


        
        # 2.A) Unlist activity titles and subset for those that mention FCDO/DFID
        # (identifies FCDO-funded Gates Foundation activities)

        partner_activities_via_title <- org_activity_list %>%
          filter(reporting_org_ref == "DAC-1601") %>%  # Gates org ID
          filter(str_detect(title_narrative, "FCDO|DFID")) %>%
          mutate(gov_funder = "Foreign, Commonwealth and Development Office",
                 fund = "FCDO Research - Partnerships") %>%
          select(iati_identifier, gov_funder, fund) %>%
          unique()

        # 2.B) Identify UK gov funded activities from participating organisations

        partner_activities_via_funder2 <- org_activity_list %>%
            # restrict to UK gov funding
          select(iati_identifier, participating_org_role, participating_org_narrative, participating_org_ref, participating_org_activity_id) %>%
          filter(str_detect(participating_org_role, "1") |
            str_detect(participating_org_ref, "GB-GOV-1") |
                   str_detect(participating_org_narrative, "Britain|DFID|FCDO|DHSC|Department of Health and Social Care") |
                   str_detect(iati_identifier, "DFID") |
                   str_detect(iati_identifier, "XI-IATI-AGR")      # AgResults partially funded
                 )
        
        partner_activities_via_funder2 <- partner_activities_via_funder2 %>%
          mutate(gov_funder = if_else(str_detect(participating_org_narrative, "Health"), "Department of Health and Social Care",
                                      "Foreign, Commonwealth and Development Office"),
                 fund = case_when(
                   # IDRC GAMRIF projects
                   str_detect(iati_identifier, "XM-DAC-301-2") & str_detect(participating_org_narrative, "Health") ~ "DHSC - Global Health Security - GAMRIF",
                   # Other DHSC partnerships
                   str_detect(participating_org_narrative, "Health") ~ "DHSC - Global Health Research - Partnerships",
                   # FCDO funding
                   TRUE ~ "FCDO Research - Partnerships"
                 )) %>%
          select(iati_identifier, gov_funder, fund) %>%
          unique()
# 
#         partner_activities_via_funder2_unnest <- partner_activities_via_funder2 %>% unnest(cols = c(participating_org_ref))
#         unique(partner_activities_via_funder2_unnest$participating_org_ref)
        
        # participating_org_id <- org_activity_list %>%
        #   select(iati_identifier, participating_org_role, participating_org_activity_id) %>%
        #   unnest(cols = c(participating_org_role, participating_org_activity_id)) %>%
        #   filter(participating_org_role == "1" & !is.na(participating_org_activity_id)) %>%
        #   unique()
        # 
        # participating_org_id <- participating_org_id %>%
        #   select(iati_identifier,participating_org_activity_id) %>%
        #   filter(participating_org_activity_id != "")
         
        # Combine 2A and 2B
        partnership_activities <- plyr::rbind.fill(partner_activities_via_title, partner_activities_via_funder2)
        
        # Filter original data
        partnership_activities <- org_activity_list %>%
          inner_join(partnership_activities, by = "iati_identifier")

# Save to Rdata file
# saveRDS(partnership_activities, file = "Outputs/partnership_activities_kp.rds")
partnership_activities <- readRDS(file = "Outputs/partnership_activities_kp.rds")


### C) Combine individual with partner activities (extractions A and B above) ---- 

partner_activity_comb <- plyr::rbind.fill(partner_activity_extract, partnership_activities) %>% 
  filter(default_flow_type_code == "10" | is.na(default_flow_type_code))


# D) Extract nested activity data ----------------------------------------------

# Extract base activity information - hierarchy and status
activity_list_base <- partner_activity_comb %>% 
  select(iati_identifier, hierarchy, 
         activity_status = activity_status_code,
         gov_funder,
         activity_id,
         fund) %>% 
  unique() %>%
  mutate(activity_status = str_replace(activity_status, "1", "Pipeline/identification"),
         activity_status = str_replace(activity_status, "2", "Implementation"),
         activity_status = str_replace(activity_status, "3", "Finalisation"),
         activity_status = str_replace(activity_status, "4", "Closed"),
         activity_status = str_replace(activity_status, "5", "Cancelled"),
         activity_status = str_replace(activity_status, "6", "Suspended"))


# 1) Unlist activity title and description
activity_list_unnest_1_title <- partner_activity_comb %>%
  select(iati_identifier, title_narrative, title_narrative_xml_lang) %>%
  unnest(cols = c(title_narrative,title_narrative_xml_lang)) %>%
  mutate(title_narrative_xml_lang = ifelse(is.na(title_narrative_xml_lang),"",title_narrative_xml_lang)) %>%
filter(title_narrative_xml_lang != "fr") %>%
  select(iati_identifier, title_narrative) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(title_narrative = paste(coalesce(title_narrative, ""), collapse = "; ")) %>%
  rename(activity_title = title_narrative)

activity_list_unnest_1_description <- partner_activity_comb %>%
  select(iati_identifier, description_narrative, description_narrative_xml_lang) %>%
  unnest(cols = c(description_narrative,description_narrative_xml_lang)) %>%
  mutate(description_narrative_xml_lang = ifelse(is.na(description_narrative_xml_lang),"",description_narrative_xml_lang)) %>%
  filter(description_narrative_xml_lang != "fr") %>%
  select(iati_identifier, description_narrative) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(description_narrative = paste(coalesce(description_narrative, ""), collapse = "; ")) %>%
  rename(activity_description = description_narrative)


activity_list_unnest_1 <- full_join(activity_list_unnest_1_title, activity_list_unnest_1_description, by = "iati_identifier")

# Summarise records with multiple "General" descriptions
# activity_list_unnest_1 <- activity_list_unnest_1 %>%
#       group_by(iati_identifier, title_narrative, reporting_org_type) %>%
#       summarise(text = paste(coalesce(text, ""), collapse = "\n\n")) %>%
#       spread(key = reporting_org_type, value = text) %>%
#       mutate(activity_description = if_else(!is.na(Objectives), paste0(General, "\n\n", Objectives), General)) %>%
#       ungroup()


# 2) Unlist recipient countries

# activity_list_unnest_2 <- partner_activity_comb %>% 
#   filter(lengths(recipient_country) != 0) %>% 
#   unnest(cols = recipient_country) %>% 
#   select(iati_identifier, country.name) %>% 
#   group_by(iati_identifier) %>%
#   unique() %>% 
#   summarise(country_name = paste(coalesce(country.name, ""), collapse = ", "))

activity_list_unnest_2_country <- partner_activity_comb %>%
  #filter(lengths(recipient_country_code) != 0) %>%
  unnest(c(recipient_country_code)) %>% 
  select(iati_identifier, recipient_country_code)

activity_list_unnest_2_country$recipient_country <- countrycode_list$name[match(activity_list_unnest_2_country$recipient_country_code,countrycode_list$code)]

activity_list_unnest_2_country <- activity_list_unnest_2_country %>% select(-recipient_country_code) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))


activity_list_unnest_2_region <- partner_activity_comb %>%
  #filter(lengths(recipient_country_code) != 0) %>%
  unnest(c(recipient_region_code)) %>% 
  select(iati_identifier, recipient_region_code)

activity_list_unnest_2_region$recipient_region <- regioncode_list$name[match(activity_list_unnest_2_region$recipient_region_code,regioncode_list$code)]

activity_list_unnest_2_region <- activity_list_unnest_2_region %>% select(-recipient_region_code) %>%
  unique() %>% 
  group_by(iati_identifier) %>%
  summarise(recipient_region = paste(coalesce(recipient_region, ""), collapse = ", "))

activity_list_unnest_2 <- full_join(activity_list_unnest_2_country, activity_list_unnest_2_region, by = "iati_identifier")

activity_list_unnest_2$recipient_country_region <- coalesce(activity_list_unnest_2$recipient_country, activity_list_unnest_2$recipient_region)

activity_list_unnest_2 <- activity_list_unnest_2 %>% 
  mutate(across(c(recipient_country_region), na_if, "Developing countries, unspecified")) %>%
  select(-recipient_country, -recipient_region)

  
# 3) Unlist sectors
# activity_list_unnest_3 <- partner_activity_comb %>% 
#   filter(lengths(sector) != 0) %>% 
#   unnest(cols = sector) %>% 
#   select(iati_identifier, sector.name) %>% 
#   filter(sector.name != "Vocabulary 99 or 98") %>% 
#   group_by(iati_identifier) %>%
#   unique() %>% 
#   summarise(sector_name = paste(coalesce(sector.name, ""), collapse = ", ")) %>% 
#   ungroup()

activity_list_unnest_3 <- partner_activity_comb %>%
  #filter(lengths(sector_code) != 0) %>%
  unnest(cols = sector_code) %>% 
  select(iati_identifier, sector_code) %>% 
  unique()

activity_list_unnest_3$sector_name <- sector_list$name[match(activity_list_unnest_3$sector_code,sector_list$code)]

activity_list_unnest_3 <- activity_list_unnest_3 %>% select(-sector_code) %>%
  unique() %>%
  group_by(iati_identifier) %>%
  summarise(sector_name = paste(coalesce(sector_name, ""), collapse = ", "))


activity_list_unnest_3[activity_list_unnest_3$iati_identifier=="GB-COH-03122495-A0014",]

# 4) Unlist implementing organisations

# Warning - some publishers (e.g. IDRC) have implementing org names in multiple 
  # languages. These are nnot filtered out currently

# activity_list_unnest_4 <- partner_activity_comb %>%
#   select(-activity_id) %>%  # newly added?
#   filter(lengths(participating_org) != 0) %>% 
#   unnest(cols = participating_org) %>% 
#   select(iati_identifier, role.name, narrative, ref) %>% 
#   filter(lengths(narrative) != 0,
#          role.name == "Implementing") %>% 
#   unnest(cols = narrative) %>% 
#   unique()
# 
#   # Identify activities with no implementing partner info
#     no_partner_info <- activity_list_unnest_4 %>% 
#       select(iati_identifier) %>% 
#       mutate(has_implementing_partner_info = "Yes") %>% 
#       unique() %>% 
#       right_join(partner_activity_comb, by = "iati_identifier") %>% 
#       filter(is.na(has_implementing_partner_info)) %>% 
#       select(iati_identifier) %>% 
#       unique()
# 
#   # Add country locations based on IATI references or lookup
#   # (takes ~10 mins to run)
#     activity_list_unnest_4 <- activity_list_unnest_4 %>%
#         # Extract 2 digit country code from org references (where populated)
#       mutate(country_code = if_else((!is.na(ref) & substr(ref,3,3) == "-" & !(substr(ref,1,2) %in% c("XI", "XM"))), substr(ref,1,2), "")) %>% 
#         # Look up country from both country code and organisation name
#       mutate(org_country_iati = map(country_code, country_code_to_name),
#              org_country_other = map(text, org_country_lookup)) %>% 
#       mutate(org_country_iati = unlist(org_country_iati),
#              org_country_other = unlist(org_country_other)) %>% 
#         # Take best of both country lookup results
#       mutate(org_country = coalesce(org_country_iati, org_country_other)) %>% 
#       select(-org_country_iati, -org_country_other)
#       
#   # Save implementing orgs with country to file
#     org_names_and_locations_1 <- activity_list_unnest_4 %>% 
#       select(project_id = iati_identifier,
#              organisation_name = text,
#              organisation_country = org_country) %>% 
#       mutate(organisation_role = 2) 
#     
#   # Collapse implementing orgs
#     activity_list_unnest_4_partner_names <- activity_list_unnest_4 %>% 
#       select(iati_identifier, text) %>% 
#       filter(!is.na(text)) %>% 
#       unique() %>% 
#       group_by(iati_identifier) %>% 
#       summarise(partner = paste(coalesce(text, ""), collapse = ", "))
#     
#     activity_list_unnest_4_partner_countries <- activity_list_unnest_4 %>% 
#       select(iati_identifier, org_country) %>% 
#       filter(!is.na(org_country)) %>% 
#       unique() %>% 
#       group_by(iati_identifier) %>% 
#       summarise(partner_country = paste(coalesce(org_country, ""), collapse = ", "))
# 
#   # Combine in single dataset
#   activity_list_unnest_4 <- activity_list_unnest_4 %>% 
#     select(iati_identifier) %>% 
#     unique() %>% 
#     left_join(activity_list_unnest_4_partner_names, by = "iati_identifier") %>% 
#     left_join(activity_list_unnest_4_partner_countries, by = "iati_identifier")

  
activity_list_unnest_4 <- partner_activity_comb %>% 
    #filter(lengths(participating_org_ref) != 0) %>%
    #unnest(c(participating_org_ref, participating_org_narrative)) %>% 
    select(iati_identifier, participating_org_role, participating_org_narrative, participating_org_ref) %>% 
    unnest(c(participating_org_role, participating_org_narrative, participating_org_ref)) %>% 
    filter(participating_org_role == "4") %>% 
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
  
  # Add country locations based on IATI org references or lookup
  # (takes ~5 mins to run)
activity_list_unnest_4 <- activity_list_unnest_4 %>%
    # Extract 2 digit country code from org references (where populated)
    mutate(country_code = if_else((!is.na(participating_org_ref) & substr(participating_org_ref,3,3) == "-" & !(substr(participating_org_ref,1,2) %in% c("XI", "XM"))), 
                                  substr(participating_org_ref,1,2), ""))
  
  # Function searches the org title (e.g. Kenya) and returns a country if its present in the name
activity_list_unnest_4$country_name2 <- map(activity_list_unnest_4$participating_org_narrative, org_country_lookup)
activity_list_unnest_4$country_name2 <- as.character(activity_list_unnest_4$country_name2)
  activity_list_unnest_4$country_name2 <- ifelse(activity_list_unnest_4$country_name2 == "NA", "", activity_list_unnest_4$country_name2)
  
  
  # check if input is a valid 2-digit country code and then does a lookup to the countrycode_list
  activity_list_unnest_4$country_name <- ifelse(is.na(activity_list_unnest_4$country_code) | nchar(activity_list_unnest_4$country_code) < 2, NA, countrycode_list$name[match(activity_list_unnest_4$country_code,countrycode_list$code)])
  activity_list_unnest_4$country_name <- ifelse(is.na(activity_list_unnest_4$country_name), "", activity_list_unnest_4$country_name)
  
  activity_list_unnest_4[activity_list_unnest_4 == ""] <- NA
  
  # This needs fixing because currently I don't know why it's only picking one of the country names when there might be several 
  activity_list_unnest_4$partner_country <- coalesce(activity_list_unnest_4$country_name, activity_list_unnest_4$country_name2)
  
  activity_list_unnest_4 <- activity_list_unnest_4%>% select(-country_name, -country_name2)
  
  # Summarise partner org countries and names
  activity_list_unnest_4_countries <- activity_list_unnest_4 %>% 
    select(iati_identifier, partner_country) %>% 
    unique() %>% 
    filter(!is.na(partner_country)) %>% 
    group_by(iati_identifier) %>% 
    summarise(partner_country = paste(partner_country, collapse = ", "))
  
  activity_list_unnest_4_partners <- activity_list_unnest_4 %>% 
    select(iati_identifier, participating_org_narrative) %>% 
    unique() %>% 
    filter(!is.na(participating_org_narrative)) %>% 
    group_by(iati_identifier) %>% 
    summarise(partner = paste(coalesce(participating_org_narrative, ""), collapse = ", ")) 
  
  # Add partner name and country info to master dataset
  activity_list_unnest_4 <- activity_list_unnest_4 %>% 
    select(-participating_org_narrative, -participating_org_ref, -country_code, -participating_org_role, -partner_country) %>% 
    left_join(activity_list_unnest_4_partners, by = "iati_identifier") %>% 
    left_join(activity_list_unnest_4_countries, by = "iati_identifier") %>%
    unique()

# 5) Unlist publishing org
  
  # activity_list_unnest_5 <- uk_gov_list_filtered %>% 
  #   #filter(lengths(participating_org_ref) != 0) %>%
  #   #unnest(c(participating_org_ref, participating_org_narrative)) %>% 
  #   select(iati_identifier, participating_org_role, participating_org_narrative) %>% 
  #   unnest(c(participating_org_role, participating_org_narrative)) %>% 
  #   filter(participating_org_role == "3") %>% 
  #   unique() %>%  select(-participating_org_role) %>% rename(extending_org = participating_org_narrative)
  
  activity_list_unnest_5 <- partner_activity_comb %>%
  filter(lengths(reporting_org_narrative) != 0) %>%
  unnest(cols = reporting_org_narrative) %>%
  select(iati_identifier,
         reporting_org_ref,reporting_org_narrative) %>%
  # take top (English) name in cases of different languages
  group_by(iati_identifier, reporting_org_ref) %>%
  slice(1) %>%
  unique() %>%
  ungroup()

    # Lookup country (takes ~10 mins to run)
    activity_list_unnest_5 <- activity_list_unnest_5 %>% 
        # Extract 2 digit country code from org references (where populated)
      mutate(country_code = if_else((!is.na(reporting_org_ref) & substr(reporting_org_ref,3,3) == "-" & !(substr(reporting_org_ref,1,2) %in% c("XI", "XM"))), 
                                    substr(reporting_org_ref,1,2), ""))
   
     # Function searches the org title (e.g. Kenya) and returns a country if its present in the name
    activity_list_unnest_5$country_name2 <- map(activity_list_unnest_5$reporting_org_narrative, org_country_lookup)
    activity_list_unnest_5$country_name2 <- as.character(activity_list_unnest_5$country_name2)
    activity_list_unnest_5$country_name2 <- ifelse(activity_list_unnest_5$country_name2 == "NA", "", activity_list_unnest_5$country_name2)
    
    # check if input is a valid 2-digit country code and then does a lookup to the countrycode_list
    activity_list_unnest_5$country_name <- ifelse(is.na(activity_list_unnest_5$country_code) | nchar(activity_list_unnest_5$country_code) < 2, NA, countrycode_list$name[match(activity_list_unnest_5$country_code,countrycode_list$code)])
    activity_list_unnest_5$country_name <- ifelse(is.na(activity_list_unnest_5$country_name), "", activity_list_unnest_5$country_name)
    
    activity_list_unnest_5[activity_list_unnest_5 == ""] <- NA
    
    # This needs fixing because currently I don't know why it's only picking one of the country names when there might be several 
    activity_list_unnest_5$reporting_org_country <- coalesce(activity_list_unnest_5$country_name, activity_list_unnest_5$country_name2)
    
    activity_list_unnest_5 <- activity_list_unnest_5%>% select(-country_name, -country_name2)

    
    # Add on to org file to save
    org_names_and_locations_1 <- activity_list_unnest_5 %>% 
                select(project_id = iati_identifier,
                       organisation_name = reporting_org_narrative,
                       organisation_country = reporting_org_country) %>% 
                mutate(organisation_role = 1) %>% # leading
                unique()
  
# Save to Rdata file
# saveRDS(org_names_and_locations_1, file = "Outputs/org_names_and_locations_1_kp.rds")
org_names_and_locations_1 <- readRDS(file = "Outputs/org_names_and_locations_1_kp.rds")
    
# 6) Unlist and aggregate budget
# activity_list_unnest_6 <- partner_activity_comb %>% 
#   select(-activity_id) %>% 
#   filter(lengths(budget) != 0) %>% 
#   unnest(cols = budget) %>% 
#   select(iati_identifier, 
#          budget_status = status.name, 
#          amount = value.value, 
#          currency = value.currency.code,
#          period_start,
#          period_end) 
#  
#       # Find activities with multiple budgets for same period (i.e. indicative and committed)
#         multiple_budgets <- activity_list_unnest_6 %>% 
#         select(iati_identifier, budget_status, period_start, period_end) %>%
#         unique() %>% 
#         group_by(iati_identifier, period_start, period_end) %>% 
#         summarise(count = n()) %>% 
#         filter (count > 1)
#       
#       # Keep only the committed budget in these cases
#         activity_list_unnest_6 <- activity_list_unnest_6 %>% 
#         filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
#                  budget_status == "Committed")
#       
#       # Sum to get total budget per activity
#         activity_list_unnest_6 <- activity_list_unnest_6 %>% 
#         group_by(iati_identifier, currency) %>% 
#         summarise(period_start = min(period_start),
#                   period_end = max(period_end),
#                   amount = sum(amount))


activity_list_unnest_6 <- partner_activity_comb %>% 
          filter(lengths(budget_value) != 0) %>%
          unnest(c(budget_status,budget_value,budget_value_currency,budget_period_start_iso_date,budget_period_end_iso_date)) %>% 
          select(iati_identifier, 
                 budget_status, 
                 budget_value, 
                 budget_value_currency,
                 budget_period_start_iso_date,
                 budget_period_end_iso_date) %>%
  unique()
        
        # Find activities with multiple budgets for same period (i.e. indicative and committed)
        multiple_budgets <- activity_list_unnest_6 %>% 
          select(iati_identifier, budget_status, budget_period_start_iso_date, budget_period_end_iso_date) %>%
          unique() %>% 
          group_by(iati_identifier, budget_period_start_iso_date, budget_period_end_iso_date) %>% 
          summarise(count = n()) %>% 
          filter (count > 1)
        
        # Keep only the committed budget in these cases
        activity_list_unnest_6 <- activity_list_unnest_6 %>% 
          filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
                   budget_status == "2")
        
        # Sum to get total budget per activity
        activity_list_unnest_6 <- activity_list_unnest_6 %>% 
          group_by(iati_identifier, budget_value_currency) %>% 
          summarise(budget_period_start_iso_date = min(budget_period_start_iso_date),
                    budget_period_end_iso_date = max(budget_period_end_iso_date),
                    budget_value = sum(budget_value)) %>%
          rename(period_start = budget_period_start_iso_date, period_end = budget_period_end_iso_date, amount = budget_value, currency = budget_value_currency)
 

        
# 7) Unlist start/end dates
# activity_list_unnest_7 <- partner_activity_comb %>% 
#   unnest(cols = activity_date) %>% 
#   select(iati_identifier, 
#          date = iso_date,
#          date_type = type.name) %>%
#   # take the first date in cases of two of the same time
#   group_by(iati_identifier, date_type) %>%
#   slice(1) %>%
#   spread(key = date_type, value = date) %>% 
#   mutate(start_date = coalesce(`Actual start`, `Planned start`),
#          end_date = coalesce(`Actual end`, `Planned End`)) %>% 
#   select(iati_identifier, start_date, end_date)

activity_list_unnest_7 <- partner_activity_comb %>% 
  #filter(lengths(activity_date_iso_date) != 0) %>%
  unnest(cols = c(activity_date_iso_date,activity_date_type)) %>% 
  select(iati_identifier, 
         date = activity_date_iso_date,
         date_type = activity_date_type) %>%
  group_by(iati_identifier, date_type) %>% 
  mutate(date_type = str_replace(date_type, "1", "Planned start"),
         date_type = str_replace(date_type, "2", "Actual start"),
         date_type = str_replace(date_type, "3", "Planned end"),
         date_type = str_replace(date_type, "4", "Actual end")) %>%
  slice(1) %>%
  spread(key = date_type, value = date) %>% 
  mutate(start_date = coalesce(`Actual start`, `Planned start`),
         end_date = coalesce(`Actual end`, `Planned end`)) %>% 
  select(iati_identifier, start_date, end_date)



# 8) Extract transactions

##### I need to fix this bit, extracting transactions, just need to run the rest of it first####

specific_trans_extract_country <- function(id) {
  page_list <- list()
  page <- 1
  page_df = data.frame()
  
  while (!is.null(page_df)) {
    Sys.sleep(1)
    message(page)
    page_df <- transactions_extract_country(page, id)
    if(!is.null(page_df)){
      page_list[[page]] = page_df
    }
    page = page + 1
  }
  rbindlist(page_list, fill=T)
}

# specific_org_extract("GB-GOV-15")

transaction_list_countries <- lapply(partner_activity_comb$iati_identifier, specific_trans_extract_country)
transaction_list_countries = rbindlist(transaction_list_countries, fill=T)

# Save to Rdata file
#saveRDS(transaction_list_countries, file = "Outputs/transaction_list_countries_kp.rds")
transaction_list_countries <- readRDS(file = "Outputs/transaction_list_countries_kp.rds")

transaction_list_countries$recipient_country <- countrycode_list$name[match(transaction_list_countries$transaction_recipient_country_code,countrycode_list$code)]

    # Extract recipient countries (where included in transactions)
        transaction_countries <- transaction_list_countries %>% 
          select(iati_identifier, recipient_country) %>% 
            # rename and remove blanks
          filter(!is.na(recipient_country)) %>% 
          unique()
        
        # Summarise countries for joining to main dataset
        transaction_countries_summarised <- transaction_countries %>% 
          group_by(iati_identifier) %>% 
          summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))
        

specific_trans_extract_recipient <- function(id) {
          page_list <- list()
          page <- 1
          page_df = data.frame()
          
          while (!is.null(page_df)) {
            Sys.sleep(1)
            message(page)
            page_df <- transactions_extract_recipient(page, id)
            if(!is.null(page_df)){
              page_list[[page]] = page_df
            }
            page = page + 1
          }
          rbindlist(page_list, fill=T)
        }
        
        # specific_org_extract("GB-GOV-15")
        
transaction_list_recipient <- lapply(partner_activity_comb$iati_identifier, specific_trans_extract_recipient)
transaction_list_recipient = rbindlist(transaction_list_recipient, fill=T)
        
# Save to Rdata file
#saveRDS(transaction_list_recipient, file = "Outputs/transaction_list_recipient_kp.rds")
transaction_list_recipient <- readRDS(file = "Outputs/transaction_list_recipient_kp.rds")        

        
    # Extract receiver organisations
        transaction_receiver_orgs <- transaction_list_recipient %>% 
          select(iati_identifier, transaction_receiver_org_narrative) %>% 
            # unnest organisation names
          #filter(lengths(receiver_organisation.narrative) != 0) %>% 
          unnest(cols = transaction_receiver_org_narrative) %>% 
          #select(-lang.code, -lang.name) %>% 
          #rename(transaction_receiver_name = text) %>% 
            # Exclude blanks, other text
          filter(!is.na(transaction_receiver_org_narrative), 
                 !str_detect(str_to_lower(transaction_receiver_org_narrative), "reimbursement"),
                 !str_detect(str_to_lower(transaction_receiver_org_narrative), "disbursement")) %>% 
          unique()
        
            # Add to organisation name and country database
            receiver_orgs_to_save <- transaction_receiver_orgs %>% 
              inner_join(no_partner_info, by = "iati_identifier") %>% 
              rename(project_id = iati_identifier,
                     organisation_name = transaction_receiver_org_narrative) %>% 
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
          summarise(transaction_receiver_org_narrative = paste(coalesce(transaction_receiver_org_narrative, ""), collapse = ", "))
       
         # Summarise org countries for joining to main dataset
        transaction_org_countries_summarised <- receiver_orgs_to_save %>% 
          select(iati_identifier = project_id, organisation_country) %>% 
          unique() %>% 
          filter(!is.na(organisation_country)) %>% 
          group_by(iati_identifier) %>% 
          summarise(transaction_receiver_country = paste(coalesce(organisation_country, ""), collapse = ", "))
        
      
      # Join on transactions country and org info to relevant datasets
        activity_list_unnest_2 <- partner_activity_comb %>% 
          select(iati_identifier) %>% 
          left_join(activity_list_unnest_2, by = "iati_identifier") %>%
          left_join(transaction_countries_summarised, by = "iati_identifier") %>% 
          mutate(recipient_country = coalesce(recipient_country_region, recipient_country)) %>% 
          select(-recipient_country_region)

        
        activity_list_unnest_4 <- partner_activity_comb %>% 
          select(iati_identifier) %>% 
          left_join(activity_list_unnest_4, by = "iati_identifier") %>%
          left_join(transaction_orgs_summarised, by = "iati_identifier") %>% 
          left_join(transaction_org_countries_summarised, by = "iati_identifier") %>% 
          mutate(partner = coalesce(partner, transaction_receiver_org_narrative),
                 partner_country = coalesce(partner_country, transaction_receiver_country)) %>% 
          select(-transaction_receiver_org_narrative, -transaction_receiver_country)


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
  mutate(reporting_org = coalesce(reporting_org_narrative, reporting_org_ref, gov_funder))

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
             TRUE ~ gov_funder)) %>% unique()


# Save to Rdata file
#saveRDS(activity_list, file = "Outputs/partner_activity_list_kp_v2.rds")
activity_list <- readRDS(file = "Outputs/partner_activity_list_kp.rds")
#write.xlsx(activity_list, file = "Outputs/partner_activity_list_kp_v3.xlsx")

# Save org names and countries to file
saveRDS(org_names_and_locations_1, file = "Outputs/org_names_and_locations_1_kp.rds")

# Clear environment
rm(partner_activity_extract, partnership_activities, partner_activities_via_title, partner_activities_via_funder,
   result, new_rows, x, y, page, ri_linked_activites,
   activity_list_base, activity_list_unnest_1, activity_list_unnest_2, activity_list_unnest_3, activity_list_unnest_4,
   activity_list_unnest_4_partner_countries, activity_list_unnest_4_partner_names, activity_list_unnest_5,
   activity_list_unnest_6, activity_list_unnest_7, unlinked_partner_iati_activity_ids)
