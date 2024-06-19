# --------------------------------------------------------------- #
# Script 1
# Extract ODA research & innovation (R&I) activity information from 
# UK government departments' IATI data 
# --------------------------------------------------------------- #

# 1) Filter the OECD research sector codes from the sector list ------


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
                        "GB-GOV-25", # DESNZ
                        "GB-GOV-50") # Prosperity Fund

# organisation_codes <- c("GB-GOV-1")
# 
# organisation_codes <- c("GB-GOV-7",  # Defra
#                         "GB-GOV-10", # DHSC
#                         "GB-GOV-12", # DCMS
#                         "GB-GOV-13", # BEIS
#                         "GB-GOV-15", # DIT
#                         "GB-GOV-50") # Prosperity Fund

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
nrow(uk_gov_list_final[uk_gov_list_final$reporting_org_ref=="GB-GOV-1",])

# part_orgs_fcdo <- unique(unlist(unique(uk_gov_list_final$participating_org_narrative)))
# uk_gov_list_final %>% filter(str_detect(participating_org_narrative,"UNEP"))

#FCDO_final <- lapply(organisation_codes, specific_org_extract)
#FCDO_final = rbindlist(FCDO_final, fill=T)
#nrow(FCDO_final[FCDO_final$reporting_org_ref=="GB-GOV-1",])

# Save output data
# You can bypass the previous step by reading it in from the Outputs folder
# saveRDS(uk_gov_list_final, file = "Outputs/uk_gov_list_final_Jul24update.rds")
uk_gov_list_final <- readRDS(file = "Outputs/uk_gov_list_final_Jul24update.rds")

# FCDO_tagcode <- uk_gov_list_final %>% unnest(col = tag_code) %>% filter(reporting_org_ref == "GB-GOV-1", !is.na(tag_code)) %>% select(iati_identifier, tag_code, description_narrative)
# write.xlsx(FCDO_tagcode, file = "C:/Users/KimPrior/OneDrive - FCDO/Documents/RED bits/Management Info/UKCDR Climate Tracker commission/FCDO_tagcode.xlsx")

# 3) Filter to keep ODA R&I activities only ------
# (via the IATI "RI" tag field eventually - not all gov departments use this yet)

# Unnest tags
uk_gov_ri_programmes <- uk_gov_list_final %>%
  unnest(col = tag_code)


#this is unnesting the sector codes from the main data pull coz i'm worried things are being excluded just using the RI tag
uk_gov_ri_sector <- uk_gov_list_final %>%
  unnest(col = sector_code) %>%
  filter(sector_code %in% c(sector_list_research$code))


# Save list of tagged research & innovation activities using uk_gov_list_final
ri_iati_activities <- uk_gov_ri_programmes %>% 
  filter(tag_code == "RI"|str_detect(title_narrative, "research|Research|CEPI")) %>% 
  select(iati_identifier) %>% 
  unique() %>% 
  mutate(tag_code = "RI")

ri_iati_activities <- ri_iati_activities %>%
  mutate(nchar = nchar(iati_identifier))

ri_iati_activities_old <- ri_iati_activities %>%
  filter(str_detect(iati_identifier, "GB-1-")) %>%
  filter(nchar == 11) %>%
  mutate(prog_id = str_sub(iati_identifier,-6,-1))

ri_iati_activities_new <- ri_iati_activities %>%
  filter(str_detect(iati_identifier, "GB-GOV-1-")) %>%
  filter(nchar == 15) %>%
  mutate(prog_id = str_sub(iati_identifier,-6,-1))

# just checking that all the projects are picked up if one part of the prog/proj gets tagged as RI
combined_identifiers <- c(ri_iati_activities_old$iati_identifier,ri_iati_activities_new$iati_identifier)
pattern <- paste(combined_identifiers, collapse = "|")

ri_iati_activities_all_temp <- uk_gov_list_final %>% filter(str_detect(iati_identifier, pattern))

length(unique(ri_iati_activities_all_temp$iati_identifier))#2352
length(unique(ri_iati_activities$iati_identifier))#2290
length(unique(uk_gov_ri_sector$iati_identifier))#6962

setdiff(unique(ri_iati_activities_all_temp$iati_identifier),unique(ri_iati_activities$iati_identifier))#find elements in all_temp that are not in the original

#checking which RED progs are being picked up, then excluding those which we already have got from ri_iati_activities
pattern2 <- paste(RED_programmes_may24$prog_id, collapse = "|")
ri_iati_activities_all_temp_unique <- ri_iati_activities_all_temp %>% 
  filter(!iati_identifier %in% ri_iati_activities$iati_identifier) %>%
  filter(str_detect(iati_identifier, pattern2))

ri_iati_activities <- ri_iati_activities %>%
  select(iati_identifier,tag_code)

ri_iati_activities_all_temp_unique <- ri_iati_activities_all_temp_unique %>%
  select(iati_identifier) %>%
  mutate(tag_code = "RI")

#binding together the lists of activities using the RI tag, and the extra activities found using the RED list of prog numbers
ri_iati_activities <- ri_iati_activities %>%
  rbind(ri_iati_activities_all_temp_unique)

ri_iati_activities <- unique(ri_iati_activities)

# double check that its grabbing all FCDO research programmes (compare to CfD dashboard list)
pattern2 <- as.character(RED_programmes_may24$prog_id)

ri_iati_activities <- ri_iati_activities %>%
  mutate(prog_numb = str_extract(iati_identifier, "\\b\\d{6}\\b"))

setdiff(unique(RED_programmes_may24$prog_id),unique(ri_iati_activities$prog_numb))#find elements in RED_programmes_may24 that are not in the iati extract

# finally i want to use the research sector codes to see if we are missing any activities this way
uk_gov_ri_sector
setdiff(unique(uk_gov_ri_sector$iati_identifier),unique(ri_iati_activities$iati_identifier))#find elements in uk_gov_ri_sector (using research sector codes) that are not in the iati extract using the RI tag or RED prog numbers
uk_gov_ri_sector[uk_gov_ri_sector$iati_identifier=="GB-GOV-1-301554",]# this is getting picked up now, so that's good

#this is now removing the activities previously picked up by prev methods (RI tag and RED progs)
uk_gov_ri_sector_temp <- uk_gov_ri_sector %>% 
  filter(!iati_identifier %in% ri_iati_activities$iati_identifier) %>%
  select(iati_identifier) %>%
  mutate(tag_code = "RI")

ri_iati_activities <- ri_iati_activities %>% select(!prog_numb)

# binding the dataframe together
ri_iati_activities <- ri_iati_activities %>%
  rbind(uk_gov_ri_sector_temp)

# hopefully my final list of RI projects
# plan to compare this list with the list that comes out the end of this script
ri_iati_activities <- unique(ri_iati_activities)

# You can bypass the previous step by reading it in from the Outputs folder
# saveRDS(ri_iati_activities, file = "Outputs/ri_iati_activities_Jul24update.rds")
ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities_Jul24update.rds")
# write.xlsx(ri_iati_activities, file = "Outputs/ri_iati_activities_Jul24update.xlsx")


# Filter list of gov department IATI activities 

uk_gov_list_filtered <- uk_gov_list_final %>% 
  select(-tag_code) %>% 
  left_join(ri_iati_activities, by = "iati_identifier") %>% 
  filter((reporting_org_ref %in% c("GB-GOV-7", "GB-GOV-10", "GB-GOV-15", "GB-GOV-50") | # Include everything from these gov departments
          str_detect(iati_identifier, "GB-GOV-3") |                               # Include everything ex-FCO
          !is.na(tag_code) |                                                      # Include tagged R&I programmes
          str_detect(iati_identifier, "NEWT|Newton|NF|GCRF|NIHR|GAMRIF|UKVN|OODA"))) %>%   # Include BEIS Newton/GCRF, as well as OODA and DHSC GHS/GHR activities
  filter(default_flow_type_code == "10" | is.na(default_flow_type_code))        # Restrict to ODA funding only


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


# paste_test <- data.frame(a = c(101,102,102,103,103), b = c("G","G","G","G","G"), c = c("i am going","to test this","in my dataframe","to see if i","can fix this paste issue"))
# 
# paste_test %>%
#   unite(new, c("b","c"),sep = ":") %>%
#   group_by(a) %>%
#   mutate(new2 = paste0(new, collapse = "; ")) %>%
#   select(a, new2) %>%
#   unique()

# A) Unlist activity title and description
gov_list_unnest_1 <- uk_gov_list_filtered %>% 
   # title
  filter(lengths(title_narrative) != 0) %>%
  unnest(cols = title_narrative) %>% 
  #rename(activity_title = text) %>% 
   # description
  unnest(cols = c(description_narrative,description_type)) %>% 
  mutate(description_type = str_replace(description_type, "1", "General"),
         description_type = str_replace(description_type, "2", "Objectives"),
         description_type = str_replace(description_type, "3", "Target Groups")) %>%
  mutate(description_type = coalesce(description_type, "General")) %>% 
  select(iati_identifier, title_narrative, description_type, description_narrative) %>%
  unite(description, c("description_type", "description_narrative"), sep = ": ") %>%
  group_by(iati_identifier, title_narrative) %>%
  mutate(new_description_narrative = paste0(description, collapse = "; ")) %>%
  select(iati_identifier, title_narrative, new_description_narrative) %>% 
  #unnest(cols = narrative) %>%     
  unique() %>% rename(activity_title = title_narrative, activity_description = new_description_narrative)

      # Summarise records with multiple "General" descriptions
      # gov_list_unnest_1 <- gov_list_unnest_1 %>%
      #       group_by(iati_identifier, activity_title, description_type) %>%
      #       summarise(text = paste(coalesce(text, ""), collapse = "\n\n")) %>%
      #       spread(key = description_type, value = text) %>%
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
  select(iati_identifier, participating_org_role, participating_org_narrative) %>% 
  unnest(c(participating_org_role, participating_org_narrative)) %>% 
  filter(participating_org_role == "4") %>% 
  unique()


# gov_list_unnest_4$len_role <- lengths(gov_list_unnest_4$participating_org_role)
# gov_list_unnest_4$len_narr <- lengths(gov_list_unnest_4$participating_org_narrative)
# gov_list_unnest_4$len_ref <- lengths(gov_list_unnest_4$participating_org_ref)
# gov_list_unnest_4$diff_narr_role <- gov_list_unnest_4$len_narr - gov_list_unnest_4$len_role


    # Add country locations based on IATI org references or lookup
    # (takes ~5 mins to run)
     # gov_list_unnest_4 <- gov_list_unnest_4 %>%
     #  # Extract 2 digit country code from org references (where populated)
     #  mutate(country_code = if_else((!is.na(participating_org_ref) & substr(participating_org_ref,3,3) == "-" & !(substr(participating_org_ref,1,2) %in% c("XI", "XM"))), 
     #                                substr(participating_org_ref,1,2), ""))
  
    # Function searches the org title (e.g. Kenya) and returns a country if its present in the name
    gov_list_unnest_4$country_name2 <- map(gov_list_unnest_4$participating_org_narrative, org_country_lookup)
    gov_list_unnest_4$country_name2 <- as.character(gov_list_unnest_4$country_name2)
    gov_list_unnest_4$country_name2 <- ifelse(gov_list_unnest_4$country_name2 == "NA", "", gov_list_unnest_4$country_name2)
    
    
    # check if input is a valid 2-digit country code and then does a lookup to the countrycode_list
    # gov_list_unnest_4$country_name <- ifelse(is.na(gov_list_unnest_4$country_code) | nchar(gov_list_unnest_4$country_code) < 2, NA, countrycode_list$name[match(gov_list_unnest_4$country_code,countrycode_list$code)])
    # gov_list_unnest_4$country_name <- ifelse(is.na(gov_list_unnest_4$country_name), "", gov_list_unnest_4$country_name)
    
    # gov_list_unnest_4[gov_list_unnest_4 == ""] <- NA
    
    # This needs fixing because currently I don't know why it's only picking one of the country names when there might be several 
    # gov_list_unnest_4$partner_country <- coalesce(gov_list_unnest_4$country_name, gov_list_unnest_4$country_name2)
    
    gov_list_unnest_4 <- gov_list_unnest_4 %>% rename(partner_country = country_name2)
    
    # gov_list_unnest_4 <- gov_list_unnest_4%>% select(-country_name, -country_name2)
    
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
      select(-participating_org_narrative, -participating_org_role, -partner_country) %>% 
      left_join(gov_list_unnest_4_partners, by = "iati_identifier") %>% 
      left_join(gov_list_unnest_4_countries, by = "iati_identifier") %>%
      unique()
    
    # Identify activities with no implementing partner info
    no_partner_info <- gov_list_unnest_4 %>%
      select(iati_identifier) %>%
      mutate(has_implementing_partner_info = "Yes") %>%
      unique() %>%
      right_join(uk_gov_list_filtered, by = "iati_identifier") %>%
      filter(is.na(has_implementing_partner_info)) %>%
      select(iati_identifier) %>%
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
# gov_list_unnest_7 <- uk_gov_list_filtered %>% 
#   filter(lengths(budget_value) != 0) %>%
#   unnest(c(budget_status,budget_value,budget_value_currency,budget_period_start_iso_date,budget_period_end_iso_date)) %>% 
#   select(iati_identifier, 
#          budget_status, 
#          budget_value, 
#          budget_value_currency,
#          budget_period_start_iso_date,
#          budget_period_end_iso_date)
# 
# 
#     # Find activities with multiple budgets for same period (i.e. indicative and committed)
#     multiple_budgets <- gov_list_unnest_7 %>% 
#       select(iati_identifier, budget_status, budget_period_start_iso_date, budget_period_end_iso_date) %>%
#       unique() %>% 
#       group_by(iati_identifier, budget_period_start_iso_date, budget_period_end_iso_date) %>% 
#       summarise(count = n()) %>% 
#       filter (count > 1)
#     
#     # Keep only the committed budget in these cases
#     gov_list_unnest_7 <- gov_list_unnest_7 %>% 
#       filter(!(iati_identifier %in% multiple_budgets$iati_identifier) |
#              budget_status == "2")
#     
#     # Sum to get total budget per activity
#     gov_list_unnest_7 <- gov_list_unnest_7 %>% 
#       group_by(iati_identifier, budget_value_currency) %>% 
#       summarise(budget_period_start_iso_date = min(budget_period_start_iso_date),
#                 budget_period_end_iso_date = max(budget_period_end_iso_date),
#                 budget_value = sum(budget_value)) %>%
#       rename(period_start = budget_period_start_iso_date, period_end = budget_period_end_iso_date, amount = budget_value, currency = budget_value_currency)
#     

gov_list_unnest_7_test <- uk_gov_list_filtered %>% 
  select(iati_identifier, reporting_org_narrative, budget_status,budget_value,
         budget_value_currency,budget_period_start_iso_date,
         budget_period_end_iso_date) %>%
  filter(lengths(budget_value) != 0)

gov_list_unnest_7_test$len_val <- lengths(gov_list_unnest_7_test$budget_value)
gov_list_unnest_7_test$len_curr <- lengths(gov_list_unnest_7_test$budget_value_currency)
gov_list_unnest_7_test$diff <- gov_list_unnest_7_test$len_val - gov_list_unnest_7_test$len_curr

gov_list_unnest_7_test_curr <- gov_list_unnest_7_test %>%
  select(iati_identifier, budget_value_currency) %>%
  unnest(c(budget_value_currency))

gov_list_unnest_7_test_curr_unique <- gov_list_unnest_7_test_curr %>% 
  unique() %>%
  group_by(iati_identifier) %>%
  summarise(budget_value_currency = paste(budget_value_currency, collapse = ", "))

unique(gov_list_unnest_7_test_curr$budget_value_currency)

gov_list_unnest_7_test_val <- gov_list_unnest_7_test %>%
  select(iati_identifier, reporting_org_narrative, budget_status,budget_value,budget_period_start_iso_date,budget_period_end_iso_date) %>%
  unnest(c(budget_status,budget_value,budget_period_start_iso_date,budget_period_end_iso_date)) %>%
  unique()

gov_list_unnest_7_test_val$budget_value_currency <- "GBP"
gov_list_unnest_7 <- gov_list_unnest_7_test_val

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

# saveRDS(gov_list, file = "Outputs/gov_list_raw_Jul24update.rds")
gov_list <- readRDS("Outputs/gov_list_raw_Jul24update.rds")

# Remove non-research activities for all departments apart from BEIS, DHSC, FCDO based on sector information
gov_list <- gov_list %>%
  filter(reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10", "GB-GOV-13") & !str_detect(iati_identifier, "GB-GOV-3") |
           !is.na(sector_name))

gov_list[gov_list$iati_identifier=="GB-GOV-1-301376",]
gov_list[gov_list$iati_identifier=="GB-GOV-1-301554",]

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
    str_detect(iati_identifier, "Newton|NEWT|NF") ~ "DSIT - Newton Fund",
    str_detect(iati_identifier, "GCRF") ~ "DSIT - Global Challenges Research Fund (GCRF)",
    str_detect(iati_identifier, "UKVN") ~ "DHSC - Global Health Security - UK Vaccine Network",
    str_detect(iati_identifier, "GAMRIF") ~ "DHSC - Global Health Security - GAMRIF",
    (str_detect(iati_identifier, "NIHR") | str_detect(activity_title, "NIHR")) ~ "DHSC - Global Health Research - Programmes",
    str_detect(iati_identifier, "ICF") ~ "DSIT - International Climate Finance (ICF)",
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
  filter((reporting_org_ref %in% c("GB-GOV-1", "GB-GOV-10") & hierarchy != 1) | # FCDO, DHSC - keep child activities 
            str_detect(iati_identifier, "GB-GOV-3") |                           # keep ex-FCO activities
            reporting_org_ref %in% c("GB-GOV-7", "GB-GOV-12", "GB-GOV-13", "GB-GOV-50")) # Defra, DCMS, BEIS, Prosperity Fund - keep parent activities

gov_list_final[gov_list_final$iati_identifier=="GB-GOV-1-300294-101",]

# Join on FCDO parent (programme) descriptions to child (component) activities
gov_list_final <- gov_list_final %>%
     # Extract FCDO programme activity ID
  mutate(programme_id = if_else(hierarchy == 2 & fund == "FCDO Research - Programmes",
                                substr(iati_identifier, 1, nchar(iati_identifier)-4), NA_character_))
  # %>%
  #    # Join on programme title
  # left_join(select(gov_list_unnest_1,
  #                  iati_identifier,
  #                  programme_title = activity_title,
  #                  programme_description = activity_description),
  #           by = c("programme_id" = "iati_identifier")) %>%
  # mutate(activity_description = if_else(reporting_org_ref == "GB-GOV-1",
  #                                       programme_description,
  #                                       activity_description))

# 6) Save to Rdata file ----
# saveRDS(gov_list_final, file = "Outputs/gov_list_final_Jul24update.rds")
gov_list_final <- readRDS("Outputs/gov_list_final_Jul24update.rds")
# write.xlsx(gov_list_final, file = "Outputs/gov_list_final_kp.xlsx")

length(unique(gov_list_final$programme_id))

# uk_activity_ids_orig = unique(gov_list_final_orig$iati_identifier)
# uk_activity_ids_new = unique(gov_list_final$iati_identifier)
# uk_activity_ids_new <- setdiff(uk_activity_ids_new, uk_activity_ids_orig)

# I) Extract transactions

##### this extracts countries listed in the transactions####

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

uk_activity_ids = unique(gov_list_final$iati_identifier)
uk_activity_ids <- URLencode(uk_activity_ids)


# batching seems not to be working, so going to try one by one again
gov_transaction_list_countries <- lapply(uk_activity_ids, specific_trans_extract_country)
gov_transaction_list_countries = rbindlist(gov_transaction_list_countries, fill=T)

# gov_transaction_list_countries_new <- lapply(uk_activity_ids_new, specific_trans_extract_country)
# gov_transaction_list_countries_new = rbindlist(gov_transaction_list_countries_new, fill=T)
# 
# batch_size = 10
# batches = c()
# current_batch = c()
# for(i in 1:length(uk_activity_ids)){
#   current_id = uk_activity_ids[i]
#   if(i %% batch_size == 0){
#     current_batch_str = paste0('("', paste(current_batch, collapse = '"OR"'), '")')
#     batches = c(batches, current_batch_str)
#     current_batch = c(current_id)
#   } else {
#     current_batch = c(current_batch, current_id)
#   }
# }

# gov_transaction_list_countries <- lapply(batches, specific_trans_extract_country)
# gov_transaction_list_countries <- rbind(gov_transaction_list_countries,gov_transaction_list_countries_new, fill=TRUE)

# Save to Rdata file
# saveRDS(gov_transaction_list_countries, file = "Outputs/gov_transaction_list_countries_Jul24update.rds")
gov_transaction_list_countries <- readRDS(file = "Outputs/gov_transaction_list_countries_Jul24update.rds")

gov_transaction_list_countries$recipient_country <- countrycode_list$name[match(gov_transaction_list_countries$transaction_recipient_country_code,countrycode_list$code)]

# Extract recipient countries (where included in transactions)
gov_transaction_countries <- gov_transaction_list_countries %>% 
  select(iati_identifier, recipient_country) %>% 
  # rename and remove blanks
  filter(!is.na(recipient_country)) %>% 
  unique()

# Summarise countries for joining to main dataset
gov_transaction_countries_summarised <- gov_transaction_countries %>% 
  group_by(iati_identifier) %>% 
  summarise(recipient_country = paste(coalesce(recipient_country, ""), collapse = ", "))


##### this extracts recipients listed in the transactions####

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

uk_activity_ids = unique(gov_list_final$iati_identifier)
uk_activity_ids <- URLencode(uk_activity_ids)

# batch_size = 15
# batches = c()
# current_batch = c()
# for(i in 1:length(uk_activity_ids)){
#   current_id = uk_activity_ids[i]
#   if(i %% batch_size == 0){
#     current_batch_str = paste0('("', paste(current_batch, collapse = '" OR "'), '")')
#     batches = c(batches, current_batch_str)
#     current_batch = c(current_id)
#   } else {
#     current_batch = c(current_batch, current_id)
#   }
# }

# re-run this code on Monday!! quota probably finished

gov_transaction_list_recipient <- lapply(uk_activity_ids, specific_trans_extract_recipient)
bind_gov_transaction_list_recipient = rbindlist(gov_transaction_list_recipient, fill=TRUE)

# gov_transaction_list_recipient_new <- lapply(uk_activity_ids_new, specific_trans_extract_recipient)
# gov_transaction_list_recipient_new = rbindlist(gov_transaction_list_recipient_new, fill=T)

# Save to Rdata file
# saveRDS(bind_gov_transaction_list_recipient, file = "Outputs/bind_gov_transaction_list_recipient_Jan24update.rds")
bind_gov_transaction_list_recipient <- readRDS(file = "Outputs/bind_gov_transaction_list_recipient_Jan24update.rds")

nonsense_recipients <- c("Various","Staff Member","Correction","Supplier Name Withheld","Customer","Journal Transaction")

# Extract receiver organisations
gov_transaction_recipient <- bind_gov_transaction_list_recipient %>% 
  select(iati_identifier, transaction_receiver_org_narrative) %>% 
  # unnest organisation names
  #filter(lengths(receiver_organisation.narrative) != 0) %>% 
  unnest(cols = transaction_receiver_org_narrative) %>% 
  #select(-lang.code, -lang.name) %>% 
  #rename(transaction_receiver_name = text) %>% 
  # Exclude blanks, other text
  filter(!is.na(transaction_receiver_org_narrative), 
         !transaction_receiver_org_narrative %in% nonsense_recipients,
         !str_detect(str_to_lower(transaction_receiver_org_narrative), "reimbursement"),
         !str_detect(str_to_lower(transaction_receiver_org_narrative), "disbursement")) %>% 
  unique()



# Add to organisation name and country database
receiver_orgs_to_save <- gov_transaction_recipient %>% 
  left_join(no_partner_info, by = "iati_identifier") %>% 
  rename(project_id = iati_identifier,
         organisation_name = transaction_receiver_org_narrative) %>% 
  # Look up country from both country code and organisation name
  mutate(organisation_country = map(organisation_name, org_country_lookup)) %>% 
  mutate(organisation_country = unlist(organisation_country)) %>% 
  mutate(organisation_role = 2) # partners

# Add on to org file to save
org_names_and_locations_1 <- receiver_orgs_to_save


# Summarise orgs for joining to main dataset
gov_transaction_recipient_summarised <- gov_transaction_recipient %>% 
  group_by(iati_identifier) %>% 
  summarise(transaction_receiver_org_narrative = paste(coalesce(transaction_receiver_org_narrative, ""), collapse = ", "))

# Summarise org countries for joining to main dataset
gov_transaction_org_countries_summarised <- receiver_orgs_to_save %>% 
  select(iati_identifier = project_id, organisation_country) %>% 
  unique() %>% 
  filter(!is.na(organisation_country)) %>% 
  group_by(iati_identifier) %>% 
  summarise(transaction_receiver_country = paste(coalesce(organisation_country, ""), collapse = ", "))


# Join on transactions country and org info to relevant datasets
gov_list_final <- gov_list_final %>% 
  left_join(gov_transaction_recipient_summarised, by = "iati_identifier") %>%
  mutate(partner = coalesce(partner, transaction_receiver_org_narrative)) %>%
  select(!transaction_receiver_org_narrative)

gov_list_final <- gov_list_final %>% 
  left_join(gov_transaction_org_countries_summarised, by = "iati_identifier") %>%
  mutate(partner_country = coalesce(partner_country, transaction_receiver_country)) %>%
  select(!transaction_receiver_country)

gov_list_final <- gov_list_final %>% 
  left_join(gov_transaction_countries_summarised, by = "iati_identifier") %>%
  mutate(recipient_country_region = coalesce(recipient_country_region, recipient_country)) %>%
  select(!recipient_country)


# 6) Save to Rdata file ----
# saveRDS(gov_list_final, file = "Outputs/gov_list_final_Jan24update.rds")
gov_list_final <- readRDS("Outputs/gov_list_final_Jan24update.rds")
# write.xlsx(gov_list_final, file = "Outputs/gov_list_final_Jan24update.xlsx")

# Doing a lookup from the geocoding/location data to the IATI extract

RED_AMP_ids <- c(RED_AMP_location[[1]])
RED_AMP_location <- RED_AMP_location %>% rename(RED_id = Project_ID)

pat <- str_c(RED_AMP_ids, collapse = "|")

RED_extract <-
  gov_list_final %>% 
  filter(reporting_org_ref=="GB-GOV-1") %>%
  select(iati_identifier, recipient_country_region) %>%
  mutate(RED_id = str_extract_all(paste(iati_identifier), pat)) %>%
  filter(lengths(RED_id)>0)

RED_extract$RED_id <- as.character(RED_extract$RED_id)

RED_extract <-
  RED_extract %>%
  left_join(RED_AMP_location, by = "RED_id")

RED_extract$recipient_country <- ""
RED_extract$recipient_country <- ifelse(nchar(RED_extract$recipient_country_region) < nchar(RED_extract$Country_Name) & grepl("region", RED_extract$recipient_country_region, ignore.case = TRUE), RED_extract$Country_Name, RED_extract$recipient_country_region)
RED_extract$recipient_country <- ifelse(is.na(RED_extract$recipient_country), RED_extract$Country_Name, RED_extract$recipient_country)
RED_extract <- RED_extract %>% select(iati_identifier,RED_id,recipient_country)


gov_list_final_red <-
  gov_list_final %>%
  left_join(RED_extract, by = "iati_identifier") %>%
  mutate(recipient_country_region = coalesce(recipient_country,recipient_country_region)) %>%
  select(-RED_id, -recipient_country) %>% 
  rename(recipient_country = recipient_country_region)



# saveRDS(gov_list_final_red, file = "Outputs/gov_list_final_red_Jan24update.rds")
# write.xlsx(gov_list_final_red, file = "Outputs/gov_list_final_red_Jan24update.xlsx")
gov_list_final_red <- readRDS("Outputs/gov_list_final_red_Jan24update.rds")

# Clear environment variables
rm(uk_gov_list_filtered, gov_list, gov_list_base, gov_list_unnest_1, gov_list_unnest_2, gov_list_unnest_2_country, gov_list_unnest_2_region, gov_list_unnest_3, 
   gov_list_unnest_4, gov_list_unnest_4_partners, gov_list_unnest_4_countries, gov_list_unnest_5, gov_list_unnest_6, 
   gov_list_unnest_7, gov_list_unnest_8, multiple_budgets, sector_list, sector_list_research, uk_gov_ri_programmes,
   gov_list_final, organisation_codes)

