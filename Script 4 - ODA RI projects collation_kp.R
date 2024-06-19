# --------------------------------------------------------------- #
# Script 4 
# Extract and collate ODA R&I award level data from
# - IATI Registry (UK government funder and external partner activities)
# - UKRI Gateway to Research
# - NIHR Open Data
# - Wellcome Trust (spreadsheet input)
# - DHSC Global Health Security (non-UKRI projects) (spreadsheet input)
# - DHSC Co-funded projects
# - BEIS GCRF and Newton Fund (spreadsheet input)
# --------------------------------------------------------------- #

# Read in org names and countries from previous script
org_names_and_locations_1 <- readRDS(file = "Outputs/org_names_and_locations_1_Jan24update.rds")
org_names_and_locations_1_lup <- org_names_and_locations_1
org_names_and_locations_1_lup <- org_names_and_locations_1_lup %>% rename(partner_org_name = organisation_name)

org_names_and_locations_1_lup <- stringdist_join(org_names_and_locations_1_lup, modari_org_lookup, by='partner_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist') %>% 
  #group_by(partner_org_name) %>% 
  slice_min(order_by=dist, n=1)

org_names_and_locations_1_lup <- org_names_and_locations_1_lup %>% select(partner_org_name.x, partner_org_country) %>% rename(organisation_name = partner_org_name.x)

org_names_and_locations_1 <- org_names_and_locations_1 %>% 
  left_join(org_names_and_locations_1_lup, by = "organisation_name") %>%
  unique()


org_names_and_locations_1 <- org_names_and_locations_1 %>% 
  mutate(organisation_country = coalesce(organisation_country, partner_org_country)) %>%
  select(!partner_org_country)



# Read in linked partner IATI activity info from script 2
ri_linked_activites <- readRDS(file = "Outputs/ri_linked_activites_Jan24update.rds")

# 1) Extract IATI projects ------------------------------------------------

# Read in list of IATI activities (from UK gov funders and select delivery partners)
# gov_iati_list <- readRDS(file = "Outputs/gov_list_final_kp.rds")
gov_list_final_red <- readRDS(file = "Outputs/gov_list_final_red_Jan24update.rds")
activity_list <- readRDS(file = "Outputs/partner_activity_list_Jan24update_v2.rds")
gov_non_iati_ids <- paste0(gov_non_iati_programmes$iati_identifier, collapse = "|")

# gov_non_iati_programmes <- RED_list_final[is.na(RED_list_final$recipient_country_region)]
# gov_non_iati_programmes <- gov_non_iati_programmes %>% select(iati_identifier,`Country Name`)
# gov_non_iati_ids <- paste0(unique(gov_non_iati_programmes$iati_identifier), collapse = "|")

# Filter gov department records for project-level activities
# iati_projects <- gov_list_final_red %>%
#   filter(str_detect(iati_identifier, "GB-GOV-3|GB-GOV-7") |     # include ex-FCO and Defra activities
#         str_detect(iati_identifier, gov_non_iati_ids)         # keep FCDO/DHSC programmes funding out of scope of IATI
#         ) %>%
#   mutate(fund = if_else(is.na(fund), "Unknown", fund)) %>% 
#   plyr::rbind.fill(activity_list) # Add partner activities

# ukri_orgs <- c("ACADEMY OF MEDICAL SCIENCES","BIOTECHNOLOGY & BIOLOGICAL SCIENCES RESEARCH COUNCIL","ECONOMIC & SOCIAL RESEARCH COUNCIL","ENGINEERING & PHYSICAL SCIENCES RESEARCH COUNCIL","MEDICAL RESEARCH COUNCIL","NATURAL ENVIRONMENT RESEARCH COUNCIL","SCIENCE AND TECHNOLOGY FACILITIES COUNCIL","UK RESEARCH AND INNOVATION")
# ukri_iati_projects <- gov_list_final_red %>%
#   #filter(extending_org %in% ukri_orgs) %>%
#   mutate(gtr_id = str_replace(iati_identifier, "GB-GOV-13-FUND--GCRF-", "")) %>% 
#   mutate(gtr_id = str_replace(gtr_id, "GB-GOV-13-FUND--Newton-", "")) %>% 
#   mutate(gtr_id = str_replace_all(gtr_id, "_", "/")) %>%
#   select(gtr_id, iati_identifier, recipient_country) %>% 
#   unique()

ukri_iati_projects <- gov_list_final_red %>%
  filter(str_detect(iati_identifier, "GB-GOV-13-FUND--GCRF-|GB-GOV-13-FUND--Newton-")) %>%
  mutate(gtr_id = str_replace(iati_identifier, "GB-GOV-13-FUND--GCRF-", "")) %>% 
  mutate(gtr_id = str_replace(gtr_id, "GB-GOV-13-FUND--Newton-", "")) %>% 
  mutate(gtr_id = str_replace_all(gtr_id, "_", "/")) %>%
  select(gtr_id, iati_identifier, recipient_country) %>% 
  unique()


unique(DSIT$extending_org)

# I'm now exploring how i can keep some of the FCDO activities that are incorrectly (i think) getting filtered out with the above code
# the below codes filters out the DSIT entries on IATI (so i need to make sure i capture the relevant activities via GtR etc below)
iati_projects <- gov_list_final_red %>%
  filter(!iati_identifier %in% ri_linked_activites$activity_id) %>%
  filter(str_detect(iati_identifier, "GB-1-|GB-GOV-1-|GB-GOV-3|GB-GOV-7")) %>%
  mutate(fund = if_else(is.na(fund), "Unknown", fund)) %>% 
  plyr::rbind.fill(activity_list) # Add partner activities

# Identify UKRI projects (by "RI" IATI tag)
# ukri_iati_projects <- gov_list_final_red %>% 
#   filter(extending_org == "UK Research & Innovation") %>% 
#   #filter(extending_org == "UK RESEARCH AND INNOVATION") %>% 
#   mutate(gtr_id = str_replace(iati_identifier, "GB-GOV-13-FUND--GCRF-", "")) %>% 
#   mutate(gtr_id = str_replace(gtr_id, "GB-GOV-13-FUND--Newton-", "")) %>% 
#   mutate(gtr_id = str_replace_all(gtr_id, "_", "/")) %>%
#   select(gtr_id, iati_identifier, recipient_country) %>% 
#   unique()

# ukri_iati_projects <- gov_list_final %>% 
#   filter(str_detect(iati_identifier, "GB-GOV-13-FUND--GCRF-|GB-GOV-13-FUND--Newton-")) %>%
#   unique()

# BEIS is has paused publishing UKRI data due to issues, hoping to republish in the next couple of weeks
# this is temporary, was from a couple of months ago
# ukri_iati <- readxl::read_xlsx("Outputs/gov_list_final_kp_UKRI.xlsx", sheet=1, col_types = "text")
# 
# ukri_iati_projects <- ukri_iati %>%
#   filter(extending_org == "UK Research & Innovation") %>%
#   mutate(gtr_id = str_replace(iati_identifier, "GB-GOV-13-FUND--GCRF-", "")) %>%
#   mutate(gtr_id = str_replace(gtr_id, "GB-GOV-13-FUND--Newton-", "")) %>%
#   mutate(gtr_id = str_replace_all(gtr_id, "_", "/")) %>%
#   select(gtr_id, iati_identifier, recipient_country_region) %>%
#   rename(recipient_country = recipient_country_region) %>%
#   unique()

# below is me trying to work out which UKRI activities are missing

# ukri_proj_curr <- setdiff(gov_list_final_red$iati_identifier, ukri_iati_projects$iati_identifier)
# length(unique(ukri_iati_projects$iati_identifier))
# 
# View(gov_list_final_red[gov_list_final_red$iati_identifier %in% c(ukri_proj_curr),])
# gov_list_final_red[gov_list_final_red$iati_identifier=="GB-GOV-13-FUND--GCRF-EP_T02397X_1",]


# Add on beneficiary countries for FCDO non-IATI programmes
iati_projects <- iati_projects %>% 
  # remove any FCDO component numbers
  mutate(programme_iati_id = if_else(reporting_org_ref == "GB-GOV-1" &
                                      substr(iati_identifier, nchar(iati_identifier)-3, nchar(iati_identifier)-3) == "-",
                                     substr(iati_identifier, 1, nchar(iati_identifier)-4), iati_identifier)) %>% 
  left_join(gov_non_iati_programmes, by = "iati_identifier") %>% 
  mutate(recipient_country = coalesce(recipient_country, str_to_title(fcdo_geocoding_countries))) %>% 
  select(-fcdo_geocoding_countries)

iati_projects$fund <- ifelse(iati_projects$fund == "DSIT - Newton Fund", "DSIT - Newton Fund", 
                             ifelse(iati_projects$fund == "DSIT - International Climate Finance (ICF)", "DSIT - International Climate Finance (ICF)", iati_projects$fund))


# Keep required fields
iati_projects_final <- iati_projects %>% 
  mutate(Funder = coalesce(gov_funder, reporting_org),
         partner_org_name = partner,
         partner_org_country = partner_country,         
         lead_org_name = coalesce(extending_org, reporting_org),
         lead_org_country = reporting_org_country,
         extending_org = coalesce(extending_org, reporting_org),
         status = if_else(!is.na(end_date),
                          if_else(Sys.Date() <= end_date, "Active", "Closed"), "Unknown"),
         iati_id = coalesce(programme_id, iati_identifier),
         last_updated = quarter_end_date) %>% 
  select(id = iati_identifier,
         title = activity_title, 
         abstract = activity_description,
         start_date,
         end_date,
         amount,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name,
         lead_org_country,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund = fund,
         Funder, 
         recipient_country,
         subject = sector_name,
         status,
         last_updated
  ) 

# Add IATI link to awards
iati_projects_final <- iati_projects_final %>% 
  mutate(link = paste0("https://d-portal.org/ctrack.html#view=act&aid=", id))

iati_projects_final <- iati_projects_final %>% 
  filter(!str_detect(title, "non ODA|non oda|Non ODA|non-oda"))

iati_projects_final <- iati_projects_final %>%
  mutate(start_date = str_sub(start_date, 1, 10)) %>%
  mutate(end_date = str_sub(end_date, 1, 10)) %>%
  mutate(period_start = str_sub(period_start, 1, 10)) %>%
  mutate(period_end = str_sub(period_end, 1, 10))

iati_projects_final_org_filled <- iati_projects_final %>% select(id, lead_org_name, lead_org_country)
names(modari_org_lookup) <- c("lead_org_name","lead_org_country")

iati_projects_final_org_filled <- stringdist_join(iati_projects_final_org_filled, modari_org_lookup, by='lead_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist') %>% 
  #group_by(partner_org_name) %>% 
  slice_min(order_by=dist, n=1)


iati_projects_final <- iati_projects_final %>% 
  left_join(iati_projects_final_org_filled, by = "id") %>% 
  mutate(lead_org_country = coalesce(lead_org_country, lead_org_country.y)) %>%
  select(!c(lead_org_country.x, lead_org_name.x, lead_org_country.y,lead_org_name.y,dist))

iati_projects_final["Fund"][iati_projects_final["Fund"] == "BEIS - Newton Fund"] <- "DSIT - Newton Fund"
iati_projects_final["Fund"][iati_projects_final["Fund"] == "BEIS - International Climate Finance (ICF)"] <- "DSIT - International Climate Finance (ICF)"

# saveRDS(iati_projects_final, file = "Outputs/iati_projects_final_Jan24update.rds")
iati_projects_final <- readRDS("Outputs/iati_projects_final_Jan24update.rds")

# Clear environment
rm(gov_iati_list, partner_iati_list, iati_projects)


# 2) Extract UKRI projects -------------------------------------------

### A - Prepare project IDs and fund labels ###

# Label GCRF and Newton projects from IATI UKRI data
ukri_projects_by_fund <- ukri_iati_projects %>%
  mutate(Fund = case_when(
                   str_detect(iati_identifier, "GCRF") ~ "DSIT - Global Challenges Research Fund (GCRF)",
                   str_detect(iati_identifier, "Newton") ~ "DSIT - Newton Fund",
                   TRUE ~ "Other"
                        ),
         Funder = "Department for Science, Innovation and Technology")

# Join GCRF/Newton project IDs to other ODA IDs (from spreadsheet) - THIS OODA WILL NOW COME DIRCTLY FROM IATI, SO I'LL NEED TO ADD THEM IN SEPARATELY

# ukri_projects_ids_full <- ukri_projects_by_fund %>% 
#   rbind(ukri_ooda_projects_ids)
ukri_projects_ids_full <- ukri_projects_by_fund
ukri_projects_ids_full$gtr_id <- trimws(ukri_projects_ids_full$gtr_id, whitespace = "[\\h\\v]")
# ukri_projects_ids_full <- ukri_projects_ids_full %>%
#   filter(nchar(gtr_id) < 15)

ukri_projects_ids_full$gtr_id
ukri_projects_by_id_all_old$gtr_id

extra_proj <- setdiff(ukri_projects_ids_full$gtr_id, ukri_projects_by_id_all_old$gtr_id)

ukri_projects_ids_full <- ukri_projects_ids_full %>% filter(gtr_id %in% c(extra_proj))

### B - Extract project info from GtR API ###

# Create empty dataset to hold projects
ukri_projects_by_id <- data.frame()
org_names_and_locations_2 <- data.frame()

# Run project info extraction over all GtR projects
# (takes 1-2 hours to run)

n <- 1 # set counter

ukri_projects_by_id_all$gtr_id <- URLencode(ukri_projects_by_id_all$gtr_id)

# i had to run the code in 6 batches coz was running into problems

for (id in ukri_projects_by_id_all$gtr_id) {
  
  print(paste0(n, " - ", id))

  data <- extract_ukri_projects_by_id(id)
  
  # Separate elements of list
  project_data <- data[[1]]
  org_data <- data[[2]]
  
  # Add new data rows to existing tables
  ukri_projects_by_id <- ukri_projects_by_id %>% 
    rbind(project_data)
  
  org_names_and_locations_2 <- org_names_and_locations_2 %>% 
    rbind(org_data)
  
  # Increment counter for next cycle
  n <- n+1
  
}


saveRDS(ukri_projects_by_id, file = "Outputs/ukri_projects_by_id_Jan24update_b10.rds")

ukri_projects_by_id_b1 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b1.rds")
ukri_projects_by_id_b2 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b2.rds")
ukri_projects_by_id_b3 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b3.rds")
ukri_projects_by_id_b4 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b4.rds")
ukri_projects_by_id_b5 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b5.rds")
ukri_projects_by_id_b6 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b6.rds")

ukri_projects_by_id_b7 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b7.rds")
ukri_projects_by_id_b8 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b8.rds")
ukri_projects_by_id_b9 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b9.rds")
ukri_projects_by_id_b10 <- readRDS("Outputs/ukri_projects_by_id_Jan24update_b10.rds")

ukri_projects_by_id_all <- rbind(ukri_projects_by_id_b1,ukri_projects_by_id_b2,ukri_projects_by_id_b3,ukri_projects_by_id_b4,ukri_projects_by_id_b5,ukri_projects_by_id_b6,ukri_projects_by_id_b7,ukri_projects_by_id_b8,
                                 ukri_projects_by_id_b9,ukri_projects_by_id_b10)

# saveRDS(ukri_projects_by_id_all, file = "Outputs/ukri_projects_by_id_Jan24update.rds")
ukri_projects_by_id_all <- readRDS("Outputs/ukri_projects_by_id_Jan24update.rds")

# ukri_projects_ids_other <- setdiff(ukri_projects_by_fund$gtr_id, ukri_projects_by_id_all$gtr_id)

# ukri_projects_by_id <- readRDS("Outputs/ukri_projects_by_id_kp.rds")

# Save org names and countries to file
# saveRDS(org_names_and_locations_2, file = "Outputs/org_names_and_locations_2_Jan24update.rds")
org_names_and_locations_2 <- readRDS(file = "Outputs/org_names_and_locations_2_Jan24update.rds")


### C - Add on fund and funder labels

# Join to fund and funder info from original list
ukri_projects_by_id_with_id <- ukri_projects_by_id_all %>% 
  left_join(select(ukri_projects_ids_full, 
                   iati_id = iati_identifier, Fund, Funder, gtr_id), by = "gtr_id")

# DS_ukri_projects_by_id_with_id <- DS_ukri_projects_by_id %>% 
#   left_join(select(ukri_iati_DS_by_fund, 
#                    iati_id = `iati-identifier`, Fund, Funder, gtr_id = `Grant reference`), by = "gtr_id") 


# See which awards from input list have not been found
missing_awards <- select(ukri_projects_ids_full, gtr_id) %>% 
  left_join(select(ukri_projects_by_id_with_id, gtr_id, title), by = "gtr_id") %>% 
  filter(is.na(title)) %>% 
  unique()

# DS_missing_awards <- select(ukri_iati_DS_by_fund, `Grant reference`) %>% 
#   left_join(select(DS_ukri_projects_by_id_with_id, gtr_id = `Grant reference`, title), by = "gtr_id") %>% 
#   filter(is.na(title)) %>% 
#   unique()

# r bind the 2 UKRI datasets
# ukri_projects_by_id_with_id <- rbind(ukri_projects_by_id_with_id, DS_ukri_projects_by_id_with_id)

# Convert all factor fields to character

ukri_projects_final <- data.frame(lapply(ukri_projects_by_id_with_id, as.character), stringsAsFactors=FALSE)

# Output final dataset
ukri_projects_final <- ukri_projects_final %>% 
  rename(start_date = fund.start,
         end_date = fund.end,
         id = gtr_id,
  ) %>% 
  mutate(subject = NA_character_,
         amount = as.numeric(amount),
         period_start = NA_character_,
         period_end = NA_character_,
         currency = "GBP",
         Fund = if_else(Fund == "GCRF", "DSIT - Global Challenges Research Fund (GCRF)",
                        if_else(Fund == "Newton", "DSIT - Newton Fund", Fund)),
         extending_org = case_when(
           extending_org == "AHRC" ~ "Arts and Humanities Research Council (AHRC)",
           extending_org == "BBSRC" ~ "Biotechnology and Biological Sciences Research Council (BBSRC)",
           extending_org == "EPSRC" ~ "Engineering and Physical Sciences Research Council (EPSRC)",
           extending_org == "ESRC" ~ "Economic and Social Research Council (ESRC)",
           extending_org == "MRC" ~ "Medical Research Council (MRC)",
           extending_org == "NERC" ~ "Natural Environment Research Council (NERC)",
           extending_org == "STFC" ~ "Science and Technology Facilities Council (STFC)",
           extending_org == "GCRF" ~ "Global Challenges Research Fund (GCRF)",
           TRUE ~ extending_org
         ),
         last_updated = as.Date(last_updated)) %>% 
  select(id,
         title, 
         abstract,
         start_date,
         end_date,
         amount,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name,
         lead_org_country,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund,
         Funder, 
         subject,
         status,
         last_updated) %>% 
  
  unique()

# Add GtR link to projects
ukri_projects_final <- ukri_projects_final %>% 
  mutate(link = paste0("https://gtr.ukri.org/projects?ref=", id))

# Remove duplicates based on different ordered partners orgs (if a project is
# co-funded, its information will be extracted more than once from the GtR API
# and may be in a different order)

ukri_projects_final <- ukri_projects_final %>% 
  group_by(across(c(-partner_org_name))) %>% 
  slice(1) %>% 
  ungroup()

# Add on beneficiary countries from IATI
ukri_projects_with_countries <- ukri_projects_final %>% 
  left_join(ukri_iati_projects, by = c("id" = "gtr_id")) %>% 
  select(-iati_identifier)

# Save as R file (to read back in if needed)
# saveRDS(ukri_projects_with_countries, file = "Outputs/ukri_projects_with_countries_Jan24update.rds")
ukri_projects_with_countries <- readRDS("Outputs/ukri_projects_with_countries_Jan24update.rds")

ukri_projects_with_countries$partner_org_country[ukri_projects_with_countries$partner_org_country==""] <- NA

ukri_projects_with_countries_test <- ukri_projects_with_countries %>% select(id, partner_org_name, partner_org_country)
names(modari_org_lookup) <- c("partner_org_name","partner_org_country")

ukri_projects_with_countries_test_filled <- stringdist_join(ukri_projects_with_countries_test, modari_org_lookup, by='partner_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist') %>% 
  #group_by(partner_org_name) %>% 
  slice_min(order_by=dist, n=1)

ukri_projects_with_countries <- ukri_projects_with_countries %>% 
  left_join(ukri_projects_with_countries_test_filled, by = "id") %>% 
  mutate(partner_org_country = coalesce(partner_org_country, partner_org_country.y)) %>%
  select(!c(partner_org_country.x, partner_org_name.x, partner_org_country.y,partner_org_name.y,dist))


# Clear environment
rm(data, n, id, missing_awards, ukri_projects_by_fund,
   ukri_projects_by_id, ukri_projects_by_id_with_id,
   ukri_projects_ids_full, ukri_projects_final,
   ukri_ooda_projects_ids, ukri_iati_projects)



### Extract FCDO partnerships project info (taken from Emma last update) from GtR API ####

# Create empty dataset to hold projects
ukri_projects_by_id <- data.frame()
org_names_and_locations_4 <- data.frame()

# Run project info extraction over all GtR projects
# (takes 1-2 hours to run)

n <- 1 # set counter

fcdo_partnerships$id <- URLencode(fcdo_partnerships$id)

# i had to run the code in 6 batches coz was running into problems

for (id in fcdo_partnerships$id) {
  
  print(paste0(n, " - ", id))
  
  data <- extract_ukri_projects_by_id(id)
  
  # Separate elements of list
  project_data <- data[[1]]
  org_data <- data[[2]]
  
  # Add new data rows to existing tables
  ukri_projects_by_id <- ukri_projects_by_id %>% 
    rbind(project_data)
  
  org_names_and_locations_4 <- org_names_and_locations_4 %>% 
    rbind(org_data)
  
  # Increment counter for next cycle
  n <- n+1
  
}

ukri_projects_by_id_org <- ukri_projects_by_id %>% select(gtr_id, lead_org_name, lead_org_country)
names(modari_org_lookup) <- c("organisation_name","lead_org_country")

ukri_projects_by_id_org <- stringdist_join(ukri_projects_by_id_org, modari_org_lookup, by='lead_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist',ignore_case = TRUE) %>% 
  #group_by(partner_org_name) %>% 
  slice_min(order_by=dist, n=1)

ukri_projects_by_id <- ukri_projects_by_id %>% 
  left_join(ukri_projects_by_id_org, by = "gtr_id") %>% 
  mutate(lead_org_country = coalesce(lead_org_country, lead_org_country.y)) %>%
  select(!c(lead_org_country.x, lead_org_name.x, lead_org_country.y,lead_org_name.y,dist))

ukri_projects_by_id$abstract <- ifelse(ukri_projects_by_id$abstract=="Awaiting Public Project Summary", ukri_projects_by_id$title, ukri_projects_by_id$abstract)
ukri_projects_by_id$abstract <- ifelse(ukri_projects_by_id$abstract=="-", ukri_projects_by_id$title, ukri_projects_by_id$abstract)
ukri_projects_by_id <- unique(ukri_projects_by_id)

ukri_projects_by_id <- ukri_projects_by_id %>% left_join(fcdo_partnerships, join_by(gtr_id == id))

org_names_and_locations_4_fill <- stringdist_join(org_names_and_locations_4, modari_org_lookup, by='organisation_name', mode='left', method = "jw", max_dist=99, distance_col='dist',ignore_case = TRUE) %>% 
  #group_by(partner_org_name) %>% 
  slice_min(order_by=dist, n=1)

org_names_and_locations_4 <- org_names_and_locations_4_fill %>%
  mutate(organisation_country = coalesce(organisation_country, lead_org_country)) %>%
  select(!c(organisation_name.x, lead_org_country,dist)) %>%
  rename(organisation_name = organisation_name.y)

# saveRDS(org_names_and_locations_4, file = "Outputs/org_names_and_locations_4_Jan24update.rds")
org_names_and_locations_4 <- readRDS("Outputs/org_names_and_locations_4_Jan24update.rds")
# saveRDS(ukri_projects_by_id, file = "Outputs/fcdo_partnerships_ukri_Jan24update.rds")
ukri_projects_by_id <- readRDS("Outputs/fcdo_partnerships_ukri_Jan24update.rds")


fcdo_partnerships_ukri_formatted <- ukri_projects_by_id %>% 
  mutate(currency = "GBP",
         period_start = NA_character_,
         period_end = NA_character_,
         subject = NA_character_,
         Funder = "Foreign, Commonwealth and Development Office",
         Fund = "FCDO Research - Partnerships",
         recipient_country = NA_character_,
         subject = NA_character_,
         last_updated = quarter_end_date)

# Select desired variables
fcdo_partnerships_ukri_formatted <- fcdo_partnerships_ukri_formatted %>% 
  select(id = gtr_id,
         title, 
         abstract,
         start_date = fund.start,
         end_date = fund.end,
         amount,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name,
         lead_org_country,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund,
         Funder, 
         recipient_country,
         status,
         subject,
         last_updated
  ) 

# Add GtR link to projects
fcdo_partnerships_ukri_formatted <- fcdo_partnerships_ukri_formatted %>% 
  mutate(link = paste0("https://gtr.ukri.org/projects?ref=", id))

fcdo_partnerships_ukri_final <- fcdo_partnerships_ukri_formatted


# Extract OODA projects from IATI ------------------------------------------------

ooda_iati <- gov_list_final_red %>%
  filter(str_detect(iati_identifier, "OODA")) %>%
  mutate(start_date = str_sub(start_date, 1, 10)) %>%
  mutate(end_date = str_sub(end_date, 1, 10)) %>%
  mutate(period_start = str_sub(period_start, 1, 10)) %>%
  mutate(period_end = str_sub(period_end, 1, 10)) %>%
  mutate(partner = str_to_title(partner)) %>%
  mutate(extending_org = str_to_title(extending_org)) #%>%
  #mutate(end_date = as.Date(end_date, "%Y-%m-%d"))

# Keep required fields
ooda_iati_final <- ooda_iati %>% 
  mutate(Funder = reporting_org,
         partner_org_name = partner,
         partner_org_country = partner_country,         
         lead_org_name = extending_org,
         lead_org_country = "United Kingdom",
         extending_org = extending_org,
         status = if_else(!is.na(end_date),
                          if_else(Sys.Date() <= end_date, "Active", "Closed"), "Unknown"),
         iati_id = NA,
         last_updated = quarter_end_date) %>% 
  select(id = iati_identifier,
         title = activity_title, 
         abstract = activity_description,
         start_date,
         end_date,
         amount,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name,
         lead_org_country,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund = fund,
         Funder, 
         recipient_country,
         subject = sector_name,
         status,
         last_updated
  )

# Add IATI link to awards
ooda_iati_final <- ooda_iati_final %>% 
  mutate(link = paste0("https://d-portal.org/ctrack.html#view=act&aid=", id))

ooda_iati_final$Fund <- ifelse(ooda_iati_final$Fund == "BEIS - Newton Fund", "DSIT - Newton Fund", ooda_iati_final$Fund)

#ooda_iati_final <- ooda_iati_final %>%
#  mutate(end_date = as.Date(end_date, "%Y-%m-%d"))


# 3) Extract NIHR projects ------------------------------------------------

# Define URL to extract ODA projects
paths <- c("https://nihr.opendatasoft.com/api/records/1.0/search/?dataset=infonihr-open-dataset&q=&rows=6000&facet=funder&facet=project_status&facet=programme&facet=programme_type&facet=programme_stream&facet=start_date&facet=acronym&facet=ctry17nm&facet=rgn17nm&facet=lad19nm&facet=pconnm&refine.funder=NIHR+(ODA)"
          ,"https://nihr.opendatasoft.com/api/records/1.0/search/?dataset=nihr-open-data-global-health-downstream-partner-data&q=&rows=6000&facet=institutionname&facet=institutioncity&facet=institutioncountry&facet=projectref")


# Extract data from the NIHR API
  
  # Set counter and create output list
  i <- 1
  nihr_data <- list()

for (path in paths) {
  
  request <- GET(url = path)
  
  # Convert to text and read from JSON
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # Extract dataframe
  data <- response$records 
  
  # Remove "field." from column names
  names(data) <- gsub(pattern = "fields.", replacement = "", x = names(data))
  
  # Save output to list
  nihr_data[[i]] <- data
 
  i <- i+1
  
}

# Extract projects and partners datasets  
nihr_projects <- nihr_data[[1]]
nihr_partners <- nihr_data[[2]] %>% 
  select(project_id = projectref, 
         organisation_name = institutionname, 
         organisation_country = institutioncountry) %>% 
  unique()

  nihr_partners_names = nihr_partners %>% 
    select(project_id, organisation_name) %>% 
    unique() %>% 
    group_by(project_id) %>% 
    summarise(organisation_name = paste(coalesce(organisation_name, ""), collapse = ", "))
  
  nihr_partners_countries = nihr_partners %>% 
    select(project_id, organisation_country) %>% 
    unique() %>% 
    group_by(project_id) %>% 
    summarise(organisation_country = paste(coalesce(organisation_country, ""), collapse = ", ")) 
  
  nihr_partners_comb <- nihr_partners_names %>% 
    left_join(nihr_partners_countries, by = "project_id")

# Join datasets
nihr_projects_final <- nihr_projects %>% 
  left_join(nihr_partners_comb, by = "project_id")


# Select order of columns
nihr_projects_final <- nihr_projects_final %>% 
  mutate(id = project_id,
         Funder = "Department of Health and Social Care",
         Fund = "DHSC - Global Health Research - Programmes",
         recipient_country = NA_character_,
         lead_org_country = ctrynm,
         iati_id = NA_character_,
         subject = programme,
         currency = "GBP",
         status = if_else(project_status %in% c("Active", "Contracted"), "Active",
                          if_else(project_status %in% c("Complete"), "Closed", 
                                  if_else(project_status %in% c("Discontinued"), "Cancelled", "Unknown"))),
         period_start = NA_character_,
         period_end = NA_character_,
         partner_org_name = organisation_name,
         partner_org_country = organisation_country,
         extending_org = "NIHR",
         last_updated = as.Date(record_timestamp)) %>% 
  select(id, 
         title = project_title,
         abstract = scientific_abstract,
         start_date, end_date,
         amount = award_amount_from_dh,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name = contracted_organisation, 
         lead_org_country, 
         partner_org_name, partner_org_country,
         iati_id, 
         Fund,
         Funder,
         recipient_country,
         subject,
         status,
         last_updated)

# Add NIHR link to awards
nihr_projects_final <- nihr_projects_final %>% 
  mutate(link = paste0("https://fundingawards.nihr.ac.uk/award/", id))

# Write org names and countries to file
org_names_and_locations_3 <- nihr_projects_final %>% 
                    select(project_id = id,
                           organisation_name = lead_org_name,
                           organisation_country = lead_org_country) %>% 
                    mutate(organisation_role = 1) %>% 
                    rbind(mutate(nihr_partners,
                                 organisation_role = 2))


# saveRDS(org_names_and_locations_3, file = "Outputs/org_names_and_locations_3_Jan24update.rds")
org_names_and_locations_3 <- readRDS("Outputs/org_names_and_locations_3_Jan24update.rds")

# Save as R file (to read back in if needed)
# saveRDS(nihr_projects_final, file = "Outputs/nihr_projects_final_Jan24update.rds")
nihr_projects_final <- readRDS("Outputs/nihr_projects_final_Jan24update.rds")


# Clear environment
rm(paths, i, nihr_data, nihr_projects, nihr_partners, nihr_partners_comb,
   nihr_partners_names, nihr_partners_countries,
   request, response)


# Co-funding activities

names(DHSC_cofund_full)

dhsc_cofunding_final <- DHSC_cofund_full %>% 
  mutate(id = `Award ID on UKRI Research Gateway`,
         title = Title,
         abstract = Abstract,
         start_date = `Start Date`, 
         end_date = `End Date`,
         amount = `Funded Value`,
         Funder = "Department of Health and Social Care",
         Fund = "DHSC - Global Health Research - Partnerships",
         lead_org_name = `Lead Organisation`,
         recipient_country = NA_character_,
         lead_org_country = NA_character_,
         iati_id = NA_character_,
         subject = `Research Activities`,
         currency = "GBP",
         status = NA_character_,
         period_start = NA_character_,
         period_end = NA_character_,
         partner_org_name = NA_character_,
         partner_org_country = NA_character_,
         extending_org = `Delivery Partner`,
         status = NA_character_,
         last_updated = NA_character_,
         link = NA_character_) %>% 
  select(id, 
         title,
         abstract,
         start_date, 
         end_date,
         amount,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name, 
         lead_org_country, 
         partner_org_name, partner_org_country,
         iati_id, 
         Fund,
         Funder,
         recipient_country,
         subject,
         status,
         last_updated,
         link)

# Create empty dataset to hold projects
dhsc_cofunding_by_id <- data.frame()
org_names_and_locations_2.1 <- data.frame()

# Run project info extraction over all GtR projects
# (takes 1-2 hours to run)

n <- 1 # set counter

for (id in dhsc_cofunding_final$id) {
  
  print(paste0(n, " - ", id))
  
  data <- extract_ukri_projects_by_id(id)
  
  # Separate elements of list
  project_data <- data[[1]]
  org_data <- data[[2]]
  
  # Add new data rows to existing tables
  dhsc_cofunding_by_id <- dhsc_cofunding_by_id %>% 
    rbind(project_data)
  
  org_names_and_locations_2.1 <- org_names_and_locations_2.1 %>% 
    rbind(org_data)
  
  # Increment counter for next cycle
  n <- n+1
  
}

# saveRDS(dhsc_cofunding_by_id, file = "Outputs/dhsc_cofunding_by_id_Jan24update.rds")
dhsc_cofunding_by_id <- readRDS("Outputs/dhsc_cofunding_by_id_Jan24update.rds")

saveRDS(org_names_and_locations_2.1, file = "Outputs/org_names_and_locations_2.1_Jan24update.rds")
org_names_and_locations_2.1 <- readRDS("Outputs/org_names_and_locations_2.1_Jan24update.rds")

dhsc_cofunding_by_id <- dhsc_cofunding_by_id %>%
  rename(id = gtr_id) %>%
  select(id, lead_org_name, lead_org_country, partner_org_name, partner_org_country)

ukri_projects_for_dhsc <- ukri_projects_with_countries %>%
  select(id, recipient_country)

dhsc_cofunding_by_id <- dhsc_cofunding_by_id %>%
  left_join(ukri_projects_for_dhsc, by = "id")

dhsc_cofunding_join <- dhsc_cofunding_final %>%
  select(!c(lead_org_name, lead_org_country, partner_org_name, partner_org_country, recipient_country)) %>%
  left_join(dhsc_cofunding_by_id, by = "id")

dhsc_cofunding_join_tit <- dhsc_cofunding_join %>%
  mutate(recipient_country_title = str_extract_all(str_to_lower(title), countries_string)) %>% 
  unnest(cols = recipient_country_title, keep_empty = TRUE) %>% 
  mutate(recipient_country_title = str_to_title(recipient_country_title)) %>%
  group_by(id) %>%
  distinct(recipient_country_title) %>%
  #summarise(recipient_country_title = paste(coalesce(recipient_country_title, ""), collapse = ", ")) %>%
  rename(country = recipient_country_title) %>%
  unique()

dhsc_cofunding_join_abs <- dhsc_cofunding_join %>%
  mutate(recipient_country_abstract = str_extract_all(str_to_lower(abstract), countries_string)) %>% 
  unnest(cols = recipient_country_abstract, keep_empty = TRUE) %>% 
  mutate(recipient_country_abstract = str_to_title(recipient_country_abstract)) %>%
  group_by(id) %>%
  distinct(recipient_country_abstract) %>%
  #summarise(recipient_country_title = paste(coalesce(recipient_country_title, ""), collapse = ", ")) %>%
  rename(country = recipient_country_abstract) %>%
  unique()

dhsc_cofunding_join_country_search <- rbind(dhsc_cofunding_join_tit, dhsc_cofunding_join_abs)
dhsc_cofunding_join_country_search[dhsc_cofunding_join_country_search == "Unknown"] <- NA

dhsc_cofunding_join_country_search <- dhsc_cofunding_join_country_search %>%
  group_by(id) %>%
  distinct(country)

dhsc_cofunding_join_country_search <- na.omit(dhsc_cofunding_join_country_search)

dhsc_cofunding_join_country_search <- dhsc_cofunding_join_country_search %>%
  summarise(country = paste(country, collapse = ", "))

dhsc_cofunding_join <- dhsc_cofunding_join %>%
  left_join(dhsc_cofunding_join_country_search, by = "id")

dhsc_cofunding_join$recipient_country <- coalesce(dhsc_cofunding_join$recipient_country, dhsc_cofunding_join$country)
dhsc_cofunding_join <- dhsc_cofunding_join %>% select(!country)

dhsc_cofunding_with_countries <- dhsc_cofunding_join

dhsc_cofunding_with_countries <- dhsc_cofunding_with_countries %>%
  mutate(start_date = case_when(
    grepl("^\\d{5}$", start_date) ~ as_date(as.numeric(start_date), origin = "1899-12-30"),
    TRUE ~ ymd(start_date)
  )) %>%
  mutate(end_date = case_when(
    grepl("^\\d{5}$", end_date) ~ as_date(as.numeric(end_date), origin = "1899-12-30"),
    TRUE ~ ymd(end_date)
  ))


dhsc_cofunding_with_countries$status <- ifelse(dhsc_cofunding_with_countries$end_date > quarter_end_date, "Active", dhsc_cofunding_with_countries$status)
dhsc_cofunding_with_countries$status <- ifelse(dhsc_cofunding_with_countries$end_date <= quarter_end_date, "Closed", dhsc_cofunding_with_countries$status)
dhsc_cofunding_with_countries$status <- ifelse(is.na(dhsc_cofunding_with_countries$end_date), "Unknown", dhsc_cofunding_with_countries$status)

# saveRDS(dhsc_cofunding_with_countries, file = "Outputs/dhsc_cofunding_with_countries_Jan24update.rds")
dhsc_cofunding_with_countries <- readRDS("Outputs/dhsc_cofunding_with_countries_Jan24update.rds")

# DHSC Partnerships

dhsc_partnerships_final <- DHSC_partnerships %>% 
  mutate(id = identifier,
         abstract = abstract_description,
         amount = ODAvalue_amount,
         Funder = "Department of Health and Social Care",
         Fund = "DHSC - Global Health Research - Partnerships",
         recipient_country = beneficiary_country,
         iati_id = NA_character_,
         subject = NA_character_,
         currency = "GBP",
         period_start = NA_character_,
         period_end = NA_character_,
         partner_org_name = other_partner_org_name,
         partner_org_country = other_partner_org_country,
         extending_org = subject,
         last_updated = NA_character_) %>% 
  select(id, 
         title,
         abstract,
         start_date, 
         end_date,
         amount,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name, 
         lead_org_country, 
         partner_org_name, partner_org_country,
         iati_id, 
         Fund,
         Funder,
         recipient_country,
         subject,
         status,
         last_updated,
         link)

dhsc_partnerships_final$partner_org_name <- ifelse(dhsc_partnerships_final$partner_org_name == "Not applicable", NA, dhsc_partnerships_final$partner_org_name)
dhsc_partnerships_final$partner_org_country <- ifelse(dhsc_partnerships_final$partner_org_country == "Not applicable", NA, dhsc_partnerships_final$partner_org_country)
dhsc_partnerships_final$partner_org_name <- ifelse(dhsc_partnerships_final$partner_org_name == "Data not available", NA, dhsc_partnerships_final$partner_org_name)
dhsc_partnerships_final$partner_org_country <- ifelse(dhsc_partnerships_final$partner_org_country == "Data not available", NA, dhsc_partnerships_final$partner_org_country)
dhsc_partnerships_final$id <- ifelse(dhsc_partnerships_final$id == "Data not available", NA, dhsc_partnerships_final$id)
dhsc_partnerships_final$lead_org_name <- ifelse(dhsc_partnerships_final$lead_org_name == "Data not available", NA, dhsc_partnerships_final$lead_org_name)
dhsc_partnerships_final$title <- ifelse(dhsc_partnerships_final$title == "Data not available", NA, dhsc_partnerships_final$title)
dhsc_partnerships_final$abstract <- ifelse(dhsc_partnerships_final$abstract == "Data not available", NA, dhsc_partnerships_final$abstract)
dhsc_partnerships_final$title <- ifelse(is.na(dhsc_partnerships_final$title), dhsc_partnerships_final$abstract, dhsc_partnerships_final$title)
dhsc_partnerships_final$abstract <- ifelse(is.na(dhsc_partnerships_final$abstract), dhsc_partnerships_final$title, dhsc_partnerships_final$abstract)

dhsc_partnerships_final_noid <- dhsc_partnerships_final %>% filter(is.na(dhsc_partnerships_final$id))
dhsc_partnerships_final_id <- dhsc_partnerships_final %>% filter(!is.na(dhsc_partnerships_final$id))

dhsc_partnerships_final_noid$id <- 1:nrow(dhsc_partnerships_final_noid)
dhsc_partnerships_final_noid <- dhsc_partnerships_final_noid %>%
  mutate(id = paste0("dhsc_modari_", id))
dhsc_partnerships_final_noid$extending_org <- "DHSC"

dhsc_partnerships_final_id$extending_org <- ifelse(str_starts(dhsc_partnerships_final_id$id, "MR|MC|G"), "MRC", dhsc_partnerships_final_id$extending_org)
dhsc_partnerships_final_id$extending_org <- ifelse(str_detect(dhsc_partnerships_final_id$id, "^EP"), "EPSRC", dhsc_partnerships_final_id$extending_org)
dhsc_partnerships_final_id$extending_org <- ifelse(is.na(dhsc_partnerships_final_id$extending_org), "DHSC", dhsc_partnerships_final_id$extending_org)

dhsc_partnerships_final <- rbind(dhsc_partnerships_final_id, dhsc_partnerships_final_noid)

dhsc_partnerships_final <- dhsc_partnerships_final %>%
  mutate(start_date = case_when(
    grepl("^\\d{5}$", start_date) ~ as_date(as.numeric(start_date), origin = "1899-12-30"),
    TRUE ~ ymd(start_date)
  )) %>%
  mutate(end_date = case_when(
    grepl("^\\d{5}$", end_date) ~ as_date(as.numeric(end_date), origin = "1899-12-30"),
    TRUE ~ ymd(end_date)
  ))

dhsc_partnerships_final$status <- ifelse(dhsc_partnerships_final$status == "NA", "Unknown", dhsc_partnerships_final$status)
dhsc_partnerships_final$status <- ifelse(dhsc_partnerships_final$status == "Completed", "Closed", dhsc_partnerships_final$status)

# saveRDS(dhsc_partnerships_final, file = "Outputs/dhsc_partnerships_final_Jan24update.rds")
dhsc_partnerships_final <- readRDS("Outputs/dhsc_partnerships_final_Jan24update.rds")

# dhsc_cofunding_with_countries$status <- ifelse(dhsc_partnerships_final$status == "NA", "Unknown", dhsc_partnerships_final$status)
# dhsc_partnerships_final$status <- ifelse(dhsc_partnerships_final$status == "Completed", "Closed", dhsc_partnerships_final$status)

# 4) Extract Wellcome projects ------------------------------------------------

# Add missing fields and format Funder/Fund field
wellcome_grants_formatted <- wellcome_grants %>% 
  mutate(status = if_else(Sys.Date() <= `Planned Dates: End Date`, "Active", "Closed"),
         extending_org = "Wellcome Trust",
         currency = "GBP",
         partner_org_name = `Other Implementing Organisations`,
         partner_org_country = `Research Location Countries`,
         recipient_country = NA_character_,
         period_start = NA_character_,
         period_end = NA_character_,
         iati_id = NA_character_,
         Funder = if_else(str_detect(`CoFunders`, "National Institute for Health Research"), 
                          "Department of Health and Social Care", `CoFunders`),
         Fund = if_else(Funder == "Department of Health and Social Care",
                        "DHSC - Global Health Research - Partnerships", "FCDO Research - Programmes"),
         last_updated = quarter_end_date) %>% 
  filter(`ODA Funding` > 0)

# Select desired variables
wellcome_grants_formatted <- wellcome_grants_formatted %>% 
  select(id = `InternalID`,
         title = Title, 
         abstract = Description,
         start_date = `Planned Dates: Start Date`,
         end_date = `Planned Dates: End Date`,
         amount = `ODA Funding`,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name = `Recipient Org: Name`,
         lead_org_country = `Recipient Org: Country`,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund,
         Funder, 
         recipient_country,
         subject = `PartnershipName`,
         status,
         last_updated
  ) 

# Format date fields for merging
wellcome_grants_final <- wellcome_grants_formatted %>% 
  mutate(start_date = as.character(start_date),
         end_date = as.character(end_date),
         link = "https://wellcome.org/grant-funding/funded-people-and-projects")

# below is me playing with the overlap between Wellcome return and Francesca's return for DHSC partnerships
wellcome_dhsc_grants_final <- wellcome_grants_final %>% filter(Funder == "Department of Health and Social Care",)

wellcome_dhsc_overlap <- setdiff(wellcome_dhsc_grants_final$id, dhsc_partnerships_final$id)

wellcome_dhsc_grants_final <- wellcome_dhsc_grants_final %>% filter(!id %in% c(wellcome_dhsc_overlap))
dhsc_wellcome_partnerships_final <- dhsc_partnerships_final %>% filter(id %in% c(wellcome_dhsc_grants_final$id))

dhsc_partnerships_final <- dhsc_partnerships_final %>% filter(!id %in% c(wellcome_dhsc_grants_final$id))


# Write lead org names and countries to file
org_names_and_locations_3 <- org_names_and_locations_3 %>% 
  rbind(select(wellcome_grants_final,
               project_id = id,
               organisation_name = lead_org_name,
               organisation_country = lead_org_country) %>% 
          mutate(organisation_role = 1))

# Write partner org names and countries to file (where simple to do)
wellcome_partners <- wellcome_grants_final %>% 
  select(id, partner_org_name, partner_org_country) %>% 
  # Exclude missings, multiple and miscellaneous partners
  filter(!is.na(partner_org_name),
         !str_detect(partner_org_name, "Misc")) %>% 
  # Separate rows with multiple partners
  separate_rows(partner_org_name, sep = ",", convert = FALSE) %>% 
  mutate(partner_org_name = str_trim(partner_org_name)) %>% 
  mutate(new_country = map(partner_org_name, org_country_lookup)) %>% 
  unnest(cols = new_country) %>% 
  mutate(partner_org_country = coalesce(new_country, partner_org_country)) %>% 
  filter(!str_detect(partner_org_country, ",")) %>% 
  select(-new_country)

org_names_and_locations_3 <- org_names_and_locations_3 %>% 
  rbind(select(wellcome_partners,
               project_id = id,
               organisation_name = partner_org_name,
               organisation_country = partner_org_country) %>% 
          mutate(organisation_role = 2))

saveRDS(org_names_and_locations_3, file = "Outputs/org_names_and_locations_3_Jan24update.rds")


# Clear environment
rm(wellcome_grants, wellcome_grants_formatted, wellcome_partners)



# 5) DHSC Global Health Security data (spreadsheet) -----
# (Covers non-UKRI GAMRIF and UK Vaccine Network projects)

# This has not yet been sent to us

# Reformat to match other dataset
dhsc_ghs_projects_final <- dhsc_ghs_projects %>%
  rename(id = `Extending organisation - award ID`,
         title = `Award title`,
         abstract = `Award description`,
         start_date = `Start date`,
         end_date = `End date`,
         amount = `Award amount (£)`,
         recipient_country = `Beneficiary country`,
         extending_org = `Extending organisation - name`,
         lead_org_name = `Lead organisation - name`,
         lead_org_country = `Lead organisation - country`,
         partner_org_name = `Implementing partner(s) - name`,
         partner_org_country = `Implementing partner(s) - country`,
         iati_id = `Funder programme - IATI ID`,
         link = `Data source`
         ) %>%
  mutate(start_date = as.character(start_date),
         end_date = as.character(end_date),
         currency = coalesce(Currency, "GBP"),
         period_start = NA_character_,
         period_end = NA_character_,
         subject = NA_character_,
         status = coalesce(if_else(end_date >= Sys.Date(), "Active", "Closed"), "Unknown"),
         last_updated = quarter_end_date
         ) %>%
  select(-`No.`, -Currency, -`Aims/Objectives`, -`Investigator(s) - name`)

dhsc_ghs_projects_final <- dhsc_ghs_projects_final[dhsc_ghs_projects_final$Fund=="DHSC - Global Health Security - GAMRIF",]

dhsc_ukvn_projects_final <- dhsc_ukvn_projects %>%
  rename(id = `Extending organisation - award ID`,
         title = `Award title`,
         abstract = `Award description`,
         start_date = `Start date`,
         end_date = `End date`,
         amount = `Award amount (£)`,
         recipient_country = `Beneficiary country`,
         extending_org = `Extending organisation - name`,
         lead_org_name = `Lead organisation - name`,
         lead_org_country = `Lead organisation - country`,
         partner_org_name = `Implementing partner(s) - name`,
         partner_org_country = `Implementing partner(s) - country`,
         iati_id = `Funder programme - IATI ID`,
         GtR_ref = `GtR reference`
  ) %>%
  mutate(start_date = as.character(start_date),
         end_date = as.character(end_date),
         currency = coalesce(Currency, "GBP"),
         period_start = NA_character_,
         period_end = NA_character_,
         subject = NA_character_,
         status = coalesce(if_else(end_date >= Sys.Date(), "Active", "Closed"), "Unknown"),
         last_updated = quarter_end_date
  ) %>%
  select(-`Aims/Objectives`, -`Investigator(s) - name`,-Currency)

# Add GtR link to projects
dhsc_ukvn_projects_final <- dhsc_ukvn_projects_final %>% 
  mutate(link = if_else((str_detect(GtR_ref, "Yes")),
                        paste0("https://gtr.ukri.org/projects?ref=", id), NA))

dhsc_ukvn_projects_final <- dhsc_ukvn_projects_final %>% select(-GtR_ref)
dhsc_ukvn_projects_final$abstract <- ifelse(is.na(dhsc_ukvn_projects_final$abstract), dhsc_ukvn_projects_final$title, dhsc_ukvn_projects_final$abstract)

dhsc_ghs_projects_final <- rbind(dhsc_ghs_projects_final, dhsc_ukvn_projects_final)

# Save as R file (to read back in if needed)
# saveRDS(dhsc_ghs_projects_final, file = "Outputs/dhsc_ghs_projects_final_Jan24update.rds")
dhsc_ghs_projects_final <- readRDS("Outputs/dhsc_ghs_projects_final_Jan24update.rds")

# Write lead org names and countries to file
org_names_and_locations_3 <- org_names_and_locations_3 %>%
  rbind(select(dhsc_ghs_projects_final,
               project_id = id,
               organisation_name = lead_org_name,
               organisation_country = lead_org_country) %>%
          mutate(organisation_role = 1))

# Write partner org names and countries to file (where simple to do)
dhsc_partners <- dhsc_ghs_projects_final %>%
  select(id, partner_org_name, partner_org_country) %>%
  # Exclude missings, multiple and miscellaneous partners
  filter(!is.na(partner_org_name),
         !str_detect(partner_org_name, ",|;"),
         !str_detect(partner_org_country, ",|;|N/A"))

org_names_and_locations_3 <- org_names_and_locations_3 %>%
  rbind(select(dhsc_partners,
               project_id = id,
               organisation_name = partner_org_name,
               organisation_country = partner_org_country) %>%
          mutate(organisation_role = 2))

# Clear environment
rm(dhsc_ghs_projects, dhsc_partners)


# 6) BEIS RODA GCRF/Newton non-UKRI data (by spreadsheet) -----

# Reformat to match other datasetS
roda_extract_gcrf_final <- roda_extract_gcrf %>% 
  rename(id = `RODA ID`,
         abstract = Description,
         title = Title,
         amount = Amount,
         recipient_country = `Benefiting countries`,
         extending_org = `Delivery partner`,
         lead_org_name = `Lead Organisation`
  ) %>% 
  mutate(Fund = "DSIT - Global Challenges Research Fund (GCRF)",
         Funder = "Department for Science, Innovation and Technology",
         #start_date = as.character(as.Date(coalesce(`Actual start date`, `Planned start date`), "%d %B %Y")),
         #end_date = as.character(as.Date(coalesce(`Actual end date`, `Planned end date`), "%d %B %Y")),
         start_date = coalesce(`Actual start date`, `Planned start date`),
         end_date = coalesce(`Actual end date`, `Planned end date`),
         lead_org_country = map(lead_org_name, org_country_lookup),
         partner_org_name = NA_character_,
         partner_org_country = NA_character_,
         iati_id = NA_character_,
         currency = "GBP",
         status = if_else(`Activity Status` %in% c("Spend in progress", "Agreement in place", "Delivery", "Finalisation"), "Active",
                          if_else(`Activity Status` %in% c("Completed"), "Closed", 
                                  if_else(`Activity Status` %in% c("Cancelled"), "Cancelled", "Unknown"))),
         period_start = NA_character_,
         period_end = NA_character_,
         subject = NA_character_,
         last_updated = quarter_end_date,
         link = NA_character_
  ) %>% 
  unnest(cols = lead_org_country) %>% 
    # suppress display of active project end dates that have passed
  #mutate(end_date = if_else(status == "Active" & Sys.Date() <= end_date, end_date, NA_character_)) %>%
    # remove unecessary variables
  select(-Level, -`Benefiting region`, -`Planned start date`, -`Actual start date`,  -`Planned end date`,
         -`Actual end date`, -`Activity Status`)

roda_extract_gcrf_final_test <- roda_extract_gcrf_final %>% select(id, lead_org_name, lead_org_country)
names(modari_org_lookup) <- c("lead_org_name","lead_org_country")

roda_extract_gcrf_final_test_filled <- stringdist_join(roda_extract_gcrf_final_test, modari_org_lookup, by='lead_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist') %>% 
  #group_by(partner_org_name) %>% 
  slice_min(order_by=dist, n=1)

roda_extract_gcrf_final <- roda_extract_gcrf_final %>% 
  left_join(roda_extract_gcrf_final_test_filled, by = "id") %>% 
  mutate(lead_org_country = coalesce(lead_org_country, lead_org_country.y)) %>%
  select(!c(lead_org_name.x, lead_org_country.x, lead_org_name.y,lead_org_country.y,dist))


roda_extract_newton$`Planned start date` <- as.character(roda_extract_newton$`Planned start date`)

roda_extract_newton_final <- roda_extract_newton %>% 
  rename(id = `RODA ID`,
         title = Title,
         abstract = Description,
         amount = Amount,
         recipient_country = `Benefiting region`,
         extending_org = `Delivery partner`,
         lead_org_name = `Lead Organisation`,
         partner_org_name = `In country partner`) %>% 
  mutate(Fund = "DSIT - Newton Fund",
         Funder = "Department for Science, Innovation and Technology",
         lead_org_country = map(lead_org_name, org_country_lookup),
         partner_org_country = NA_character_,
         iati_id = NA_character_,
         link = NA_character_,
         start_date = coalesce(`Actual start date`, `Planned start date`),
         end_date = coalesce(`Actual end date`, `Planned end date`),
         currency = "GBP",
         status = if_else(`Activity Status` %in% c("Spend in progress", "Agreement in place", "Delivery", "Finalisation"), "Active",
                          if_else(`Activity Status` %in% c("Completed"), "Closed", 
                                  if_else(`Activity Status` %in% c("Cancelled"), "Cancelled", "Unknown"))),
         period_start = NA_character_,
         period_end = NA_character_,
         subject = NA_character_,
         last_updated = quarter_end_date) %>% 
  unnest(cols = lead_org_country) %>%
  # suppress display of end dates that have passed
  #mutate(end_date = if_else(Sys.Date() <= end_date, end_date, NA_character_)) %>%
  select(-Level, -`Benefiting countries`, -`Intended beneficiaries`, -`Planned start date`, -`Activity Status`,
         -`Planned end date`, -`Actual start date`, -`Actual end date`)

# Write lead org names and countries to file
org_names_and_locations_3 <- org_names_and_locations_3 %>% 
  rbind(select(roda_extract_gcrf_final,
               project_id = id,
               organisation_name = lead_org_name,
               organisation_country = lead_org_country) %>% 
          mutate(organisation_role = 1)) %>% 
  
  rbind(select(roda_extract_newton_final,
               project_id = id,
               organisation_name = lead_org_name,
               organisation_country = lead_org_country) %>% 
          mutate(organisation_role = 1))


roda_extract_newton_final_test <- roda_extract_newton_final %>% select(id, partner_org_name, partner_org_country)
names(modari_org_lookup) <- c("partner_org_name","partner_org_country")

roda_extract_newton_final_test_filled <- stringdist_join(roda_extract_newton_final_test, modari_org_lookup, by='partner_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist') %>% 
  #group_by(partner_org_name) %>% 
  slice_min(order_by=dist, n=1)

roda_extract_newton_final <- roda_extract_newton_final %>% 
  left_join(roda_extract_newton_final_test_filled, by = "id") %>% 
  mutate(partner_org_country = coalesce(partner_org_country, partner_org_country.y)) %>%
  select(!c(partner_org_name.x, partner_org_country.x, partner_org_name.y,partner_org_country.y,dist))


# saveRDS(org_names_and_locations_3, file = "Outputs/org_names_and_locations_3_Jan24update.rds")
org_names_and_locations_3 <- readRDS("Outputs/org_names_and_locations_3_Jan24update.rds")

# Clear environment
rm(roda_extract_gcrf, roda_extract_newton)


# DEFRA

defra_grants_formatted <- defra_grants %>% 
  mutate(status = if_else(Sys.Date() <= `End date`, "Active", "Closed"),
         extending_org = "DEFRA",
         currency = "GBP",
         partner_org_name = `Partners cited on the application`,
         partner_org_country = `Research location Countries`,
         recipient_country = NA_character_,
         period_start = NA_character_,
         period_end = NA_character_,
         iati_id = NA_character_,
         subject = NA_character_,
         Funder = "Department for Environment, Food, and Rural Affairs",
         Fund = paste0("DEFRA - ",`Grant Programme`),
         last_updated = quarter_end_date)

# Select desired variables
defra_grants_formatted <- defra_grants_formatted %>% 
  select(id = `Internal Grant ID`,
         title = `Grant Title`, 
         abstract = `Grant Description`,
         start_date = `Start date`,
         end_date = `End date`,
         amount = `Amount awarded`,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name = `Recipient Org Name`,
         lead_org_country = `Recipient Org Country`,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund,
         Funder, 
         recipient_country,
         status,
         subject,
         last_updated
  ) 

# Format date fields for merging
defra_grants_final <- defra_grants_formatted %>% 
  mutate(start_date = as.character(start_date),
         end_date = as.character(end_date),
         link = ifelse(defra_grants_formatted$Fund=="DEFRA - Darwin Initiative","https://www.darwininitiative.org.uk/",ifelse(
           defra_grants_formatted$Fund=="DEFRA - Darwin Plus","https://darwinplus.org.uk/project-search/",ifelse(
             defra_grants_formatted$Fund=="DEFRA - Illegal Wildlife Trade Challenge Fund","https://iwt.challengefund.org.uk/",""
           )
         )))

defra_grants_final$status <- ifelse(is.na(defra_grants_final$status), "Unknown", defra_grants_final$status)

# Getting FCDO Concordat grants in the data

concordat_grants 

concordat_projects_ids_full <- concordat_grants$`Award Reference`

### B - Extract project info from GtR API ###

# Create empty dataset to hold projects
concordat_projects_by_id <- data.frame()
org_names_and_locations_2.0 <- data.frame()

# Run project info extraction over all GtR projects
# (takes 1-2 hours to run)

n <- 1 # set counter

for (id in concordat_grants$`Award Reference`) {
  
  print(paste0(n, " - ", id))
  
  data <- extract_ukri_projects_by_id(id)
  
  # Separate elements of list
  project_data <- data[[1]]
  org_data <- data[[2]]
  
  # Add new data rows to existing tables
  concordat_projects_by_id <- concordat_projects_by_id %>% 
    rbind(project_data)
  
  org_names_and_locations_2.0 <- org_names_and_locations_2.0 %>% 
    rbind(org_data)
  
  # Increment counter for next cycle
  n <- n+1
  
}

# saveRDS(concordat_projects_by_id, file = "Outputs/concordat_projects_by_id_Jan24update.rds")
saveRDS(org_names_and_locations_2.0, file = "Outputs/org_names_and_locations_2.0_Jan24update.rds")

concordat_not_on_GtR <- setdiff(concordat_grants$`Award Reference`, concordat_projects_by_id$gtr_id)

concordat_not_on_GtR <- concordat_grants[concordat_grants$`Award Reference` %in% c(concordat_not_on_GtR),]
concordat_not_on_GtR <- concordat_not_on_GtR[concordat_not_on_GtR$`Funding Value` > 0,]
concordat_not_on_GtR <- concordat_not_on_GtR %>%
  rename(gtr_id = `Award Reference`, extending_org = Funder, title = `Award Title`, fund.start = `Start Date`, fund.end = `End Date`, amount = `Funding Value`)
concordat_not_on_GtR$fund.start <- as.character(concordat_not_on_GtR$fund.start)
concordat_not_on_GtR$fund.end <- as.character(concordat_not_on_GtR$fund.end)

concordat_projects_by_id <- concordat_projects_by_id %>% bind_rows(concordat_not_on_GtR)



concordat_projects_by_id_tit <- concordat_projects_by_id %>%
  mutate(recipient_country_title = str_extract_all(str_to_lower(title), countries_string)) %>% 
  unnest(cols = recipient_country_title, keep_empty = TRUE) %>% 
  mutate(recipient_country_title = str_to_title(recipient_country_title)) %>%
  group_by(gtr_id) %>%
  distinct(recipient_country_title) %>%
  #summarise(recipient_country_title = paste(coalesce(recipient_country_title, ""), collapse = ", ")) %>%
  rename(country = recipient_country_title) %>%
  unique()

concordat_projects_by_id_abs <- concordat_projects_by_id %>%
  mutate(recipient_country_abs = str_extract_all(str_to_lower(abstract), countries_string)) %>%
  unnest(cols = recipient_country_abs, keep_empty = TRUE) %>% 
  mutate(recipient_country_abs = str_to_title(recipient_country_abs)) %>%
  group_by(gtr_id) %>%
  distinct(recipient_country_abs) %>%
  #summarise(recipient_country_abs = paste(coalesce(recipient_country_abs, ""), collapse = ", ")) %>%
  rename(country = recipient_country_abs) %>%
  unique()

concordat_projects_by_id_countries <- rbind(concordat_projects_by_id_tit, concordat_projects_by_id_abs)
concordat_projects_by_id_countries[concordat_projects_by_id_countries == "Unknown"] <- NA

concordat_projects_by_id_countries <- concordat_projects_by_id_countries %>%
  group_by(gtr_id) %>%
  distinct(country)

concordat_projects_by_id_countries <- na.omit(concordat_projects_by_id_countries)

concordat_projects_by_id_countries <- concordat_projects_by_id_countries %>%
  summarise(country = paste(country, collapse = ", "))

concordat_projects_by_id <- concordat_projects_by_id %>%
  left_join(concordat_projects_by_id_countries, by = "gtr_id") %>%
  rename(recipient_country = country)

# saveRDS(concordat_projects_by_id, file = "Outputs/concordat_projects_by_id_Jan24update.rds")
concordat_projects_by_id <- readRDS("Outputs/concordat_projects_by_id_Jan24update.rds")

concordat_projects_formatted <- concordat_projects_by_id %>% 
  mutate(currency = "GBP",
         period_start = NA_character_,
         period_end = NA_character_,
         iati_id = "GB-GOV-1-300347",
         subject = NA_character_,
         Funder = "Foreign, Commonwealth and Development Office",
         Fund = "FCDO Research - Partnerships",
         extending_org = "Medical Research Council (MRC)",
         last_updated = quarter_end_date)

# Select desired variables
concordat_projects_formatted <- concordat_projects_formatted %>% 
  select(id = gtr_id,
         title, 
         abstract,
         start_date = fund.start,
         end_date = fund.end,
         amount,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name,
         lead_org_country,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund,
         Funder, 
         recipient_country,
         status,
         subject,
         last_updated
  ) 

# Add GtR link to projects
concordat_projects_final <- concordat_projects_formatted %>% 
  mutate(link = paste0("https://gtr.ukri.org/projects?ref=", id))

concordat_projects_formatted$link <- ""
concordat_projects_formatted$link <- ifelse(!is.na(concordat_projects_formatted$recipient_country) & nchar(concordat_projects_formatted$id) > 8, paste0("https://gtr.ukri.org/projects?ref=", concordat_projects_formatted$id), NA)

concordat_projects_final <- concordat_projects_formatted
concordat_projects_final$status <- ifelse(is.na(concordat_projects_final$status) & concordat_projects_final$end_date < quarter_end_date, "Closed", concordat_projects_final$status)

unique(concordat_projects_final$status)


# 7) Join funder datasets together ----------------------------------------------
nihr_projects_final$start_date <- ymd(nihr_projects_final$start_date)
ukri_projects_with_countries$start_date <- ymd(ukri_projects_with_countries$start_date)
ooda_iati_final$start_date <- ymd(ooda_iati_final$start_date)
fcdo_partnerships_ukri_final$start_date <- ymd(fcdo_partnerships_ukri_final$start_date)
concordat_projects_final$start_date <- ymd(concordat_projects_final$start_date)
dhsc_partnerships_final$start_date <- ymd(dhsc_partnerships_final$start_date)
dhsc_cofunding_with_countries$start_date <- ymd(dhsc_cofunding_with_countries$start_date)
iati_projects_final$start_date <- ymd(iati_projects_final$start_date)
wellcome_grants_final$start_date <- ymd(wellcome_grants_final$start_date)
defra_grants_final$start_date <- ymd(defra_grants_final$start_date)
dhsc_ghs_projects_final$start_date <- ymd(dhsc_ghs_projects_final$start_date)
roda_extract_gcrf_final$start_date <- ymd(roda_extract_gcrf_final$start_date)
roda_extract_newton_final$start_date <- ymd(roda_extract_newton_final$start_date)

nihr_projects_final$end_date <- ymd(nihr_projects_final$end_date)
ukri_projects_with_countries$end_date <- ymd(ukri_projects_with_countries$end_date)
ooda_iati_final$end_date <- ymd(ooda_iati_final$end_date)
fcdo_partnerships_ukri_final$end_date <- ymd(fcdo_partnerships_ukri_final$end_date)
concordat_projects_final$end_date <- ymd(concordat_projects_final$end_date)
dhsc_partnerships_final$end_date <- ymd(dhsc_partnerships_final$end_date)
dhsc_cofunding_with_countries$end_date <- ymd(dhsc_cofunding_with_countries$end_date)
iati_projects_final$end_date <- ymd(iati_projects_final$end_date)
wellcome_grants_final$end_date <- ymd(wellcome_grants_final$end_date)
defra_grants_final$end_date <- ymd(defra_grants_final$end_date)
dhsc_ghs_projects_final$end_date <- ymd(dhsc_ghs_projects_final$end_date)
roda_extract_gcrf_final$end_date <- ymd(roda_extract_gcrf_final$end_date)
roda_extract_newton_final$end_date <- ymd(roda_extract_newton_final$end_date)

all_projects <- rbind(nihr_projects_final, 
                      ukri_projects_with_countries,
                      ooda_iati_final,
                      fcdo_partnerships_ukri_final,
                      concordat_projects_final,
                      dhsc_partnerships_final,
                      dhsc_cofunding_with_countries,
                      iati_projects_final, 
                      wellcome_grants_final,
                      defra_grants_final,
                      dhsc_ghs_projects_final,
                      roda_extract_gcrf_final, roda_extract_newton_final) %>% 
  unique() %>% 
  ungroup()

#all_projects[all_projects$id=="GB-1-202568-104",]

# all_projects <- all_projects %>%
#   mutate(start_date = case_when(
#     grepl("^\\d{5}$", start_date) ~ as_date(as.numeric(start_date), origin = "1899-12-30"),
#     TRUE ~ ymd(start_date)
#   )) %>%
#   mutate(end_date = case_when(
#     grepl("^\\d{5}$", end_date) ~ as_date(as.numeric(end_date), origin = "1899-12-30"),
#     TRUE ~ ymd(end_date)
#   ))

names(all_projects)
unique(all_projects$lead_org_name)

# 8) Manual exclusions and formatting -------------------------------------------

# Manually edit country info for Chevening Scholarships
all_projects_tidied <- all_projects %>% 
  mutate(lead_org_country = if_else(Fund == "FCDO - Chevening Scholarships", "United Kingdom", lead_org_country),
         start_date = if_else(Fund == "FCDO - Chevening Scholarships", NA, start_date))

# Name BEIS delivery partners fully
all_projects_tidied <- all_projects_tidied %>% 
  mutate(extending_org = case_when(
              extending_org == "AMS" ~ "Academy of Medical Sciences", 
              extending_org == "BA" ~ "British Academy",
              extending_org %in% c("BC", "BRITISH COUNCIL") ~ "British Council",
              extending_org == "MO" ~ "Met Office",
              extending_org == "RAE" ~ "Royal Academy of Engineering",
              extending_org == "RS" ~ "Royal Society",
              extending_org == "UKSA" ~ "UK Space Agency",
              TRUE ~ extending_org
              ))

# Remove non-research partners
# (linked partner data from non-RED managed programmes)
all_projects_tidied <- all_projects_tidied %>% 
  filter(!(extending_org %in% c("Sightsavers",
                                "Coffey International Development Limited, a Tetra Tech Company",
                                "SIGHTSAVERS")))

all_projects_tidied <- all_projects_tidied %>%
  filter(!(Funder %in% c("The Institute of Development Studies")))

# Correct missing IDS name (ARPA activity)
all_projects_tidied <- all_projects_tidied %>% 
  mutate(extending_org = if_else(extending_org == "GB-COH-877338", 
                                 "Institute of Development Studies", extending_org),
         lead_org_name = if_else(lead_org_name == "GB-COH-877338", 
                                 "Institute of Development Studies", lead_org_name))

# Add FCDO DevTracker links in absence of other public source
all_projects_tidied <- all_projects_tidied %>% 
  mutate(link = if_else((str_detect(iati_id, "GB-GOV-1-") | str_detect(iati_id, "GB-1-")) & is.na(link),
                        paste0("https://devtracker.fcdo.gov.uk/projects/", iati_id, "/summary"), link))

all_projects_tidied$amount <- as.numeric(all_projects_tidied$amount)
all_projects_tidied$Funder <- ifelse(all_projects_tidied$Funder=="DEPARTMENT FOR BUSINESS, ENERGY & INDUSTRIAL STRATEGY", "Department for Science, Innovation and Technology", all_projects_tidied$Funder)


# all_projects_tidied$status <- ifelse(all_projects_tidied$end_date > quarter_end_date, "Active", all_projects_tidied$status)
# all_projects_tidied$status <- ifelse(all_projects_tidied$end_date <= quarter_end_date, "Closed", all_projects_tidied$status)
# all_projects_tidied$status <- ifelse(is.na(all_projects_tidied$end_date), "Unknown", all_projects_tidied$status)

all_projects_tidied[all_projects_tidied$id=="GB-1-202568-104",]
# Adding on the cofunded DHSC projects
# DHSC_cofund <- DHSC_cofund %>% rename(id = `DHSC - MRC co-funded awards`)
# all_projects_tidied <- all_projects_tidied %>% mutate(`Co-Funder` = "", .after=Funder)
# all_projects_tidied$`Co-Funder` <- ifelse(all_projects_tidied$id %in% c(DHSC_cofund$id), "DHSC", all_projects_tidied$`Co-Funder`)

# Some data cleaning - partner org name

all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="UK - Medical Research Council, Correction","UK - Medical Research Council", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Correction","", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Pegasys Limited, Correction","Pegasys Limited", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Supplier Name Excluded","", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="ACTIONAID MYANMAR, Correction","ACTIONAID MYANMAR", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name %in% c("Oxford University, Correction", "University of Oxford, Supplier Name Excluded","Oxford University","Oxford University, Customer, Correction"),"University of Oxford", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Supplier Name Excluded, Correction","", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name %in% c("The GSMA Foundation, Correction","The GSMA Foundation, Journal Transaction"),"The GSMA Foundation", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name %in% c("Abt Associates Inc, Correction","Abt Associates Inc, Supplier Name Excluded"),"Abt Associates Inc", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name %in% c("London School of Hygiene and Tropical Medicine, Customer, Correction","London School of Hygiene and Tropical Medicine, Journal Transaction"),"London School of Hygiene and Tropical Medicine", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Asian Development Bank, Supplier Name Excluded","Asian Development Bank", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Supplier Name Excluded, Bi Valve Shelfish Association of SA (Pty) Ltd","Bi Valve Shelfish Association of SA (Pty) Ltd", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name %in% c("Chemonics International Inc., Supplier Name Excluded","Chemonics International Inc., Supplier Name Excluded, Journal Transaction"),"Chemonics International Inc.", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="IORA Ecological Soluations Pvt Ltd","IORA Ecological Solutions Pvt Ltd", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Supplier Name Excluded, Journal Transaction, Yale University","Yale University", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="School of Oriental and African Studies (SOAS), Journal Transaction","School of Oriental and African Studies (SOAS)", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Global Disability Innovation Hub, Correction","Global Disability Innovation Hub", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="University of Manchester Research, Journal Transaction","University of Manchester Research", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="Cowatersogema International Inc, Journal Transaction","Cowatersogema International Inc", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="IMC Worldwide, Journal Transaction, Customer","IMC Worldwide", all_projects_tidied$partner_org_name)
all_projects_tidied$partner_org_name <- ifelse(all_projects_tidied$partner_org_name=="International Rescue Committee (UK), Supplier Name Excluded","International Rescue Committee (UK)", all_projects_tidied$partner_org_name)





# More data cleaning - partner org country

all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="University of Montpellier 3 Paul Valery", "France", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Institute of Labor Economics", "Germany", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="University of Manchester Research", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="University of Oxford", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Shell Foundation", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Crown Agents Bank", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Pegasys Limited", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Global Innovation Fund", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="UK - Medical Research Council", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Path", "United States", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="ACTIONAID MYANMAR", "Myanmar", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Bi Valve Shelfish Association of SA (Pty) Ltd", "South Africa", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Mott MacDonald Limited", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Dentons UK and Middle East LLP", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Chemonics International Inc.", "United States", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name %in% c("IORA Ecological Solutions Pvt Ltd","IORA"), "India", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="KBR", "United States", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="School of Oriental and African Studies (SOAS)", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Global Disability Innovation Hub", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Centre for Agriculture and Bioscience International", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Results for Development", "United States", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="J-PAL", "United States", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="The GSMA Foundation", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="IMC Worldwide", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="London School of Hygiene and Tropical Medicine", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Cowatersogema International Inc", "Canada", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Global Integrity", "United States", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Abt Associates Inc", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="International Rescue Committee (UK)", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="MannionDaniels", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="UNESCO IIEP", "France", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Malcolm H. Kerr Carnegie Middle East Center, Rift Valley Institute, The Asia Foundation", "Lebanon, Kenya, United States of America (the)", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Brink Innovation Ltd, IMC Worldwide LTD", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Johns Hopkins University, Insecurity Insight, University of Geneva, Bikash Shrot Kendra, Aga Khan Foundation, Central African Institute of Statistics, Economic and Social Studies (ICASEES)", "United States of America (the), Switzerland, Nepal, Central African Republic", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Busara Center for Behavioral Economics, 60 Decibels, Acumen Resilient Agriculture Fund", "Kenya, United States", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="PAK GIRL GUIDES, **********, International Labour Organization (ILO)", "Pakistan", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Centre for Economic Policy Research, The University of Oxford, The University of Groningen, African Center for Economic Transformation (ACET), The University of Notre Dame, The Yale Research Initiative on Innovation and Scale", "United Kingdom of Great Britain and Northern Ireland (the), United States of America (the), Ghana", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Crop2Cash", "Nigeria", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="J-Palm", "Liberia", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Green Agro - Lersha", "Ethiopia", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Benaa Foundation", "Egypt", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Bakhabar Kissan", "Pakistan", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Simusolar", "Tanzania", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Hello Tractor", "Nigeria", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Geokrishi", "Nepal", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Komunidad", "Philippines (the)", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="CoAmana", "Nigeria", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Aquarech", "Kenya", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Diyalo", "Nepal", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Bhumijo", "Bangladesh", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Swacch Sustainable Solutions", "India", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="The Freetown Waste Transformers", "Sierra Leone", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Janajal - Supremus", "India", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Powerstove", "Nigeria", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="The University of Oxford, Centre for Ecnomic and Social Studies, Young Lives India, Pankhurt Deveopment Research and Consulting PLC, Policy Studies Institute, Grupo de Analisis para el Desarollo, Centre for Analysis and Forecasting, Vietnamese Academy of Social Sciences, General Statistics Office, Ministry of Planning and Investment, University of Lancaster", "United Kingdom of Great Britain and Northern Ireland (the), India, Vietnam, Peru", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="HERD international, American University of Beirut, Queen Margaret University, UK, Macfarlane Burnet Institute for Medical Research and Public Health Ltd, College of Medicine and Allied Health Sciences", "Nepal, Lebanon, United Kingdom, Australia, Sierra Leone", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Malawi Liverpool Wellcome Trust Programme, Malawi, Makerere University School of Public Health, Liverpool School of Tropical Medicine, African Institute for Development Policy (AFIDEP), Kenya, Zankli Research Centre, London School of Hygiene & Tropical Medicine, Respiratory Society of Kenya", "Malawi, Uganda, Kenya, Nigeria, United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="International Water Management Institute (IWMI)", "Sri Lanka", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="International Rice Research Institute (IRRI)", "Philippines (the)", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="International Food Policy Research institute (IFPRI)", "United States", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Land O'Lakes Venture37, PricewaterhouseCoopers TZ", "United States, Tanzania", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="GALVMED", "United Kingdom", all_projects_tidied$partner_org_country)
all_projects_tidied$partner_org_country <- ifelse(all_projects_tidied$partner_org_name=="Connexus", "Australia", all_projects_tidied$partner_org_country)

# 9) Compare to previous data -------------------------------------------

prev_all_projects_tidied <- readRDS("Outputs/all_projects_tidied_kp_2607.rds")

not_carried_over <- setdiff(unique(prev_all_projects_tidied$id), unique(all_projects_tidied$id))

prev_all_projects_tidied_to_add <- prev_all_projects_tidied %>% filter(id %in% c(not_carried_over)) %>% select(-`Co-Funder`)
# 
# prev_all_projects_tidied_to_add_dsit <- prev_all_projects_tidied_to_add[prev_all_projects_tidied_to_add$Funder=="Department for Science, Innovation and Technology",]
# unique(prev_all_projects_tidied_to_add_dsit$id)
# 
prev_all_projects_tidied_to_add_partner_org <- prev_all_projects_tidied_to_add %>% select(id, partner_org_name, partner_org_country)

names(modari_org_lookup) <- c("partner_org_name","partner_org_country")

prev_all_projects_tidied_to_add_partner_org_filled <- stringdist_join(prev_all_projects_tidied_to_add_partner_org, modari_org_lookup, by='partner_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist',ignore_case = TRUE) %>%
  #group_by(partner_org_name) %>%
  slice_min(order_by=dist, n=1)
# 
# 
prev_all_projects_tidied_to_add <- prev_all_projects_tidied_to_add %>%
  left_join(prev_all_projects_tidied_to_add_partner_org_filled, by = "id") %>%
  mutate(partner_org_country = coalesce(partner_org_country, partner_org_country.y)) %>%
  select(!c(partner_org_name.x, partner_org_country.x, partner_org_name.y,partner_org_country.y,dist))

prev_all_projects_tidied_to_add <- prev_all_projects_tidied_to_add %>%
  mutate(start_date = str_sub(start_date, 1, 10)) %>%
  mutate(end_date = str_sub(end_date, 1, 10)) %>%
  mutate(period_start = str_sub(period_start, 1, 10)) %>%
  mutate(period_end = str_sub(period_end, 1, 10))

# prev_all_projects_tidied_to_add$start_date <- ymd(prev_all_projects_tidied_to_add$start_date)
# prev_all_projects_tidied_to_add$end_date <- ymd(prev_all_projects_tidied_to_add$end_date)
# prev_all_projects_tidied_to_add$period_start <- ymd(prev_all_projects_tidied_to_add$period_start)
# prev_all_projects_tidied_to_add$period_end <- ymd(prev_all_projects_tidied_to_add$period_end)

prev_all_projects_tidied_to_add <- prev_all_projects_tidied_to_add %>% filter(Funder != "Department for Science, Innovation and Technology")

prev_all_projects_tidied_to_add$status <- ifelse(prev_all_projects_tidied_to_add$end_date <= quarter_end_date, "Closed", prev_all_projects_tidied_to_add$status)
prev_all_projects_tidied_to_add$status <- ifelse(prev_all_projects_tidied_to_add$end_date > quarter_end_date, "Active", prev_all_projects_tidied_to_add$status)
prev_all_projects_tidied_to_add$status <- ifelse(is.na(prev_all_projects_tidied_to_add$end_date), "Unknown", prev_all_projects_tidied_to_add$status)

all_projects_tidied <- rbind(all_projects_tidied, prev_all_projects_tidied_to_add)

all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("England","Scotland","Wales","Devon, United Kingdom", "Somerset, UK","Northern Ireland","United","United Kingdom of Great Britain and Northern Ireland (the)","UK"), "United Kingdom", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country == "32 Coconut Drive, San Pedro, Belize", "Belize", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("All possible countries (Global Investigation; no restriction to the sample size)", "Data not available"), NA, all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("TANZANIA","TANZANIA, UNITED REPUBLIC OF", "United Republic of Tanzania"), "Tanzania", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("US","USA, N/A", "United States of America (the)","United States of America"), "United States", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("CÃ´te d'Ivoire for field work and France for laboratory activites","CÃ´te d'Ivoire"), "Ivory Coast", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("Viale Guglielmo Marconi 446, Roma, 00146, Italy"), "Italy", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("Viet Nam"), "Vietnam", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("Netherlands (the)"), "Netherlands", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country %in% c("Afghanistan (and Iraq TBC)"), "Afghanistan, Iraq", all_projects_tidied$lead_org_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country == "PerÃº", "Peru", all_projects_tidied$lead_org_country)
all_projects_tidied$recipient_country <- ifelse(all_projects_tidied$recipient_country == "PerÃº", "Peru", all_projects_tidied$recipient_country)
all_projects_tidied$recipient_country <- ifelse(all_projects_tidied$recipient_country == "All possible countries (Global Investigation; no restriction to the sample size)", NA, all_projects_tidied$recipient_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country == "Cameroon and South Africa", "Cameroon, South Africa", all_projects_tidied$lead_org_country)
all_projects_tidied$recipient_country <- ifelse(all_projects_tidied$recipient_country == "Cameroon and South Africa", "Cameroon, South Africa", all_projects_tidied$recipient_country)
all_projects_tidied$lead_org_country <- ifelse(all_projects_tidied$lead_org_country == "Uganda and Tanzania", "Uganda, Tanzania", all_projects_tidied$lead_org_country)
all_projects_tidied$recipient_country <- ifelse(all_projects_tidied$recipient_country == "Uganda and Tanzania", "Uganda, Tanzania", all_projects_tidied$recipient_country)
all_projects_tidied$recipient_country <- ifelse(all_projects_tidied$recipient_country == "0", NA, all_projects_tidied$recipient_country)


# Save org names and countries to file
org_names_and_locations <- rbind(org_names_and_locations_1, org_names_and_locations_2, org_names_and_locations_2.0, org_names_and_locations_2.1,
                                 org_names_and_locations_3, org_names_and_locations_4) %>% 
  mutate(organisation_name = str_trim(organisation_name)) %>% 
  filter(!is.na(organisation_name)) %>% 
  unique()

saveRDS(org_names_and_locations, file = "Outputs/org_names_and_locations_Jan24update.rds")
org_names_and_locations <- readRDS("Outputs/org_names_and_locations_Jan24update.rds")
# write.xlsx(org_names_and_locations, file = "Outputs/org_names_and_locations_Jan24update.xlsx")

org_names_and_locations[org_names_and_locations$project_id == "CV19RR09",]

project_org_table_leftover <- setdiff(unique(all_projects_tidied$id), unique(org_names_and_locations$project_id))

project_org_table_leftover <- all_projects_tidied %>% filter(id %in% c(project_org_table_leftover))

project_org_table_leftover$lead_org_name <- ifelse(project_org_table_leftover$lead_org_name == "0", NA, project_org_table_leftover$lead_org_name)
project_org_table_leftover$lead_org_name <- ifelse(project_org_table_leftover$extending_org %in% c("ROYAL SOCIETY","Innovate UK","UK SPACE AGENCY","ACADEMY OF MEDICAL SCIENCES",
                                                                                                   "ROYAL ACADEMY OF ENGINEERING","British Council") & is.na(project_org_table_leftover$lead_org_name), project_org_table_leftover$extending_org, project_org_table_leftover$lead_org_name)
project_org_table_leftover$lead_org_country <- ifelse(project_org_table_leftover$lead_org_name %in% c("ROYAL SOCIETY","Innovate UK","UK SPACE AGENCY","ACADEMY OF MEDICAL SCIENCES",
                                                                                                   "ROYAL ACADEMY OF ENGINEERING","British Council"), "United Kingdom", project_org_table_leftover$lead_org_country)


org_leftover_lead <- project_org_table_leftover %>% 
  select(id, lead_org_name, lead_org_country) %>%
  rename(project_id = id, organisation_name = lead_org_name, organisation_country = lead_org_country) %>%
  mutate(organisation_role = 1) # lead

org_leftover_lead$organisation_name <- ifelse(org_leftover_lead$organisation_name == "Contracting org not identified", NA, org_leftover_lead$organisation_name)

names(modari_org_lookup) <- c("organisation_name","organisation_country")

org_leftover_lead_filled <- stringdist_join(org_leftover_lead, modari_org_lookup, by='organisation_name', mode='left', method = "jw", max_dist=99, distance_col='dist',ignore_case = TRUE) %>%
  #group_by(partner_org_name) %>%
  slice_min(order_by=dist, n=1)

org_leftover_lead_filled$organisation_country.y <- ifelse(org_leftover_lead_filled$organisation_country.y == "-", "United Kingdom", org_leftover_lead_filled$organisation_country.y)

org_leftover_lead_filled <- org_leftover_lead_filled %>%
  mutate(organisation_country = coalesce(organisation_country.x, organisation_country.y)) %>%
  select(!c(organisation_country.x, organisation_country.y, dist, organisation_name.y)) %>%
  rename(organisation_name = organisation_name.x)

org_leftover_lead_filled <- org_leftover_lead_filled %>% select(project_id,organisation_country)

org_leftover_lead <- org_leftover_lead %>%
  left_join(org_leftover_lead_filled, by = "project_id") %>%
  mutate(organisation_country = coalesce(organisation_country.x, organisation_country.y)) %>%
  select(!c(organisation_country.x, organisation_country.y))

org_names_and_locations <- rbind(org_names_and_locations, org_leftover_lead)

org_names_and_locations_filled <- stringdist_join(org_names_and_locations, modari_org_lookup, by='organisation_name', mode='left', method = "jw", max_dist=99, distance_col='dist') %>%
  #group_by(partner_org_name) %>%
  slice_min(order_by=dist, n=1)

org_names_and_locations_filled <- org_names_and_locations_filled %>%
  mutate(organisation_country = coalesce(organisation_country.x, organisation_country.y)) %>%
  select(!c(organisation_country.x, organisation_country.y, dist, organisation_name.y)) %>%
  rename(organisation_name = organisation_name.x)

org_names_and_locations_filled <- org_names_and_locations_filled %>% select(project_id,organisation_country)

org_names_and_locations <- org_names_and_locations %>%
  left_join(org_names_and_locations_filled, by = "project_id") %>%
  mutate(organisation_country = coalesce(organisation_country.x, organisation_country.y)) %>%
  select(!c(organisation_country.x, organisation_country.y))

org_names_and_locations <- unique(org_names_and_locations)

saveRDS(org_names_and_locations, file = "Outputs/org_names_and_locations_Jan24update.rds")
# write.xlsx(org_names_and_locations, file = "Outputs/org_names_and_locations_Jan24update.xlsx")

names(org_names_and_locations) <- c("id","lead_org_name","organisation_role","lead_org_country")

all_projects_tidied$lead_org_name <- ifelse(is.na(all_projects_tidied$lead_org_name), all_projects_tidied$extending_org, all_projects_tidied$lead_org_name)
all_projects_tidied$lead_org_name <- ifelse(all_projects_tidied$lead_org_name == "I am a full-time 3rd-year student enrolled in the Bachelor of Medicine & Bachelor of Surgery (MBBS) program at GMERS Medical College, Gandhinagar, Gujarat, India.", "GMERS Medical College", all_projects_tidied$lead_org_name)

all_projects_tidied_leadorg <- all_projects_tidied %>% select("id","lead_org_name","lead_org_country")
all_projects_tidied_leadorg <- all_projects_tidied_leadorg[is.na(all_projects_tidied_leadorg$lead_org_country),]

names(modari_org_lookup) <- c("lead_org_name","lead_org_country")

all_projects_tidied_leadorg <- stringdist_join(all_projects_tidied_leadorg, modari_org_lookup, by='lead_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist',ignore_case = TRUE) %>%
#group_by(partner_org_name) %>%
slice_min(order_by=dist, n=1)

all_projects_tidied_leadorg <- all_projects_tidied_leadorg %>%
  mutate(lead_org_country = coalesce(lead_org_country.x, lead_org_country.y)) %>%
  select(!c(lead_org_country.x, lead_org_country.y, dist, lead_org_name.y)) %>%
  rename(lead_org_name = lead_org_name.x)

all_projects_tidied_leadorg <- all_projects_tidied_leadorg %>% select(id,lead_org_country)

all_projects_tidied <- all_projects_tidied %>%
  left_join(all_projects_tidied_leadorg, by = "id") %>%
  mutate(lead_org_country = coalesce(lead_org_country.x, lead_org_country.y)) %>%
  select(!c(lead_org_country.x, lead_org_country.y))

all_projects_tidied_partorg <- all_projects_tidied %>% select("id","partner_org_name","partner_org_country")
all_projects_tidied_partorg <- all_projects_tidied_partorg[is.na(all_projects_tidied_partorg$partner_org_country),]

names(modari_org_lookup) <- c("partner_org_name","partner_org_country")

all_projects_tidied_partorg <- stringdist_join(all_projects_tidied_partorg, modari_org_lookup, by='partner_org_name', mode='left', method = "jw", max_dist=99, distance_col='dist',ignore_case = TRUE) %>%
  #group_by(partner_org_name) %>%
  slice_min(order_by=dist, n=1)

all_projects_tidied_partorg <- all_projects_tidied_partorg %>%
  mutate(partner_org_country = coalesce(partner_org_country.x, partner_org_country.y)) %>%
  select(!c(partner_org_country.x, partner_org_country.y, dist, partner_org_name.y)) %>%
  rename(partner_org_name = partner_org_name.x)

all_projects_tidied_partorg <- all_projects_tidied_partorg %>% select(id,partner_org_country)

all_projects_tidied <- all_projects_tidied %>%
  left_join(all_projects_tidied_partorg, by = "id") %>%
  mutate(partner_org_country = coalesce(partner_org_country.x, partner_org_country.y)) %>%
  select(!c(partner_org_country.x, partner_org_country.y))


# 10) Save datasets -------------------------------------------

saveRDS(all_projects_tidied, file = "Outputs/all_projects_tidied_Jan24update_2901.rds")
# all_projects_tidied <- readRDS("Outputs/all_projects_tidied_Jan24update.rds")
write.xlsx(all_projects_tidied, file = "Outputs/all_projects_tidied_Jan24update_2901.xlsx")





# Clear environment
rm(org_names_and_locations_1, org_names_and_locations_2, org_names_and_locations_3, 
   org_names_and_locations, ukri_projects_with_countries, 
   nihr_projects_final, 
   iati_projects_final, 
   wellcome_grants_final,
   dhsc_ghs_projects_final,
   roda_extract_gcrf_final, roda_extract_newton_final)



