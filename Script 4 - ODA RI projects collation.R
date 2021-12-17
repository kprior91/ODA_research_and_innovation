# --------------------------------------------------------------- #
# Script 4 
# Extract and collate ODA R&I award level data from
# - IATI Registry 
# - UKRI Gateway to Research
# - NIHR Open Data
# - Wellcome Trust (spreadsheet)
# - FCDO partners (spreadsheets)
# - BEIS (RODA)
# --------------------------------------------------------------- #

# Read in org names and countries from previous script
org_names_and_locations_1 <- readRDS(file = "Outputs/org_names_and_locations_1.rds")

# 1) Extract IATI projects ------------------------------------------------

# Read in list of IATI activities (from UK gov funders and select delivery partners)
iati_activity_list <- readRDS(file = "Outputs/gov_list_final.rds") %>% 
  rename(recipient_country = all_countries)
partner_iati_list <- readRDS(file = "Outputs/partner_activity_list.rds")

# Filter gov department records for project-level activities
iati_projects <- iati_activity_list %>%
  filter(str_detect(iati_identifier, "GB-GOV-3") |   # ex-FCO activities
         str_detect(iati_identifier, "1-205053") |   # South Asia Country Research Fund (FCDO)
         str_detect(iati_identifier, "1-300708") |   # Evidence Fund (FCDO)  
             #   str_detect(iati_identifier, "UKSA") |   # UKSA awards (GCRF)
             #   str_detect(iati_identifier, "NEWT-MO") |   # Met Office awards (Newton)
             #   str_detect(iati_identifier, "NEWT-BIS") |  # Other Met Office awards?
             #   str_detect(iati_identifier, "NEWT-BC") |  # British Council
             #   str_detect(iati_identifier, "GCRF-Clm") |  # Academies
             #   str_detect(iati_identifier, "RS-GCRF|NEWT-RS") |  # Royal Society
             #   str_detect(iati_identifier, "RAENG-GCRF|NEWT-RAE") |  # Royal Academy of Engineering
        str_detect(iati_identifier, "GB-GOV-7")     # Defra activities
  ) %>%    
  filter(flow_type == "ODA") %>% 
  mutate(fund = if_else(is.na(fund), "Unknown", fund)) %>% 
  plyr::rbind.fill(partner_iati_list) # Add partner activities

# Identify UKRI projects (by "RI" IATI tag)
ukri_iati_projects <- iati_activity_list %>% 
  filter(extending_org == "UK Research & Innovation") %>% 
  mutate(gtr_id = str_replace(iati_identifier, "GB-GOV-13-FUND--GCRF-", "")) %>% 
  mutate(gtr_id = str_replace(gtr_id, "GB-GOV-13-FUND--Newton-", "")) %>% 
  mutate(gtr_id = str_replace_all(gtr_id, "_", "/")) %>%
  select(gtr_id, iati_identifier, recipient_country) %>% 
  unique()

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
         iati_id = programme_id,
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

# Clean up
rm(iati_activity_list)
rm(partner_iati_list)
rm(iati_projects)


# 2) Extract UKRI projects -------------------------------------------

### A - Prepare project IDs and fund labels ###

# Label GCRF and Newton projects from IATI UKRI data
ukri_projects_by_fund <- ukri_iati_projects %>% 
  mutate(Fund = case_when(
                   str_detect(iati_identifier, "GCRF") ~ "Global Challenges Research Fund (GCRF)",
                   str_detect(iati_identifier, "Newton") ~ "Newton Fund",
                   TRUE ~ "Other"
                        ),
         Funder = "Department for Business, Energy and Industrial Strategy")

# Join GCRF/Newton project IDs to other ODA IDs (from spreadsheet) 
ukri_projects_ids_full <- ukri_projects_by_fund %>% 
  rbind(ukri_ooda_projects_ids)


### B - Extract project info from GtR API ###

# Create empty dataset to hold projects
ukri_projects_by_id <- data.frame()
org_names_and_locations_2 <- data.frame()

# Run project info extraction over all GtR projects

n <- 1 # set counter

for (id in ukri_projects_ids_full$gtr_id) {
  
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

saveRDS(ukri_projects_by_id, file = "Outputs/ukri_projects_by_id.rds")
# ukri_projects_by_id <- readRDS("Outputs/ukri_projects_by_id.rds") 


### C - Add on fund and funder labels

# Join to fund and funder info from original list
ukri_projects_by_id_with_id <- ukri_projects_by_id %>% 
  left_join(select(ukri_projects_ids_full, 
                   iati_id = iati_identifier, Fund, Funder, gtr_id), by = "gtr_id") 

# See which awards from input list have not been found
missing_awards <- select(ukri_projects_ids_full, gtr_id) %>% 
  left_join(select(ukri_projects_by_id_with_id, gtr_id, title), by = "gtr_id") %>% 
  filter(is.na(title)) %>% 
  unique()

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
         Fund = if_else(Fund == "GCRF", "Global Challenges Research Fund (GCRF)",
                        if_else(Fund == "Newton", "Newton Fund", Fund)),
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
saveRDS(ukri_projects_with_countries, file = "Outputs/ukri_projects_with_countries.rds")
# ukri_projects_with_countries <- readRDS("Outputs/ukri_projects_with_countries.rds") 

# Save org names and countries to file
saveRDS(org_names_and_locations_2, file = "Outputs/org_names_and_locations_2.rds")
# org_names_and_locations_2 <- readRDS(file = "Outputs/org_names_and_locations_2.rds")


rm(data)
rm(n)
rm(id)
rm(missing_awards)
rm(ukri_gcrf_newton_ids)
rm(ukri_projects_by_fund)
rm(ukri_projects_by_fund_with_id)
rm(ukri_projects_by_id)
rm(ukri_projects_by_id_with_id)
rm(ukri_projects_ids)


# 3) Extract NIHR projects ------------------------------------------------

# Define URL to extract ODA projects
path <- paste0("https://nihr.opendatasoft.com/api/records/1.0/search/?dataset=infonihr-open-dataset&q=&rows=6000&facet=funder&facet=project_status&facet=programme&facet=programme_type&facet=programme_stream&facet=start_date&facet=acronym&facet=ctry17nm&facet=rgn17nm&facet=lad19nm&facet=pconnm&refine.funder=NIHR+(ODA)")

# Extract data from the NIHR API
request <- GET(url = path)
request$status_code # 200 = success

# Convert to text and read from JSON
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 

# Extract dataframe
nihr_projects <- response$records 

# Remove unneeded columns
nihr_projects <- nihr_projects %>% 
  select(-1, -2) 

# Remove "field." from column names
names(nihr_projects) <- gsub(pattern = "fields.", replacement = "", x = names(nihr_projects))

# Select order of columns
nihr_projects_final <- nihr_projects %>% 
  mutate(id = project_id,
         Funder = "Department of Health and Social Care",
         Fund = "Global Health Research - Programmes",
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
         partner_org_name = NA_character_,
         partner_org_country = NA_character_,
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
                    mutate(organisation_role = 1)

# Save as R file (to read back in if needed)
saveRDS(nihr_projects_final, file = "Outputs/nihr_projects_final.rds")
# nihr_projects_final <- readRDS("Outputs/nihr_projects_final.rds") 


rm(nihr_projects)
rm(request)
rm(response)


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
                        "Global Health Research - Partnerships", "FCDO Research - Programmes"),
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

rm(wellcome_grants)
rm(wellcome_grants_formatted)

# 5) FCDO spreadsheet data

# Detect all Excel files in Data folder
path = "Inputs//IATI returns"
file_list <- list.files(path = path, pattern='*.xlsx', full.names = TRUE)

# Read all files into R
data_list <- lapply(file_list, 
                    read_excel, 
                    sheet = 2)

# Bind the rows, adding an ID field for the Excel file number
partner_spreadsheet_data <- bind_rows(data_list, .id = "file_number")

# Reformat to match other dataset
collated_spreadsheet_data <- partner_spreadsheet_data %>% 
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
  select(-`No.`, -`Funder programme - name`, -Notes, -file_number, -Currency,
         -`Aims/Objectives`, -`Investigator(s) - name`, -`FCDO programme - name`,
         -`FCDO programme - IATI ID`, -Link)


# Save as R file (to read back in if needed)
saveRDS(collated_spreadsheet_data, file = "Outputs/collated_spreadsheet_data.rds")
# collated_spreadsheet_data <- readRDS("Outputs/collated_spreadsheet_data.rds") 

# Write lead org names and countries to file
org_names_and_locations_3 <- org_names_and_locations_3 %>% 
  rbind(select(collated_spreadsheet_data,
               project_id = id,
               organisation_name = lead_org_name,
               organisation_country = lead_org_country) %>% 
          mutate(organisation_role = 1))

# Write partner org names and countries to file (where simple to do)
spreadsheet_partners <- collated_spreadsheet_data %>% 
  select(id, partner_org_name, partner_org_country) %>% 
  # Exclude missings, multiple and miscellaneous partners
  filter(!is.na(partner_org_name),
         !str_detect(partner_org_name, ",|;"),
         !str_detect(partner_org_country, ",|;|N/A")) 

org_names_and_locations_3 <- org_names_and_locations_3 %>% 
  rbind(select(spreadsheet_partners,
               project_id = id,
               organisation_name = partner_org_name,
               organisation_country = partner_org_country) %>% 
          mutate(organisation_role = 2))

rm(partner_spreadsheet_data)
rm(data_list)
rm(file_list)


# 6) BEIS RODA data (spreadsheet)

# Reformat to match other datasetS
roda_extract_gcrf_final <- roda_extract_gcrf %>% 
  rename(id = `RODA identifier`,
         abstract = Description,
         title = Title,
         amount = Amount,
         recipient_country = `Recipient country`,
         extending_org = `Delivery partner`,
         lead_org_name = `Lead Organisation`
  ) %>% 
  mutate(Fund = "Global Challenges Research Fund (GCRF)",
         Funder = "Department for Business, Energy and Industrial Strategy",
         start_date = as.character(as.Date(coalesce(`Actual start date`, `Planned start date`), "%d %B %Y")),
         end_date = as.character(as.Date(coalesce(`Actual end date`, `Planned end date`), "%d %B %Y")),
         lead_org_country = map(lead_org_name, org_country_lookup),
         partner_org_name = NA_character_,
         partner_org_country = NA_character_,
         iati_id = NA_character_,
         currency = "GBP",
         status = if_else(Status %in% c("Spend in progress", "Agreement in place", "Delivery", "Finalisation"), "Active",
                          if_else(Status %in% c("Completed"), "Closed", 
                                  if_else(Status %in% c("Cancelled"), "Cancelled", "Unknown"))),
         period_start = NA_character_,
         period_end = NA_character_,
         subject = NA_character_,
         last_updated = quarter_end_date,
         link = NA_character_
  ) %>% 
  unnest(cols = lead_org_country) %>% 
    # suppress display of active project end dates that have passed
  mutate(end_date = if_else(status == "Active" & Sys.Date() <= end_date, end_date, NA_character_)) %>%
    # remove unecessary variables
  select(-Level, -`Recipient region`, -`Planned start date`, -`Actual start date`,  -`Planned end date`,
         -`Actual end date`, -Status)


roda_extract_newton_final <- roda_extract_newton %>% 
  rename(id = `RODA ID`,
         title = Title,
         abstract = Description,
         amount = Amount,
         recipient_country = `Recipient country`,
         extending_org = `Delivery partner`,
         lead_org_name = `Lead Organisation`,
         partner_org_name = `Partner organisations`) %>% 
  mutate(Fund = "Newton Fund",
         Funder = "Department for Business, Energy and Industrial Strategy",
         lead_org_country = map(lead_org_name, org_country_lookup),
         partner_org_country = NA_character_,
         iati_id = NA_character_,
         link = NA_character_,
         start_date = as.character(as.Date(coalesce(`Actual start date`, `Planned start date`), "%d %B %Y")),
         end_date = as.character(as.Date(coalesce(`Actual end date`, `Planned end date`), "%d %B %Y")),
         currency = "GBP",
         status = if_else(Status %in% c("Spend in progress", "Agreement in place", "Delivery", "Finalisation"), "Active",
                          if_else(Status %in% c("Completed"), "Closed", 
                                  if_else(Status %in% c("Cancelled"), "Cancelled", "Unknown"))),
         period_start = NA_character_,
         period_end = NA_character_,
         subject = NA_character_,
         last_updated = quarter_end_date) %>% 
  unnest(cols = lead_org_country) %>%
  # suppress display of end dates that have passed
  mutate(end_date = if_else(Sys.Date() <= end_date, end_date, NA_character_)) %>%
  
  select(-Level, -`Recipient region`, -`Planned start date`, -Status,
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

saveRDS(org_names_and_locations_3, file = "Outputs/org_names_and_locations_3.rds")
# org_names_and_locations_3 <- readRDS("Outputs/org_names_and_locations_3.rds") 


rm(roda_extract_gcrf)
rm(roda_extract_newton)


# 7) Join funder datasets together ----------------------------------------------

all_projects <- rbind(ukri_projects_with_countries, 
                      nihr_projects_final, 
                      iati_projects_final, 
                      wellcome_grants_final,
                      collated_spreadsheet_data,
                      roda_extract_gcrf_final, roda_extract_newton_final) %>% 
  unique() %>% 
  ungroup()


# 8) Manual exclusions and formatting -------------------------------------------

# Manually edit country info for Chevening Scholarships
all_projects_tidied <- all_projects %>% 
  mutate(lead_org_country = if_else(Fund == "Chevening Scholarships", "United Kingdom", lead_org_country),
         start_date = if_else(Fund == "Chevening Scholarships", NA_character_, start_date))

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
                                "Coffey International Development Limited, a Tetra Tech Company")))

# Correct missing IDS name (ARPA activity)
all_projects_tidied <- all_projects_tidied %>% 
  mutate(extending_org = if_else(extending_org == "GB-COH-877338", 
                                 "Institute of Development Studies", extending_org))

# Remove WHO non-research/innovation activities
all_projects_tidied <- all_projects_tidied %>% 
  filter(!(extending_org == "World Health Organization") |
           str_detect(title, "research|innovation"))

# Add FCDO DevTracker links in absence of other public source
all_projects_tidied <- all_projects_tidied %>% 
  mutate(link = if_else((str_detect(iati_id, "GB-GOV-1-") | str_detect(iati_id, "GB-1-")) & is.na(link),
                        paste0("https://devtracker.fcdo.gov.uk/projects/", iati_id, "/summary"), link))


# 9) Save datasets -------------------------------------------

saveRDS(all_projects_tidied, file = "Outputs/all_projects_tidied.rds")
# all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 

# Save org names and countries to file
org_names_and_locations <- rbind(org_names_and_locations_1, org_names_and_locations_2, 
                                 org_names_and_locations_3) %>% 
  mutate(organisation_name = str_trim(organisation_name)) %>% 
  filter(!is.na(organisation_name)) %>% 
  unique()

saveRDS(org_names_and_locations, file = "Outputs/org_names_and_locations.rds")

