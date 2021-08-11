#####################################
# Script 3 
# Extract and collate ODA R&I award level data from
# - UKRI Gateway to Research
# - NIHR Open Data
# - IATI Registry 
# - Wellcome Trust (spreadsheet)
# - FCDO partners (spreadsheets)
#####################################

if (!("googlesheets4" %in% installed.packages())) {
  install.packages("googlesheets4")
}
if (!("gargle" %in% installed.packages())) {
  install.packages("gargle")
}
if (!("geonames" %in% installed.packages())) {
  install.packages("geonames")
}
if (!("RgoogleMaps" %in% installed.packages())) {
  install.packages("RgoogleMaps")
}
if (!("rworldmap" %in% installed.packages())) {
  install.packages("rworldmap")
}
if (!("ggmap" %in% installed.packages())) {
  install.packages("ggmap")
}
if (!("jsonlite" %in% installed.packages())) {
  install.packages("jsonlite")
}
if (!("rvest" %in% installed.packages())) {
  install.packages("rvest")
}
if (!("stringi" %in% installed.packages())) {
  install.packages("stringi")
}
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")
}
if (!("readxl" %in% installed.packages())) {
  install.packages("readxl")
}
if (!("writexl" %in% installed.packages())) {
  install.packages("writexl")
}

# Load packages -----
library(geonames) 
library(RgoogleMaps)
library(rworldmap)
library(ggmap)
library(jsonlite)
library(rvest)
library(stringi)
library(googlesheets4)
library(gargle)
library(httr)
library(tidyverse)
library(writexl)
library(readxl)

# -- Read in GRID data
# Match institutions to countries with GRID database
grid_institutes <- read.csv("Inputs/GRID tables/institutes.csv") %>% 
  select(grid_id, name) %>% 
  unique()  %>% 
    # Remove common organisation names
  filter(!(name %in% c("Ministry of Health", "Ministry of Public Health")))

grid_addresses <- read.csv("Inputs/GRID tables/addresses.csv") %>% 
  select(grid_id, country, country_code) %>% 
  unique()

grid_aliases <- read.csv("Inputs/GRID tables/aliases.csv")


# 1) Extract UKRI projects -------------------------------------------

### A - GCRF/Newton IDs ###

# Define function to extract projects by fund name (GCRF/Newton)
extract_ukri_projects_by_fund <- function(page, fund) {
  
  path <- paste0("https://gtr.ukri.org:443/gtr/api/projects?q=",
                 fund, "&f=pro.rcukp&p=", page, "&s=100")
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  projects <- response$project
  return(projects)
}

# Create empty dataset to hold projects
ukri_projects_by_fund <- data.frame()

for (fund in c("GCRF", "Newton")) {
  
  for (page in c(1:15)) {
    
    projects <- extract_ukri_projects_by_fund(page, fund)
    
    # Check if data exists, label country column and append to master
    # project list if so
    if(!is.null(nrow(projects))) {
      
      projects <- projects %>% 
        mutate(Fund = fund,
               Funder = "Department for Business, Energy and Industrial Strategy")
      
      if("participantValues" %in% names(projects)) {
        projects <- projects %>% 
          select(-participantValues)
      }
      
      if(!("participantValues.participant" %in% names(projects))) {
        projects <- projects %>% 
          mutate(participantValues.participant = "")
      }   
      
      ukri_projects_by_fund <- ukri_projects_by_fund %>% 
        rbind(projects)
      
    } 
    
  }
}

saveRDS(ukri_projects_by_fund, file = "Outputs/ukri_projects_by_fund.rds")
# ukri_projects_by_fund <- readRDS("Outputs/ukri_projects_by_fund.rds") 

# Extract GTR ID from extract
ukri_projects_by_fund_with_id <- ukri_projects_by_fund %>% 
  unnest(cols = identifiers.identifier) %>% 
  rename(`GtR ID` = value) %>% 
  select(-type)

# Format data to join to other GtR ODA projects
ukri_gcrf_newton_ids <- ukri_projects_by_fund_with_id %>% 
  mutate(`IATI ID` = "", Stage = "", Funder = "Department for Business, Energy and Industrial Strategy") %>% 
  select(`IATI ID`, Fund, Funder, extending_org = leadFunder, Stage, `GtR ID`, `Project Title` = title)


### B - Combine GCRF/Newton project IDs with "other ODA" ones ###

# Read in other ODA UKRI projects
ukri_projects_ids <- read_xlsx("Inputs/UKRI non GCRF-Newton projects.xlsx", sheet=1)

# Join GCRF/Newton project IDs on  
ukri_projects_ids <- ukri_projects_ids %>% 
  rbind(ukri_gcrf_newton_ids)

### c - Extract project info from GtR API ###

extract_ukri_projects_by_id <- function(id) {
  
  path <- paste0("http://gtr.ukri.org/projects?ref=", id)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  data <- response$projectOverview
  
  if(length(data) > 0) {
    
    # Unlist first level
    data <- data$projectComposition
    
    # Extract project, lead org and participating org lists
    projects <- data$project
    lead_org <- data$leadResearchOrganisation
    org_roles <- data$organisationRole %>% 
      rename(org_name = name) %>% 
      unnest(cols = role) %>% 
      filter(name %in% c("PARTICIPATING", "COLLABORATOR", "PARTICIPANT")) 
    
    # Collapse partner orgs into single records
    if(length(org_roles$name) > 0) {
      
      org_roles <- org_roles %>% 
        left_join(grid_institutes, by = c("org_name" = "name")) %>% 
        left_join(grid_addresses, by = "grid_id") 
      
      org_names <- org_roles %>% 
        select(org_name) %>% 
        unique() %>% 
        summarise(partner_name = paste(org_name, collapse = ", "))
      
      if("address.country" %in% names(org_roles)) {
        org_countries <- org_roles %>% 
          mutate(address.country = coalesce(address.country, country)) %>% 
          select(address.country) %>% 
          unique() %>% 
          summarise(partner_country = paste(address.country[!is.na(address.country)], collapse = ", "))
        
      } else {
        
        org_countries <- org_roles %>% 
          select(country) %>% 
          unique() %>% 
          summarise(partner_country = paste(country[!is.na(country)], collapse = ", "))
      }
        
      org_roles_summarised <- cbind(org_names, org_countries)
      
    }
    
    # Start constructing data frame
    project_data <- data.frame(
      title = projects[["title"]],
      status = projects[["status"]],
      gtr_id = projects[["grantReference"]],
      fund = projects[["fund"]],
      abstract = projects[["abstractText"]],
      lead_org_name = lead_org[["name"]])
    
    # Unnest if a lead org address is given
    if(length(lead_org[["address"]]) > 0) {
      
      lead_org_address <- lead_org[["address"]]
      
      # Extract lead org country
      if("country" %in% names(lead_org_address)) {
        project_data <- project_data %>% 
          mutate(lead_org_country = lead_org_address$country) 
        
      } else {
        project_data <- project_data %>% 
          mutate(lead_org_country = "Unknown")              
      }
      
    } else {
      project_data <- project_data %>% 
        mutate(lead_org_country = "Unknown")              
    }
    
    # Attach partner org info
    if(length(org_roles$name) == 0) {
      project_data <- project_data %>% 
        mutate(partner_org_name = "",
               partner_org_country = "")
    } else {
      project_data <- project_data %>% 
        mutate(partner_org_name = org_roles_summarised$partner_name,
               partner_org_country = org_roles_summarised$partner_country)
    }
    
    # Keep desired fields
    project_data <- project_data %>% 
      select(gtr_id, title, abstract, fund.start, fund.end, amount = fund.valuePounds, extending_org = fund.funder.name,
             lead_org_name, lead_org_country, partner_org_name, partner_org_country) 
    
    return(project_data)
  }
  
}

# Create empty dataset to hold projects
ukri_projects_by_id <- data.frame()

for (id in ukri_projects_ids$`GtR ID`) {
  
  print(id)
  
  data <- extract_ukri_projects_by_id(id)
  
  ukri_projects_by_id <- ukri_projects_by_id %>% 
    rbind(data)
  
}

saveRDS(ukri_projects_by_id, file = "Outputs/ukri_projects_by_id.rds")
# ukri_projects_by_id <- readRDS("Outputs/ukri_projects_by_id.rds") 

### D 
# Join to fund and funder info from original list
ukri_projects_by_id_with_id <- ukri_projects_by_id %>% 
  left_join(select(ukri_projects_ids, 
                   iati_id = `IATI ID`, Fund, Funder,`GtR ID`), by = c("gtr_id" = "GtR ID")) 

# See which awards from input list have not been found
missing_awards <- select(ukri_projects_ids, `GtR ID`) %>% 
  left_join(select(ukri_projects_by_id_with_id, gtr_id, title), by = c("GtR ID" = "gtr_id")) %>% 
  filter(is.na(title)) %>% 
  unique()

# Convert all factor fields to character
ukri_projects_final <- data.frame(lapply(ukri_projects_by_id_with_id, as.character), stringsAsFactors=FALSE)


ukri_projects_final <- ukri_projects_final %>% 
  rename(start_date = fund.start,
         end_date = fund.end,
         id = gtr_id,
  ) %>% 
  mutate(recipient_country = "",
         subject = "",
         amount = as.numeric(amount),
         currency = "GBP",
         status = if_else(as.Date(end_date) > Sys.Date(), "Active", "Closed"),
         Fund = if_else(Fund == "GCRF", "Global Challenges Research Fund (GCRF)",
                        if_else(Fund == "Newton", "Newton Fund", Fund))) %>% 
  select(id,
         title, 
         abstract,
         start_date,
         end_date,
         amount,
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
         subject,
         status) %>% 
  unique()

# Add GtR link to projects
ukri_projects_final <- ukri_projects_final %>% 
  mutate(link = paste0("https://gtr.ukri.org/projects?ref=", id))

# Save as R file (to read back in if needed)
saveRDS(ukri_projects_final, file = "Outputs/ukri_projects_final.rds")
# ukri_projects_final <- readRDS("Outputs/ukri_projects_final.rds") 


# 2) Extract NIHR projects ------------------------------------------------

# Define URL to extract ODA projects
path <- paste0("https://nihr.opendatasoft.com/api/records/1.0/search/?dataset=infonihr-open-dataset&q=&rows=100&facet=funder&facet=project_status&facet=programme&facet=programme_type&facet=programme_stream&facet=start_date&facet=acronym&facet=ctry17nm&facet=rgn17nm&facet=lad19nm&refine.funder=NIHR+(ODA)")

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
  select(-1, -2, -3) 

# Remove "field." from column names
names(nihr_projects) <- gsub(pattern = "fields.", replacement = "", x = names(nihr_projects))

# Select order of columns
nihr_projects_final <- nihr_projects %>% 
  mutate(id = project_id,
         Funder = "Department of Health and Social Care",
         Fund = "Global Health Research - Programmes",
         recipient_country = "",
         lead_org_country = ctry17nm,
         iati_id = "",
         subject = programme,
         currency = "GBP",
         partner_org_name = "",
         partner_org_country = "",
         extending_org = "NIHR") %>% 
  select(id, 
         title = project_title,
         abstract = scientific_abstract,
         start_date, end_date,
         amount = award_amount_from_dh,
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
         status = project_status)

# Add NIHR link to awards
nihr_projects_final <- nihr_projects_final %>% 
  mutate(link = paste0("https://fundingawards.nihr.ac.uk/award/", id))


# 3) Extract IATI projects ------------------------------------------------

# Read in list of IATI activities (from funders and select delivery partners)
iati_activity_list <- readRDS(file = "Outputs/gov_list_final.rds")
partner_iati_list <- readRDS(file = "Outputs/partner_activity_list.rds")

# Filter gov department records for minimum granularity
iati_projects <- iati_activity_list %>%
  filter(  str_detect(iati_identifier, "GB-GOV-3") |   # ex-FCO activities
           str_detect(iati_identifier, "1-205053") |   # South Asia Country Research Fund (FCDO)
           str_detect(iati_identifier, "1-204584") |   # Policy Research Fund (FCDO)  
           str_detect(iati_identifier, "UKSA") |   # UKSA awards (GCRF)
           str_detect(iati_identifier, "NEWT-MO") |   # Met Office awards (Newton)
           str_detect(iati_identifier, "NEWT-BIS") |  # Other Met Office awards?
           str_detect(iati_identifier, "NEWT-BC") |  # British Council
           str_detect(iati_identifier, "GCRF-Clm") |  # Academies
           str_detect(iati_identifier, "RS-GCRF|NEWT-RS") |  # Royal Society
           str_detect(iati_identifier, "RAENG-GCRF|NEWT-RAE") |  # Royal Academy of Engineering
           str_detect(iati_identifier, "GB-GOV-7")     # Defra activities
  ) %>%    
  filter(flow_type == "ODA") %>% 
  mutate(fund = if_else(is.na(fund), "Unknown", fund)) %>% 
  plyr::rbind.fill(partner_iati_list) # Add partner activities

# Keep required fields
iati_projects_final <- iati_projects %>% 
  mutate(Funder = coalesce(gov_funder, reporting_org),
         lead_org_country = "",
         partner_org_name = "",
         partner_org_country = "",
         extending_org = coalesce(extending_org, reporting_org)) %>% 
  select(id = iati_identifier,
         title = activity_title, 
         abstract = activity_description,
         start_date,
         end_date,
         amount,
         currency,
         extending_org,
         lead_org_name = partner,
         lead_org_country = partner_country,
         partner_org_name,
         partner_org_country,
         iati_id = programme_id,
         Fund = fund,
         Funder, 
         recipient_country = all_countries,
         subject = sector_name,
         status = activity_status
  ) 

# Add IATI link to awards
iati_projects_final <- iati_projects_final %>% 
  mutate(link = paste0("https://d-portal.org/ctrack.html#view=act&aid=", id))

# Clean up
rm(request)
rm(response)


# 4) Extract Wellcome projects ------------------------------------------------

# Read in public data on Wellcome Grants
wellcome_grants <- read_excel("Inputs/wellcome-grants-awarded-2005-2020.xlsx")

# Read in partnerships data provided by Annie (Jan 21) - restrict to suspected ODA
wellcome_partnerships <- read_excel("Inputs/Active Partnership record - 25 01 2021 (ODA labelled).xlsx") %>% 
  filter(`ODA funding?` %in% c("Yes", "Maybe"),
         `How the partnership is paid` != "Wellcome pays the partner, the partner pays the awardee",
         `Partner Organisation(s)` != "Medical Research Council")  # exclude MRC ones - these will be on GtR

# Join the two
wellcome_grants_comb <- wellcome_grants %>% 
  inner_join(wellcome_partnerships, by = c("Internal ID" = "Reference")) 

# Add missing fields and format Funder/Fund field
wellcome_grants_comb <- wellcome_grants_comb %>% 
  mutate(status = if_else(Sys.Date() <= `Planned Dates:End Date`, "Active", "Closed"),
         extending_org = "Wellcome Trust",
         currency = "GBP",
         partner_org_name = "",
         partner_org_country = `Research Location Countries`,
         recipient_country = "",
         Funder = if_else(str_detect(`Partner Organisation(s)`, "National Institute for Health Research"), 
                          "Department of Health and Social Care", `Partner Organisation(s)`),
         Fund = if_else(Funder == "Department of Health and Social Care",
                        "Global Health Research - Partnerships", "FCDO partially funded")) 

# Select desired variables
wellcome_grants_comb <- wellcome_grants_comb %>% 
  select(id = `Internal ID`,
         title = Title.x, 
         abstract = Description,
         start_date = `Planned Dates:Start Date`,
         end_date = `Planned Dates:End Date`,
         amount = `Amount Awarded`,
         currency,
         extending_org,
         lead_org_name = `Recipient Org:Name`,
         lead_org_country = `Recipient Org:Country`,
         partner_org_name,
         partner_org_country,
         iati_id = `Partnership Name`,
         Fund,
         Funder, 
         recipient_country,
         subject = `Master Grant Type Name`,
         status
  ) 

# Format date fields for merging
wellcome_grants_final <- wellcome_grants_comb %>% 
  mutate(start_date = as.character(start_date),
         end_date = as.character(end_date),
         link = "https://wellcome.org/grant-funding/funded-people-and-projects")



# 5) FCDO spreadsheet data

# Detect all Excel files in Data folder
path = "C:\\Users\\e-clegg\\OneDrive - DFID\\PROJECT - MODARI\\2. Awards\\IATI\\External partner data\\2 - Completed returns"
file_list <- list.files(path = path, pattern='*.xlsx', full.names = TRUE)

# Read all files into R (skipping first 28 lines in Excel sheet as this contains no data)
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
         currency = coalesce(Currency, "GDP"),
         subject = "",
         status = if_else(end_date >= Sys.Date(), "Active", "Closed")
         ) %>% 
  select(-`No.`, -`Funder programme - name`, -Notes, -file_number, -Currency,
         -`Aims/Objectives`, -`Investigator(s) - name`, -`FCDO programme - name`,
         -`FCDO programme - IATI ID`)


# 6) Join 5 sources together ----------------------------------------------

all_projects <- rbind(ukri_projects_final, nihr_projects_final, 
                      iati_projects_final, wellcome_grants_final,
                      collated_spreadsheet_data)


# Change terminology around award status
all_projects <- all_projects %>% 
  mutate(status = if_else(status %in% c("Contracted", "Implementation"), "Active", status)) %>% 
  unique()

# Save as R file (to read back in if needed)
saveRDS(all_projects, file = "Outputs/all_projects.rds")
# all_projects <- readRDS("Outputs/all_projects.rds") 


# 7) CHECKING ----
test1 <- filter(all_projects, str_detect(extending_org, "Elrha"))
test2 <- filter(all_projects, str_detect(extending_org, "Abdul"))

test <- filter(all_projects, str_detect(id, "MR/N006267/1"))


