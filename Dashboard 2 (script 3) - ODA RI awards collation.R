#####################################
# Script 3 
# Extract and collate ODA R&I award level data from
# - UKRI Gateway to Research
# - NIHR Open Data
# - IATI Registry 
# - Wellcome Trust (spreadsheet)
# - FCDO partners (spreadsheets)
#####################################

if (!("httr" %in% installed.packages())) {
  install.packages("httr")
}
if (!("jsonlite" %in% installed.packages())) {
  install.packages("jsonlite")
}
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")
}
if (!("readxl" %in% installed.packages())) {
  install.packages("readxl")
}


# Load packages -----
library(jsonlite)
library(httr)
library(tidyverse)
library(readxl)


# 1) Set up -------------------------------------------

# Set quarter end date
quarter_end_date <- as.Date("2021-06-30")

# Read in GRID data

grid_institutes <- read.csv("Inputs/GRID tables/institutes.csv") %>% 
  select(grid_id, name) %>% 
  unique()  %>% 
    # Remove common organisation names
  filter(!(name %in% c("Ministry of Health", "Ministry of Public Health")))

grid_addresses <- read.csv("Inputs/GRID tables/addresses.csv") %>% 
  select(grid_id, country, country_code) %>% 
  unique()

grid_aliases <- read.csv("Inputs/GRID tables/aliases.csv")



# 2) Extract UKRI projects -------------------------------------------

### A - Define UKRI API functions ###

# 1 - Function to extract project IDs by fund name (GCRF/Newton)
extract_ukri_projects_by_fund <- function(page, fund) {
  
  path <- paste0("https://gtr.ukri.org:443/gtr/api/projects?q=",
                 fund, "&f=pro.rcukp&p=", page, "&s=100")
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  projects <- response$project
  
  return(projects)
}


# 2 - Function to extract staff organisation
# person_id <- "6E394347-A44B-4868-8EC3-06CA4D034BDA"

extract_staff_org <- function(staff_data, person_id) {
  
  path <- paste0("http://gtr.ukri.org/person/", person_id)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  person_current_org_name <- ((response$personOverview)$organisation)$name
  person_current_org_id <- ((response$personOverview)$organisation)$id
  
  staff_org_data <- rbind(staff_data, data.frame(person_id, 
                                                     person_current_org_name,
                                                     person_current_org_id))
  return(staff_org_data)
}


# 3 - Function to extract country from organisation ID
# (checking GRID database as well as UKRI)
# org_id <- "3ED60B49-9C2B-4D71-B644-96CCC7F10194"

extract_org_country <- function(org_id) {
  
  path <- paste0("http://gtr.ukri.org/organisation/", org_id)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # Look up country from UKRI GtR 
  org_address <- ((response$organisationOverview)$organisation)$address
  
    if("country" %in% names(org_address)) {
      org_country_ukri <- org_address$country
      
    } else {
      org_country_ukri <- "Unknown"
    }
  
  # Look up country from GRID database
  grid_org_search <- data.frame(name = ((response$organisationOverview)$organisation)$name) %>% 
    left_join(grid_institutes, by = "name") %>% 
    left_join(grid_addresses, by = "grid_id")
  
  # Use GRID country over UKRI GtR one
  org_country <- coalesce(grid_org_search$country, org_country_ukri)
  
  return(org_country)
}


### B - Extract GCRF/Newton project IDs ###

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
  mutate(`Funder IATI ID` = "", Funder = "Department for Business, Energy and Industrial Strategy") %>% 
  select(`Funder IATI ID`, Fund, Funder, `Extending Org` = leadFunder, `GtR ID`)


### C - Combine GCRF/Newton project IDs with "other ODA" ones ###
### TO UPDATE ###

# Read in other ODA UKRI projects
ukri_projects_ids <- read_xlsx("Inputs/UKRI non GCRF-Newton projects.xlsx", sheet=1)

# Join GCRF/Newton project IDs on  
ukri_projects_ids <- ukri_projects_ids %>% 
  rbind(ukri_gcrf_newton_ids)


### D - Extract project info from GtR API ###

# id <- "102643"

extract_ukri_projects_by_id <- function(id) {
  
  path <- paste0("http://gtr.ukri.org/projects?ref=", id)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # extract project data and last refresh date
  data <- response$projectOverview
  
  last_updated <- (response$lastRefreshDate)$lastRefreshDate %>% 
    str_replace_all("Data last updated:  ", "") %>% 
    as.Date(format = "%d %b %Y")

  
  if(length(data) > 0) {
    
    # Unlist first level
    data <- data$projectComposition
    
    # Extract project, lead org and co-investigator staff ids
    projects <- data$project
    lead_org <- data$leadResearchOrganisation
    person_roles <- data$personRole
    
    # Extract staff information (if applicable)
    if(length(person_roles) > 0) {                        # checks length of list
      
        person_roles <- person_roles %>% 
          unnest(col = role) %>% 
          filter(name == "CO_INVESTIGATOR") %>% 
          select(id)
      
              if(nrow(person_roles) > 0) {                # checks no. of rows in dataframe
                
              # Extract current organisation of staff
              staff_org_data <- data.frame()
              
              for (person_id in person_roles$id) {
                staff_org_data <- extract_staff_org(staff_org_data, person_id)
              }
            
              # Join on country of organisation
              staff_org_data <- staff_org_data %>% 
                mutate(person_current_org_country = map(person_current_org_id, extract_org_country)) %>% 
                unnest(col = person_current_org_country)
            
              
              # Collapse staff partner orgs and countries into single records
              if(length(staff_org_data$person_current_org_name) > 0) {
                
                staff_org_names <- staff_org_data %>% 
                  select(person_current_org_name) %>% 
                  unique() %>% 
                  summarise(partner_name = paste(person_current_org_name, collapse = ", "))
                
                staff_org_countries <- staff_org_data %>% 
                  select(person_current_org_country) %>% 
                  filter(person_current_org_country != "Unknown") %>% 
                  unique() %>% 
                  summarise(partner_country = paste(person_current_org_country, collapse = ", "))
                
                org_roles_summarised <- cbind(staff_org_names, staff_org_countries)
                
              }
          }
    }
    
    # Start constructing project data frame
    project_data <- data.frame(
      title = projects[["title"]],
      status = projects[["status"]],
      gtr_id = projects[["grantReference"]],
      fund = projects[["fund"]],
      abstract = projects[["abstractText"]],
      lead_org_name = lead_org[["name"]],
      last_updated = as.Date(last_updated))
    
    # Add country of lead org
    project_data <- project_data %>% 
      mutate(lead_org_country = map(lead_org[["id"]], extract_org_country)) %>% 
      unnest(col = lead_org_country)
    
    # Attach partner org info
    if(exists("org_roles_summarised")) {
      project_data <- project_data %>% 
        mutate(partner_org_name = org_roles_summarised$partner_name,
               partner_org_country = org_roles_summarised$partner_country)
    } else {
      project_data <- project_data %>% 
        mutate(partner_org_name = "",
               partner_org_country = "")
    }
    
    # Keep desired fields
    project_data <- project_data %>% 
      select(gtr_id, title, abstract, fund.start, fund.end, amount = fund.valuePounds, extending_org = fund.funder.name,
             lead_org_name, lead_org_country, partner_org_name, partner_org_country, last_updated) 
    
    
  } else {
    
    # If no data available to extract, return empty dataframe
    project_data <- data.frame()
  }
  
  return(project_data)
}



# Create empty dataset to hold projects
ukri_projects_by_id <- data.frame()

# Run project info extraction over all GtR projects

n <- 0 # set counter

for (id in ukri_projects_ids$`GtR ID`) {
  
  print(paste0(n, " - ", id))
  
  data <- extract_ukri_projects_by_id(id)
  
  ukri_projects_by_id <- ukri_projects_by_id %>% 
    rbind(data)
  
  n <- n+1
  
}

saveRDS(ukri_projects_by_id, file = "Outputs/ukri_projects_by_id.rds")
# ukri_projects_by_id <- readRDS("Outputs/ukri_projects_by_id.rds") 


### E - Add on fund and funder labels

# Join to fund and funder info from original list
ukri_projects_by_id_with_id <- ukri_projects_by_id %>% 
  left_join(select(ukri_projects_ids, 
                   iati_id = `Funder IATI ID`, Fund, Funder,`GtR ID`), by = c("gtr_id" = "GtR ID")) 

# See which awards from input list have not been found
missing_awards <- select(ukri_projects_ids, `GtR ID`) %>% 
  left_join(select(ukri_projects_by_id_with_id, gtr_id, title), by = c("GtR ID" = "gtr_id")) %>% 
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
  mutate(recipient_country = "",
         subject = "",
         amount = as.numeric(amount),
         period_start = "",
         period_end = "",
         currency = "GBP",
         status = if_else(as.Date(end_date) > Sys.Date(), "Active", "Closed"),
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
         recipient_country,
         subject,
         status,
         last_updated) %>% 
  unique()

# Add GtR link to projects
ukri_projects_final <- ukri_projects_final %>% 
  mutate(link = paste0("https://gtr.ukri.org/projects?ref=", id))

# Save as R file (to read back in if needed)
saveRDS(ukri_projects_final, file = "Outputs/ukri_projects_final.rds")
# ukri_projects_final <- readRDS("Outputs/ukri_projects_final.rds") 



# 2) Extract NIHR projects ------------------------------------------------

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
         recipient_country = "",
         lead_org_country = ctrynm,
         iati_id = "",
         subject = programme,
         currency = "GBP",
         status = if_else(project_status %in% c("Active", "Contracted"), "Active",
                          if_else(project_status %in% c("Complete"), "Closed", 
                                  if_else(project_status %in% c("Discontinued"), "Cancelled", "Unknown"))),
         period_start = "",
         period_end = "",
         partner_org_name = "",
         partner_org_country = "",
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
         extending_org = coalesce(extending_org, reporting_org),
         status = if_else(activity_status %in% c("Implementation", "Pipeline/identification", "Finalisation"), 
                          "Active", activity_status),
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
         lead_org_name = partner,
         lead_org_country = partner_country,
         partner_org_name,
         partner_org_country,
         iati_id = programme_id,
         Fund = fund,
         Funder, 
         recipient_country = all_countries,
         subject = sector_name,
         status,
         last_updated
  ) 

# Add IATI link to awards
iati_projects_final <- iati_projects_final %>% 
  mutate(link = paste0("https://d-portal.org/ctrack.html#view=act&aid=", id))

# Clean up
rm(request)
rm(response)


# 4) Extract Wellcome projects ------------------------------------------------

# Read in public data on Wellcome Grants
wellcome_grants <- read_excel("Inputs/wellcome grants.xlsx")

# Add missing fields and format Funder/Fund field
wellcome_grants_formatted <- wellcome_grants %>% 
  mutate(status = if_else(Sys.Date() <= `Planned Dates:End Date`, "Active", "Closed"),
         extending_org = "Wellcome Trust",
         currency = "GBP",
         partner_org_name = `Other Implementing Organisations`,
         partner_org_country = `Research Location Countries`,
         recipient_country = "",
         period_start = "",
         period_end = "",
         iati_id = "",
         Funder = if_else(str_detect(`Co-funder`, "National Institute for Health Research"), 
                          "Department of Health and Social Care", `Co-funder`),
         Fund = if_else(Funder == "Department of Health and Social Care",
                        "Global Health Research - Partnerships", "FCDO Research - Programmes"),
         last_updated = quarter_end_date) 

# Select desired variables
wellcome_grants_formatted <- wellcome_grants_formatted %>% 
  select(id = `Internal ID`,
         title = Title, 
         abstract = Description,
         start_date = `Planned Dates:Start Date`,
         end_date = `Planned Dates:End Date`,
         amount = `ODA funding`,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name = `Recipient Org:Name`,
         lead_org_country = `Recipient Org:Country`,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund,
         Funder, 
         recipient_country,
         subject = `Partnership Name`,
         status,
         last_updated
  ) 

# Format date fields for merging
wellcome_grants_final <- wellcome_grants_formatted %>% 
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
         period_start = "",
         period_end = "",
         subject = "",
         status = coalesce(if_else(end_date >= Sys.Date(), "Active", "Closed"), "Unknown"),
         last_updated = quarter_end_date
         ) %>% 
  select(-`No.`, -`Funder programme - name`, -Notes, -file_number, -Currency,
         -`Aims/Objectives`, -`Investigator(s) - name`, -`FCDO programme - name`,
         -`FCDO programme - IATI ID`, -Link)


# 6) BEIS RODA data (spreadsheet)

# Read in BEIS data
roda_extract <- read_excel("Inputs/BEIS RODA - GCRF non-UKRI Q1-21-22.xlsx", sheet = 2)

# Reformat to match other datasetS
roda_extract_final <- roda_extract %>% 
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
         status = if_else(`Status` %in% c("Spend in progress", "Agreement in place", "Delivery", "Finalisation"), "Active",
                          if_else(`Status` %in% c("Completed"), "Closed", 
                                  if_else(`Status` %in% c("Cancelled"), "Cancelled", "Unknown"))),
         period_start = "",
         period_end = "",
         subject = "",
         last_updated = quarter_end_date
  ) %>% 
  select(-`No.`, -Currency, -`Aims/Objectives`, -`Investigator(s) - name`,  -Status)



# 7) Join 5 sources together ----------------------------------------------

all_projects <- rbind(ukri_projects_final, nihr_projects_final, 
                      iati_projects_final, wellcome_grants_final,
                      collated_spreadsheet_data, roda_extract_final)

# Save as R file (to read back in if needed)
saveRDS(all_projects, file = "Outputs/all_projects.rds")
# all_projects <- readRDS("Outputs/all_projects.rds") 


# 7) CHECKING ----
test1 <- filter(all_projects, str_detect(extending_org, "NIHR"))
test2 <- filter(all_projects, str_detect(id, "MR/N006267/1"))


