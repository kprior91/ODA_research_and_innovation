
### Set end of quarter data for update ----

quarter_end_date <- as.Date("2023-09-30")


### Check and install packages ----

packages <- data.frame(installed.packages())

if (!("jsonlite" %in% packages$Package)) {
  install.packages("jsonlite")
}
if (!("data.table" %in% packages$Package)) {
  install.packages("data.table")
}
if (!("httr" %in% packages$Package)) {
  install.packages("httr")
}
if (!("tidyverse" %in% packages$Package)) {
  install.packages("tidyverse")
}
if (!("dplyr" %in% packages$Package)) {
  install.packages("dplyr")
}
if (!("readxl" %in% packages$Package)) {
  install.packages("readxl")
}
if (!("writexl" %in% packages$Package)) {
  install.packages("writexl")
}
if (!("googlesheets4" %in% packages$Package)) {
  install.packages("googlesheets4")
}
if (!("gargle" %in% packages$Package)) {
  install.packages("gargle")
}
if (!("openxlsx" %in% packages$Package)) {
  install.packages("openxlsx")
} # for adding hyperlinks and formatting to output Excel reports
if (!("DBI" %in% packages$Package)) {
  install.packages("DBI")
} # for read/writing to Excel database
if (!("odbc" %in% packages$Package)) {
  install.packages("odbc")
} 
if (!("countrycode" %in% packages$Package)) {
  install.packages("countrycode")
} 
if (!("testthat" %in% packages$Package)) {
  install.packages("testthat")
} 
if (!("lubridate" %in% packages$Package)) {
  install.packages("lubridate")
} 

library(jsonlite)
library(httr)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(googlesheets4)
library(gargle)
library(openxlsx)
library(DBI)
library(odbc)
library(countrycode)
library(testthat)
library(data.table)
library(lubridate)

### Adding in the API Key ----

API_KEY = "b2ebb1258b6e43858457db5a48a0a162"
authentication = add_headers(`Ocp-Apim-Subscription-Key` = API_KEY)


### Read in reference data ----

# 1) GRID research institution lookup
grid_institutes <- read.csv("Inputs/GRID tables/institutes.csv") %>% 
  select(grid_id, name) %>% 
  mutate(name = str_to_lower(name)) %>% 
  unique()  %>% 
  # Remove common organisation names
  filter(!(name %in% c("ministry of health", "ministry of public health")))

grid_addresses <- read.csv("Inputs/GRID tables/addresses.csv") %>% 
  select(grid_id, country, country_code) %>% 
  unique()

grid_aliases <- read.csv("Inputs/GRID tables/aliases.csv")

# Country names
countries <- rev(countrycode::codelist$country.name.en)   # reverse to list "Nigeria" before "Niger" for later string detection
countries_string <- paste0(str_to_lower(countries), collapse = "|")


# 2) DAC country lookup and Tableau accepted country list
dac_lookup <- readxl::read_xlsx("Inputs/Country lookup - Tableau and DAC Income Group.xlsx") %>% 
  mutate(country_name = str_to_lower(country_name))



### Input data ----

# FCDO partner IATI activities (to add manually as not linked) 
# see the R script "checking_linking_RED_data.R" for how I collated this spreadsheet
unlinked_partner_iati_activity_ids <- readxl::read_xlsx("Inputs/IATI partner activities_kp.xlsx", sheet=1)

# UKRI non GCRF/Newton project IDs
# ukri_ooda_projects_ids <- readxl::read_xlsx("Inputs/UKRI non GCRF-Newton projects.xlsx", sheet=1) %>% 
#  mutate(recipient_country = NA_character_)

# Wellcome ODA grant data
wellcome_grants <- readxl::read_excel("Inputs/2023-07-26 Wellcome ODA.xlsx")

# BEIS RODA GCRF/Newton extracts
roda_extract_gcrf <- readxl::read_excel("Inputs/BEIS_GCRF_MODARI_Q4_2022-2023.xlsx", sheet = 1)
roda_extract_newton <- readxl::read_excel("Inputs/BEIS_NF_MODARI_Q4_2022-2023.xlsx", sheet = 1)

# DEFRA ODA grant data
defra_grants <- readxl::read_excel("Inputs/DEFRA_MODARI_data_request.xlsx")

# DHSC Global Health Security projects
# dhsc_ghs_projects <- readxl::read_excel("Inputs/MODARI award data - GHS (GAMRIF and UKVN).xlsx", sheet = 1)

# DHSC/FCDO core contribution programmes/components 
# (these are out of scope of IATI)
gov_non_iati_programmes <- readxl::read_excel("Inputs/FCDO core contribution programmes (with beneficiary countries)_kp.xlsx")

# DHSC co-funded projects
DHSC_cofund <- readxl::read_excel("Inputs/UKRI-DHSC co-funded awards.xlsx")

# FCDO RED programmes Location data. Will need to compare countries here to countries found through IATI

RED_AMP_location <- readxl::read_excel("Inputs/byProject_RED_endMay23_Programmes with geography data.xlsx")
RED_AMP_location <- RED_AMP_location %>% 
  group_by(ProjectId) %>% 
  rename(Country_Name = Name) %>%
  summarise(Country_Name = paste(coalesce(Country_Name, ""), collapse = ", "))

# RED_AMP_location <- RED_AMP_location %>% 
#   select(Programme, Name) %>%
#   unique() %>%
#   group_by(Programme) %>% 
#   rename(Country_Name = Name) %>%
#   summarise(Country_Name = paste(coalesce(Country_Name, ""), collapse = ", "))
# write.xlsx(RED_AMP_location, file = "C:/Users/KimPrior/OneDrive - FCDO/Documents/RED bits/Management Info/UKCDR Climate Tracker commission/RED_countries.xlsx")

### Functions -----

### Country lookup of organisation ###

org_country_lookup <- function(org_name) {
  
  # Look up country from GRID database
  country_lookup <- data.frame(name = str_to_lower(org_name)) %>%
    
    # Join on GRID database
    left_join(grid_institutes, by = "name") %>% 
    left_join(grid_addresses, by = "grid_id") %>% 
    select(name, grid_country = country) %>% 
    
    # Extract any countries in name
    mutate(name_country = str_extract_all(name, countries_string)) %>% 
    unnest(cols = name_country, keep_empty = TRUE) %>% 
  
    # Coalesce country results
    mutate(final_country = coalesce(grid_country, name_country))
  
    result <- str_to_title((country_lookup$final_country)[1])
  
  return(result)
}


### IATI ###

# Function to match IATI country code to name 

countrycode_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Country.csv")


regioncode_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Region.csv")


IATI_org_ID_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/IATIOrganisationIdentifier.csv")

# KP - i'm not quite sure what this function is doing, i've got it to return a country code, but there's no country name

# country_code_to_name <- function(country_code) {
#   
#   # check if input is a valid 2-digit country code
#   if(is.na(country_code) | nchar(country_code) < 2) { country_name <- NA }
#   
#   else {
#       path <- paste0("https://api.iatistandard.org/datastore/activity/select?",
#                      'q=recipient_country_code:',
#                      country_code,
#                      '&wt=json',
#                      '&fl=recipient_country_code')
#       request <- GET(url = path, authentication)
#       response <- content(request, as = "text", encoding = "UTF-8")
#       response <- (fromJSON(response, flatten = TRUE))$response$docs
#       
#       # Check whether a name has been found
#       if(length(response) > 0) {
#         country_name <- response$name
#       } else {
#         country_name <- NA
#       }
#   }
#   return(country_name)
# }

# Extract 5-digit OECD sector codes

sector_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Sector.csv")

# Function to make qstring
make_org_qstring <- function(iati_name) {
  a <- sapply(iati_name, function(x){
    paste0("%22",x,"%22")
  })
  paste0("q=reporting_org_ref:(",paste(a, collapse = " "),")")
}

make_activity_qstring <- function(iati_name) {
  a <- sapply(iati_name, function(x){
    paste0("%22",x,"%22")
  })
  paste0("q=iati_identifier:(",paste(a, collapse = " "),")")
}

# KP - i'm getting a col called default_flow_type_code, is that ok for dataset joining? need to think about 1000 rows max returned####

# Function to extract IATI activity info from activity ID
# iati_activity_extract <- function(activity_id) {
#   
#   # Reformat ID if it contains spaces (for API)
#   activity_id <- str_replace_all(activity_id, " ", "%20")
#   
#   path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
#                  'q=iati_identifier:"',
#                  activity_id,
#                  '"&wt=json',
#                  "&fl=other_identifier*,reporting_org*,location*,default_flow_type*,activity_date*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*"
#   )
#   request <- GET(url = path, authentication)
#   response <- content(request, as = "text", encoding = "UTF-8")
#   response <- fromJSON(response, flatten = TRUE) 
#   new_data <- response$response$docs
#   
#   # Ensure "default flow type" field exists for joining datasets
#   if("default_flow_type.name" %in% names(new_data)) {
#     new_data <- new_data %>% 
#       mutate(default_flow_type = default_flow_type.name) %>% 
#       select(-default_flow_type.name, -default_flow_type.code)
#   } 
#   
#   return(new_data)
# }


iati_activity_extract <- function(page, activity_id) {
  
  # Reformat ID if it contains spaces (for API)
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=iati_identifier:',
                 activity_id,
                 '&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,reporting_org*,location*,sector_code*,default_flow_type*,recipient_country_code,recipient_region_code,activity_date*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  } 
  
  return(new_data)
}


# Function to extract IATI activity IDs for a specified org code

org_activity_extract <- function(page, org_code) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 org_code,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,recipient_country_code,recipient_region_code,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  }
  return(new_data)

}

# Function to extract IDRC

org_activity_extract_IDRC <- function(page, IDRC) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 IDRC,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,recipient_country_code,recipient_region_code,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  }
  flat_data_list = list() 
  flat_data_index = 1
  for(i in 1:nrow(new_data)){
    row = new_data[i,]
    participating_org_cols = which(startsWith(names(new_data),"participating_org"))
    participating_org_data = row[,participating_org_cols]
    row[,participating_org_cols] = NULL
    for(j in 1:length(participating_org_data[1,1][[1]])){
      org_row = row
      for(k in 1:length(participating_org_cols)){
        if(!is.null(participating_org_data[1,k][[1]])){
          org_row[1,names(participating_org_data)[k]] = participating_org_data[1,k][[1]][j]
        }
      }
      flat_data_list[[flat_data_index]] = org_row
      flat_data_index = flat_data_index + 1
    }
  }
  return(rbindlist(flat_data_list, fill=T))
  
}

# Function to extract COUNTRIES from transactions for a specified IATI activity ID
# the latest transactions_extract_country function is below, modified as my batches code don't work anymore

# transactions_extract_country <- function(page, activity_id) {
#   rows <- 1000
#   start <- (page - 1) * rows
#   path <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
#                  'q=iati_identifier:',
#                  activity_id,
#                  '&wt=json',
#                  '&rows=', rows,
#                  '&start=', start,
#                  '&fl=iati_identifier,value*,transaction_date*,transaction_recipient_country_code,description*,currency*')
#   request <- GET(url = path, authentication)
#   message(request$status_code)
#   response <- content(request, as = "text", encoding = "UTF-8")
#   response <- fromJSON(response, flatten = TRUE)
#   new_data <- response$response$docs
#   numb_data <- response$response$numFound
#   # Condition to check when to stop
#   
#   if(start >= numb_data) {
#     return(NULL)
#   }
#   return(new_data)
#   
# }

transactions_extract_country <- function(page, activity_id) {
  rows <- 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
                 'q=iati_identifier:"',
                 activity_id,
                 '"&wt=json',
                 '&rows=', rows,
                 '&start=', start,
                 '&fl=iati_identifier,value*,transaction_date*,transaction_recipient_country_code,description*,currency*')
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE)
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  # Condition to check when to stop
  
  if(start >= numb_data) {
    return(NULL)
  }
  return(new_data)
  
}

# Function to extract RECIPIENTS from transactions for a specified IATI activity ID
# the latest transactions_extract_recipient function is below, modified as my batches code don't work anymore

# transactions_extract_recipient <- function(page, activity_id) {
#   rows <- 1000
#   start <- (page - 1) * rows
#   path <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
#                  'q=iati_identifier:',
#                  activity_id,
#                  '&wt=json',
#                  '&rows=', rows,
#                  '&start=', start,
#                  '&fl=iati_identifier,value*,transaction_date*,transaction_receiver_org_narrative,transaction_receiver_org_ref,description*,currency*')
#   request <- GET(url = path, authentication)
#   message(request$status_code)
#   response <- content(request, as = "text", encoding = "UTF-8")
#   response <- fromJSON(response, flatten = TRUE)
#   new_data <- response$response$docs
#   numb_data <- response$response$numFound
#   # Condition to check when to stop
#   
#   if(start >= numb_data) {
#     return(NULL)
#   }
#   return(new_data)
#   
# }


transactions_extract_recipient <- function(page, activity_id) {
  rows <- 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
                 'q=iati_identifier:"',
                 activity_id,
                 '"&wt=json',
                 '&rows=', rows,
                 '&start=', start,
                 '&fl=iati_identifier,value*,transaction_date*,transaction_receiver_org_narrative,transaction_receiver_org_ref,description*,currency*')
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE)
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  # Condition to check when to stop

  if(start >= numb_data) {
    return(NULL)
  }
  return(new_data)

}


# Function to extract activity names from an IATI activity ID

extract_iati_activity_name <- function(page, activity_id) {
  rows <- 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=iati_identifier:',
                 activity_id,
                 '&wt=json',
                 '&rows=', rows,
                 '&start=', start,
                 "&fl=iati_identifier,title*")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE)
  new_data <- response$response$docs
  numb_data <- response$response$numFound

  if(start >= numb_data) {
    return(NULL)
  }
  return(new_data)

}



### UKRI ###

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

extract_org_country <- function(org_id) {
  
  path <- paste0("http://gtr.ukri.org/organisation/", org_id)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  org_name <- ((response$organisationOverview)$organisation)$name
  
  # Look up country from UKRI GtR 
  org_address <- ((response$organisationOverview)$organisation)$address
  
  if("country" %in% names(org_address)) {
    org_country_ukri <- org_address$country
    
  } else {
    org_country_ukri <- "Unknown"
  }
  
  # If unknown use other generic lookup function
  if(org_country_ukri == "Unknown") {
    org_country <- org_country_lookup(org_name)
    
  } else {
    org_country <- org_country_ukri
  }
  
  return(org_country)
}


# 4 - Master function to extract UKRI project data by ID

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
  
  # Create blank org table for output
  org_table <- data.frame()
  
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
        staff_org_country_data <- staff_org_data %>% 
          mutate(person_current_org_country = map(person_current_org_id, extract_org_country)) %>%
          unnest(col = person_current_org_country) 
        
        # Collapse staff partner orgs and countries into single records
        if(length(staff_org_country_data$person_current_org_name) > 0) {
          
          # Keep staff org names and countries to output
          org_table <- staff_org_country_data %>% 
                  mutate(project_id = projects[["grantReference"]],
                         organisation_role = 2) %>% 
                  select(project_id,
                         organisation_role,
                         organisation_name = person_current_org_name,
                         organisation_country = person_current_org_country)
          
          # Collapse org names and locations
          staff_org_names <- staff_org_country_data %>% 
            select(person_current_org_name) %>% 
            unique() %>% 
            summarise(partner_name = paste(person_current_org_name, collapse = ", "))
          
          staff_org_countries <- staff_org_country_data %>% 
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
        mutate(partner_org_name = NA_character_,
               partner_org_country = NA_character_)
    }
    
    # Write lead org name and country to file
    org_table <- org_table %>% 
      rbind(select(project_data,
                   project_id = gtr_id,
                   organisation_name = lead_org_name,
                   organisation_country = lead_org_country) %>% 
            mutate(organisation_role = 1))
    
    # Keep desired fields
    project_data <- project_data %>% 
      select(gtr_id, title, abstract, fund.start, fund.end, amount = fund.valuePounds, 
             extending_org = fund.funder.name,
             lead_org_name, lead_org_country, partner_org_name, partner_org_country, 
             status, last_updated) 
    
    
  } else {
    
    # If no data available to extract, return empty dataframe
    project_data <- data.frame()
  }
  
  return(list(project_data, org_table))
}


