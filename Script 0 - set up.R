
### Set end of quarter data for update ----

quarter_end_date <- as.Date("2021-09-30")


### Check and install packages ----

packages <- data.frame(installed.packages())

if (!("jsonlite" %in% packages$Package)) {
  install.packages("jsonlite")
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
countries <- countrycode::codelist$country.name.en
countries_string <- paste0(str_to_lower(countries), collapse = "|")


# 2) DAC country lookup and Tableau accepted country list
dac_lookup <- read_xlsx("Inputs/Country lookup - Tableau and DAC Income Group.xlsx") %>% 
  mutate(country_name = str_to_lower(country_name))


### Input data ----

# FCDO partner IATI activities (to add manually as not linked) 
iati_activity_ids <- read_xlsx("Inputs/IATI partner activities.xlsx", sheet=1)

# UKRI non GCRF/Newton project IDs
ukri_projects_ids <- read_xlsx("Inputs/UKRI non GCRF-Newton projects.xlsx", sheet=1)

# Wellcome ODA grant data
wellcome_grants <- read_excel("Inputs/wellcome grants.xlsx")

# BEIS RODA GCRF/Newton extracts
roda_extract_gcrf <- read_excel("Inputs/BEIS_GCRF_MODARI_Q2_2021-2022.xlsx", sheet = 1)
roda_extract_newton <- read_excel("Inputs/BEIS_NF_MODARI_Q2_2021-2022.xlsx", sheet = 1)


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

# Function to extract 5-digit OECD sector codes

sector_extract <- function(page, sector_list) {
  path <- paste0("https://iati.cloud/api/sectors/?fields=category,url,name,code&format=json&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  
  # Condition to check when 5-digit codes stop being returned
  if(!("category" %in% names(response$results))) {
    sector_list <- rbind(sector_list, response$results)
  } else {
    sector_list <- sector_list
  }
  return(sector_list)
}

# Function to extract IATI activity info from activity ID
iati_activity_extract <- function(activity_id) {
  
  # Reformat ID if it contains spaces (for API)
  activity_id <- str_replace_all(activity_id, " ", "%20")
  
  path <- paste0("https://iati.cloud/api/activities/?iati_identifier=", activity_id, "&format=json&fields=other_identifier,reporting_org,location,default_flow_type,activity_date,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity&page_size=20")
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results
  
  # Ensure "default flow type" field exists for joining datasets
  if("default_flow_type.name" %in% names(new_data)) {
    new_data <- new_data %>% 
      mutate(default_flow_type = default_flow_type.name) %>% 
      select(-default_flow_type.name, -default_flow_type.code)
  } 
  
  return(new_data)
}


# Function to extract IATI activity IDs for a specified org code

org_activity_extract <- function(page, org_code, org_activity_list) {
  path <- paste0("https://iati.cloud/api/activities/?format=json&reporting_org_identifier=", org_code, "&fields=iati_identifier,other_identifier,activity_date,reporting_org,sector,location,default_flow_type,budget,policy_marker,activity_status,hierarchy,title,description,participating_org,related_activity,tag&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results
  
  # Ensure "default flow type" field exists for joining datasets
  if("default_flow_type.name" %in% names(new_data)) {
    new_data <- new_data %>% 
      mutate(default_flow_type = default_flow_type.name) %>% 
      select(-default_flow_type.name, -default_flow_type.code)
  } 
  
  results <- rbind(org_activity_list, new_data)
  
  return(results)
}


# Function to extract transactions for a specified IATI activity ID
transactions_extract <- function(activity_id, page, output_data) {
  
  # Reformat ID if it contains spaces (for API)
  activity_id <- str_replace_all(activity_id, " ", "%20")
  
  path <- paste0("https://iati.cloud/api/transactions/?iati_identifier=", activity_id, "&fields=value,transaction_date,description,currency,receiver_organisation&format=json&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results
  
  if(length(new_data) > 0) {
    output <- plyr::rbind.fill(output_data, new_data)
  } else {
    output <- output_data
  }
  
  return(output)
}


# Function to extract programme names from IATI

extract_iati_activity_name <- function(activity_id) {
  
  # Reformat ID if it contains spaces (for API)
  activity_id <- str_replace_all(activity_id, " ", "%20")
  
  path <- paste0("https://iati.cloud/api/activities/?iati_identifier=", activity_id, "&format=json&fields=title")
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results 
  
  if(length(new_data) > 0) {
    new_data <- new_data %>% 
      unnest(col = title.narrative) %>% 
      select(funder_iati_id = iati_identifier, funder_programme = text)
  } else {
    new_data <- data.frame()
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
        mutate(partner_org_name = "",
               partner_org_country = "")
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


