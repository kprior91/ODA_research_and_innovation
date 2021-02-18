# Extract UK Vaccine Network projects from GtR

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
               Funder = "BEIS")
      
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

# Extract GTR ID from extract
ukri_projects_by_fund_with_id <- ukri_projects_by_fund %>% 
  unnest(cols = identifiers.identifier) %>% 
  rename(`GtR ID` = value) %>% 
  select(-type)

# Format data to join to other GtR ODA projects
ukri_gcrf_newton_ids <- ukri_projects_by_fund_with_id %>% 
  mutate(`IATI ID` = "", Stage = "", Funder = "BEIS") %>% 
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
      filter(name %in% c("PARTICIPATING", "COLLABORATOR")) 

    # Collapse partner orgs into single records
    if(length(org_roles$name) > 0) {
        
        if("address.country" %in% names(org_roles)) {
          org_roles_summarised <- org_roles %>% 
              select(org_name, address.country) %>%
              summarise(partner_name = paste(org_name, collapse = ", "),
                        partner_country = paste(address.country[!is.na(address.country)], collapse = ", ")) 
          
        } else {
          org_roles_summarised <- org_roles %>% 
            select(org_name) %>%
            summarise(partner_name = paste(org_name, collapse = ", "),
                      partner_country = "")             
        }
        
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

# id = "EP/R013756/1"

# Create empty dataset to hold projects
ukri_projects_by_id <- data.frame()

for (id in ukri_projects_ids$`GtR ID`) {
  
  print(id)
  
  data <- extract_ukri_projects_by_id(id)
  
  ukri_projects_by_id <- ukri_projects_by_id %>% 
    rbind(data)
  
}


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
          status = if_else(as.Date(end_date) > Sys.Date(), "Active", "Closed")) %>% 
   select(id,
          title, 
          abstract,
          start_date,
          end_date,
          amount,
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


test <- filter(ukri_projects_final, str_detect(title, "Immunobridging"))

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
    mutate(id = "",
           Funder = "DHSC",
           Fund = "Global Health Research",
           recipient_country = "",
           lead_org_country = ctry17nm,
           iati_id = "",
           subject = programme,
           partner_org_name = "",
           partner_org_country = "",
           extending_org = "NIHR") %>% 
    select(id, 
           title = project_title,
           abstract = scientific_abstract,
           start_date, end_date,
           amount = award_amount_from_dh,
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
  
  
# 3) Extract IATI projects ------------------------------------------------

# Read in list of IATI activities (from funders and select delivery partners)
iati_activity_list <- readRDS(file = "Outputs/activity_list.rds")
partner_iati_list <- readRDS(file = "Outputs/partner_activity_list.rds")

# Filter for known project level information
iati_projects <- iati_activity_list %>%
                    filter(str_detect(iati_identifier, "204584") | # FCDO Policy Research Fund
                           str_detect(iati_identifier, "UKSA") |   # UKSA awards (GCRF)
                           str_detect(iati_identifier, "NEWT-MO") |   # Met Office awards (Newton)
                           str_detect(iati_identifier, "NEWT-BIS") |  # Other Met Office awards?
                           str_detect(iati_identifier, "GB-GOV-7")     # Defra activities
                           ) %>%   # Component-level FCO activities   
                    filter(flow_type == "ODA",
                           !str_detect(iati_identifier, "GB-CHC-209131-A03172")) %>%  
                    mutate(fund = if_else(str_detect(iati_identifier, "204584"), "204584: Policy Research Fund", 
                                       if_else(fund == "Other", "Unknown", 
                                               if_else(is.na(fund), "Unknown", fund))),
                           activity_description = if_else(str_detect(iati_identifier, "204584"), "", activity_description)) %>%  # remove generic description for Policy Research Fund
                    plyr::rbind.fill(partner_iati_list) # Add British Council data

# Keep required fields
iati_projects_final <- iati_projects %>% 
                            mutate(id = "",
                                   Funder = coalesce(gov_funder, if_else(reporting_org_ref %in% c("GB-CHC-209131", "GB-GOV-1"), "FCDO", 
                                                    if_else(reporting_org_ref == "GB-GOV-7", "Defra", "BEIS"))),
                                   lead_org_country = "",
                                   partner_org_name = partner,
                                   partner_org_country = "",
                                   extending_org = coalesce(extending_org, if_else(reporting_org_ref == "GB-GOV-1", "FCDO", 
                                                           if_else(reporting_org_ref == "GB-CHC-209131", "British Council", 
                                                                   if_else(reporting_org_ref == "GB-GOV-7", "Defra", "UK Space Agency"))))) %>% 
                            select(id,
                                   title = activity_title, 
                                   abstract = activity_description,
                                   start_date,
                                   end_date,
                                   amount,
                                   extending_org,
                                   lead_org_name = partner,
                                   lead_org_country,
                                   partner_org_name,
                                   partner_org_country,
                                   iati_id = iati_identifier,
                                   Fund = fund,
                                   Funder, 
                                   recipient_country = all_countries,
                                   subject = sector_name,
                                   status = activity_status
                                   ) 
                                    

# Clean up
rm(request)
rm(response)


# 4) Join 3 sources together ----------------------------------------------

all_projects <- rbind(ukri_projects_final, nihr_projects_final, iati_projects_final)

# Save as R file (to read back in if needed)
saveRDS(all_projects, file = "Outputs/all_projects.rds")
# all_projects <- readRDS("Outputs/all_projects.rds") 

# Change terminology around award status
all_projects <- all_projects %>% 
                  mutate(status = if_else(status %in% c("Contracted", "Implementation"), "Active", status))


# 5) Extract countries -----------------------------------

# Extract countries mentioned in abstract or title
countries <- countrycode::codelist$country.name.en
countries_string <- paste0(countries, collapse = "|")

countries_in_description <- all_projects %>% 
      mutate(text = paste0(title, " ", abstract)) %>% 
      select(title, text) %>% 
      mutate(countries_abstract = str_extract_all(text, countries_string)) %>% 
      unnest(cols = countries_abstract) %>% 
      unique() %>% 
      group_by(title) %>% 
      summarise(countries_abstract = paste(coalesce(countries_abstract, ""), collapse = ", "))


all_projects_final <- all_projects %>% 
  left_join(countries_in_description, by = "title") %>% 
  mutate(partner_countries = paste0(lead_org_country, ",", partner_org_country),
         countries_abstract = paste0(recipient_country, ",", countries_abstract)) 

# Add ID
all_projects_final$ID <- seq.int(nrow(all_projects_final))

# Distinguish between abstract and partner countries
countries_data <- all_projects_final %>% 
  select(ID, countries_abstract, partner_countries) %>%
  gather(key = "country_type", value = "Country", -ID) %>% 
  right_join(select(all_projects_final, -countries_abstract, -partner_countries), by = "ID") %>% 
  mutate(Country = str_replace_all(Country, "NA", ""),
         Country = str_replace_all(Country, ",,", ","))


# 6) Separate out lead, partner countries ---------------------------------

all_projects_split_country <- countries_data %>%
  select(ID, country_type, Country) %>% 
  mutate(Country = str_replace_all(Country, "Tanzania, United Republic Of,|Tanzania, United Republic of,", "Tanzania,")) %>%
  separate_rows(Country, sep = ",", convert = FALSE) %>%
  mutate(Country = str_trim(Country)) %>% 
  mutate(Country = str_replace_all(Country, c("UK|Scotland|Wales|United kingdom|England|Northern Ireland|UNITED KINGDOM"), "United Kingdom"),
         Country = str_replace_all(Country, c("USA|UNITED STATES|United states"), "United States"),
         Country = str_replace(Country, "N/A", "Unknown"),
         Country = str_replace(Country, "The Netherlands", "Netherlands"),
         Country = str_replace(Country, "The Philippines", "Philippines"),
         Country = if_else(str_detect(Country, "Ivoire"), "Ivory Coast", Country),
         Country = str_replace(Country, "Republic of Congo", "Congo Republic"),
         Country = str_replace(Country, "DRC", "Democratic Republic of the Congo"),
         Country = if_else(str_detect(Country, "Hong Kong"), "Hong Kong", Country),
         Country = str_replace_all(Country, "é", "e")) %>% 
  unique() %>% 
  filter(!(Country %in% c("", "NA", "Unknown")) & !is.na(Country)) %>% 
  arrange(ID)

# Read in DAC country lookup and Tableau accepted country list
dac_lookup <- read_xlsx("Inputs/Country lookup - Tableau and DAC Income Group.xlsx")

# Check countries that are unmatched (this information will be lost)
unmatched_countries <- all_projects_split_country %>%
  filter(!(Country %in% dac_lookup$country_name)) %>% 
  select(ID, Country) %>% 
  unique()

# Replace country with "Unknown" if not recognised against Tableau's 
# accepted list
all_projects_split_country <- all_projects_split_country %>%
  mutate(Country = if_else(Country %in% dac_lookup$country_name, Country, "Unknown")) %>% 
  unique()


# Join countries to project data

all_projects_final <- countries_data %>% 
  rename(all_countries = Country) %>% 
 # select(-country_type) %>% 
  left_join(all_projects_split_country, by = c("ID", "country_type")) %>% 
  mutate(date_refreshed = Sys.Date())


# Write to RDS ----
saveRDS(all_projects_final, "Outputs/all_projects_final.rds")
# all_projects_final <- readRDS("Outputs/all_projects_final.rds") 

# Write data to EC google drive -----

# Authorise googlesheets4 to view and manage sheets on EC Drive
# (using saved authentication token in folder)

ODA_RI_url <- "https://docs.google.com/spreadsheets/d/1ByVBWb3LNSoqAUzKlddd537DleQ-y9MINwY_SuuZEbY/edit#gid=2024786204"
results <- as_sheets_id(ODA_RI_url)

results_sheet <- sheet_write(all_projects_final,
                             ss = results,
                             sheet = "ODA_RI_projects")



# --------------------------------------------------------------------------
# testing
test <- filter(all_projects_final, extending_org == "British Council")

test <- filter(all_projects_final, str_detect(title, "immunobridging"))

