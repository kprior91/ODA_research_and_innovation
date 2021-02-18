# Extract UK Vaccine Network projects from GtR

if (!("jsonlite" %in% installed.packages())) {
  install.packages("jsonlite")
}
if (!("rvest" %in% installed.packages())) {
  install.packages("rvest")
}
if (!("stringi" %in% installed.packages())) {
  install.packages("stringi")
}

# Load packages -----
library(jsonlite)
library(rvest)
#library(stringi)
library(httr)
library(tidyverse)
library(writexl)
library(readxl)


# 1) Extract UKRI GtR projects that have DFID as a co-funder

# Return DFID organisation IDs

path <- paste0("http://gtr.ukri.org/search/organisation?term=DFID")
request <- GET(url = path)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
data <- response$searchResult
data <- data$results

org_ids <- data$organisation.id

# Define function to extract projects for each DFID organisations 
extract_ukri_projects_by_funder <- function(org_id, page) {
  
  path <- paste0("https://gtr.ukri.org:443/gtr/api/organisations/", org_id, "/projects?s=100&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  projects <- response$project
  return(projects)
  
}


# Create empty dataset to hold projects
ukri_projects_by_funder <- data.frame()

# Extract project info
for (page in c(1,2)) {
  
  for (ID in org_ids) {
    
    print(ID)
    
    data <- extract_ukri_projects_by_funder(ID, page)
    
    ukri_projects_by_funder <- ukri_projects_by_funder %>% 
      rbind(data)
    
  }
}

# Unnest and extract GtR project identifiers
ukri_projects_dfid <- ukri_projects_by_funder %>%
    unnest(cols = identifiers.identifier) %>% 
    select(`GtR ID` = value)


# 1) Extract UKRI projects -------------------------------------------

# Non-FCDO/Newton/GCRF UKRI projects
ukri_projects_ids <- read_xlsx("Inputs/FCDO UKRI awards.xlsx", sheet=1) %>% 
    plyr::rbind.fill(ukri_projects_dfid)


# Define function to extract projects by GtR ID 
extract_ukri_projects_by_id <- function(id) {
  
  path <- paste0("http://gtr.ukri.org/projects?ref=", id)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  data <- response$projectOverview
  
  if(length(data) > 0) {
  
      data <- data$projectComposition
      projects <- data$project
      lead_org <- data$leadResearchOrganisation
      #partner_org <- data$collaborator
      org_roles <- data$organisationRole %>% 
                     rename(org_name = name) %>% 
                     unnest(cols = role) %>% 
                     filter(name %in% c("PARTICIPATING", "COLLABORATOR")) 
      
      if("address.country" %in% names(org_roles)) {
        org_roles <- org_roles %>% 
          filter(address.country == "United Kingdom") 
      } else {
        org_roles <- org_roles %>% 
          filter(name == "anything") # exclude all!
      }
       
        # org_list <-  org_roles %>%
        #                select(partner_name = org_name)
        # 
        # org_list_final <- org_list_final %>% 
        #                      rbind(org_list)
        
        org_roles <- org_roles %>%
                     select(partner_name = org_name) %>% 
                     summarise(partner_name = paste(partner_name, collapse = ", "))
      
      if(length(lead_org$address) > 0) {
        
        project_data <- data.frame(
                title = projects[["title"]],
                status = projects[["status"]],
                gtr_id = projects[["grantReference"]],
                fund = projects[["fund"]],
                lead_org = lead_org[["name"]],
                lead_org_address = lead_org[["address"]])
        
        if(length(org_roles$partner_name) == 0) {
          project_data$partner_org <- ""
        } else {
          project_data$partner_org <- org_roles$partner_name
        }
               
               if("lead_org_address.country" %in% names(project_data)) {
                 project_data <- project_data %>% 
                      rename(lead_org_country = lead_org_address.country) %>% 
                      select(gtr_id, title, fund.start, fund.end, lead_org, lead_org_country, partner_org)
               } else {
                 project_data <- project_data %>% 
                   mutate(lead_org_country = "Unknown") %>% 
                   select(gtr_id, title, fund.start, fund.end, lead_org, lead_org_country, partner_org)                
               }
        
      } else {
      
          project_data <- data.frame(
                         title = projects[["title"]],
                         status = projects[["status"]],
                         gtr_id = projects[["grantReference"]],
                         fund = projects[["fund"]],
                         lead_org = lead_org[["name"]],
                         lead_org_country = "Unknown")
          
          if(length(org_roles$partner_name) == 0) {
            project_data$partner_org <- ""
          } else {
            project_data$partner_org <- org_roles$partner_name
          }
          
          project_data <- project_data %>% 
            select(gtr_id, title, fund.start, fund.end, lead_org, lead_org_country, partner_org) 
      
      }
      
      return(project_data)
  }
  
}


# Create empty dataset to hold projects
ukri_projects_by_id <- data.frame()
org_list_final <- data.frame()

for (ID in ukri_projects_ids$`GtR ID`) {
  
  print(ID)
  
  data <- extract_ukri_projects_by_id(ID)
  
  ukri_projects_by_id <- ukri_projects_by_id %>% 
    rbind(data)
  
}




# Filter those active in 2019-20
ukri_fy_projects <- ukri_projects_by_id %>% 
      filter(fund.start < "2020-03-31",
             fund.end > "2019-04-01") 
           #  lead_org_country == "United Kingdom" | lead_org == "Liverpool School of Tropical Medicine")

# Join on FCDO programme ID and team
ukri_active_projects_red <- ukri_fy_projects %>% 
     left_join(select(ukri_projects_ids, c(1:3,5,7)), by = c("gtr_id" = "GtR ID")) %>% 
      unique()

# Save to Excel
write_xlsx(x = list(`RED UKRI projects` = ukri_active_projects_red), 
           path = "Outputs/RED UKRI projects 2.xlsx")


partners <- select(ukri_active_projects_red, partner_org) %>%  
                  separate_rows(partner_org, sep = ",") %>% 
                  mutate(partner_org = str_trim(partner_org)) %>% 
                  unique() %>% 
                  filter(partner_org != "") %>% 
                  arrange(partner_org)

# Find UK unis
uni_list <- data.frame(lead_org = unique(ukri_fy_projects$lead_org)) %>% 
               arrange(lead_org)

# Write to Excel

cb <- function(df, sep="\t", dec=",", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}
cb(uni_list)




