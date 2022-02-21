# Script to pull off lists of most common organisations by country

# Read in dataset of organisations and countries from previous script
org_names_and_locations <- readRDS("Outputs/org_names_and_locations.rds")
tableau_projects_tidied <- readRDS("Outputs/tableau_projects_tidied.rds") 


# Set country of interest
country <- c("United Kingdom")

project_subset <- tableau_projects_tidied %>% 
  filter(Funder == "Foreign, Commonwealth and Development Office")

# Subset for list of organisations based in specified country, active 
# projects only, specified funder only
country_orgs <- org_names_and_locations %>% 
  filter(organisation_country %in% country,
         project_id %in% project_subset$id)

# Summarise organisations by number of projects
country_orgs_summarised <- country_orgs %>% 
  group_by(organisation_name) %>% 
  summarise(no_projects = n()) %>% 
  arrange(-no_projects)

