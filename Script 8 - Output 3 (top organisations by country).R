# Script to pull off lists of most common organisations by country

# Read in dataset of organisations and countries from previous script
org_names_and_locations <- readRDS("Outputs/org_names_and_locations.rds")

# Set country of interest
country <- c("Sierra Leone")

# Output list of organisations based in that country, by number of projects
country_orgs <- org_names_and_locations %>% 
  filter(organisation_country %in% country,
         project_id %in% all_projects_tidied$id)

country_orgs_summarised <- country_orgs %>% 
  group_by(organisation_name) %>% 
  summarise(no_projects = n()) %>% 
  arrange(-no_projects)

