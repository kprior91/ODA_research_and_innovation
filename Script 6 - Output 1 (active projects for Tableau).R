# --------------------------------------------------------------- #
# Script 6 
# Format dataset for Tableau map
# --------------------------------------------------------------- #

# Read in project and country datasets from previous script
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds")
country_table_final <- readRDS("Outputs/country_table_final.rds")

# 1) Join countries to project data ----

tableau_projects <- all_projects_tidied %>% 
  left_join(country_table_final, by = c("id" = "project_id"))
  
# 2) Remove unnecessary country "unknown" records --------

# Identify projects with no country info whatsoever
project_country_unknowns <- filter(tableau_projects, Country == "Unknown") %>%
  select(id, country_type) %>%
  unique() %>%
  group_by(id) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

# Delete any other unknown records - these don't need displaying in Tableau
tableau_projects <- tableau_projects %>% 
  filter(id %in% project_country_unknowns$id | Country != "Unknown") 


# 3) Add funder programme names ------------------

# Add FCDO/DHSC programme names to dataset

    # Create vector of gov funder programme IATI IDs
    # (takes 10-15 mins to run)
    gov_funder_iati_ids <- tableau_projects %>% 
      select(Funder, iati_id) %>% 
      filter(str_detect(iati_id, "GB-1-|GB-GOV-1-|GB-GOV-10-")) %>% # filter FCDO and DHSC IDs only
        # remove any FCDO component numbers
        mutate(programme_iati_id = if_else(Funder == "Foreign, Commonwealth and Development Office" &
                                   substr(iati_id, nchar(iati_id)-3, nchar(iati_id)-3) == "-",
                                   substr(iati_id, 1, nchar(iati_id)-4), iati_id)) %>% 
        # Add programme names on
        mutate(funder_programme = map(programme_iati_id, extract_iati_activity_name)) %>% 
        mutate(funder_programme = unlist(funder_programme)) %>% 
       select(-Funder, -programme_iati_id) %>% 
       unique()

  
# Join funder programme name to main dataset
tableau_projects_tidied <- tableau_projects %>%
      left_join(gov_funder_iati_ids, by = "iati_id") %>% 
      mutate(funder_programme = if_else(extending_org == "Wellcome Trust", subject, funder_programme))


# 4) Apply manual exclusions/rules ----------------------------

# TEMPORARY ***
# Remove IDRC DHSC IATI data (this has been provided instead by spreadsheet)
tableau_projects_tidied <- tableau_projects_tidied %>% 
  filter(!(Funder == "Department of Health and Social Care" & 
           extending_org == "International Development Research Centre" &
           is.na(amount))
         )

# 5) Write data --------------------------------

# Restrict to active projects for Tableau
tableau_projects_tidied <- tableau_projects_tidied %>% 
  filter(status %in% c("Active", "Unknown")) %>% 
  unique()

tableau_projects_tidied <- tableau_projects_tidied %>% 
  mutate(Fund = case_when(
    Fund == "Global Challenges Research Fund (GCRF)" ~ "BEIS - Global Challenges Research Fund (GCRF)",
    Fund == "Newton Fund" ~ "BEIS - Newton Fund",
    Fund == "Chevening Scholarships" ~ "FCDO - Chevening Scholarships",
    Fund == "Global Health Research - Partnerships" ~ "DHSC - Global Health Research - Partnerships",
    Fund == "Global Health Research - Programmes" ~ "DHSC - Global Health Research - Programmes",
    Fund == "Global Health Security - GAMRIF" ~ "DHSC - Global Health Security - GAMRIF",
    Fund == "Global Health Security - UK Vaccine Network" ~ "DHSC - Global Health Security - UK Vaccine Network",
    Fund == "International Climate Finance (ICF)" ~ "BEIS - International Climate Finance (ICF)",        
    TRUE ~ Fund
  ))

# Write to RDS 
saveRDS(tableau_projects_tidied, "Outputs/tableau_projects_tidied.rds")
# tableau_projects_tidied <- readRDS("Outputs/tableau_projects_tidied.rds") 

# Write data to EC google drive 
# Authorise googlesheets4 to view and manage sheets on EC Drive
# (using saved authentication token in folder)

ODA_RI_url <- "https://docs.google.com/spreadsheets/d/1ByVBWb3LNSoqAUzKlddd537DleQ-y9MINwY_SuuZEbY/edit#gid=2024786204"
results <- as_sheets_id(ODA_RI_url)

results_sheet <- sheet_write(tableau_projects_tidied,
                             ss = results,
                             sheet = "ODA_RI_projects")

