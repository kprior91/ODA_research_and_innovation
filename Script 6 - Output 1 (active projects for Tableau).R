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
  
# Add row ID field to dataset
tableau_projects$row_id <- seq.int(nrow(tableau_projects))


test <- filter(all_projects_tidied, str_detect(id, "300211-6"))
test2 <- filter(country_table_final, str_detect(project_id, "300211-6"))
test3 <- filter(tableau_projects_tidied, str_detect(id, "300211-6"))

# 2) Remove unnecessary country "unknown" records) --------

# Extract project records with unknown or missing country field
unknown_country_projects <- filter(tableau_projects, 
                                   Country %in% c("Unknown") | is.na(Country)) %>% 
  select(row_id, id) %>% 
  unique() %>% 
  mutate(exclude = 1)

# Identify projects that have both a populated and missing country field 
# Restrict to just the populated fields (to keep)
duplicate_country_projects <- filter(tableau_projects, 
                                     !(Country %in% c("Unknown") | is.na(Country))) %>% 
  select(row_id, id) %>% 
  unique() %>% 
  filter(id %in% unknown_country_projects$id) %>% 
  mutate(keep = 1)

# Exclude project records for "Unknown" country when the project has other country info
tableau_projects_tidied <- tableau_projects %>% 
  left_join(unknown_country_projects, by = c("row_id", "id")) %>% 
  left_join(duplicate_country_projects, by = c("row_id", "id")) %>%
  filter(keep == 1 |
         exclude == 1 & !(id %in% duplicate_country_projects$id) |
         is.na(keep) & is.na(exclude)) %>% 
  select(-keep, -exclude) %>% 
  mutate(Country = coalesce(Country, "Unknown"),
         country_type = coalesce(country_type, 1))

rm(tableau_projects)
rm(unknown_country_projects)
rm(duplicate_country_projects)

# 3) Add funder programme names ------------------

# Add FCDO programme ID to dataset
tableau_projects_tidied <- tableau_projects_tidied %>% 
    # remove any text before "-1-" in the FCDO IATI ID
  mutate(fcdo_programme_id = if_else((Funder == "Foreign, Commonwealth and Development Office"
                                     & str_detect(iati_id, "-1-")),
                                     sub(".*-1-", "", iati_id), "")) %>% 
    # remove any FCDO component numbers
  mutate(fcdo_programme_id = sub("-.*", "", fcdo_programme_id))

# Add FCDO programme name to dataset

    # Create vector of FCDO gov funder programme IATI IDs
    gov_funder_iati_ids <- tableau_projects_tidied %>% 
      select(iati_id) %>% 
      filter(str_detect(iati_id, "GB-1-|GB-GOV-1-")) %>% 
      unique()
    
    # Create empty dataframe to hold name extract from IATI
    gov_funder_programme_names <- data.frame()
    
    # Run function over all IATI ids
    for (id in gov_funder_iati_ids$iati_id) {
      print(id)
      data <- extract_iati_activity_name(id)
      gov_funder_programme_names <- rbind(gov_funder_programme_names, data)
    }

# Join funder programme name to main dataset
tableau_projects_tidied <- tableau_projects_tidied %>%
      left_join(gov_funder_programme_names, by = c("iati_id" = "funder_iati_id")) %>% 
      mutate(funder_programme = if_else(extending_org == "Wellcome Trust", subject, funder_programme))


# 4) Apply manual exclusions/rules ----------------------------

# TEMPORARY ***
# Remove IDRC DHSC IATI data (this has been provided by spreadsheet)
tableau_projects_tidied <- tableau_projects_tidied %>% 
  filter(!(Funder == "Department of Health and Social Care" & 
           extending_org == "International Development Research Centre" &
           is.na(amount))
         )

test <- tableau_projects_tidied %>% 
  filter(extending_org == "International Development Research Centre")
 # filter(!(country_type %in% c(1,2)))

# TEMPORARY
# Remove Afghanistan projects (added Sep 21)
tableau_projects_tidied <- tableau_projects_tidied %>% 
  filter(Country != "Afghanistan")


# 5) Write data --------------------------------

# Restrict to active projects for Tableau
tableau_projects_tidied <- tableau_projects_tidied %>% 
  filter(status %in% c("Active", "Unknown")) %>% 
  unique()

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
