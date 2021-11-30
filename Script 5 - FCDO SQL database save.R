# --------------------------------------------------------------- #
# Script 5 
# Script to split output dataset into 4 relational tables for FCDO SQL database
# --------------------------------------------------------------- #

# Read in collated dataset from previous script
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 
org_names_and_locations <- readRDS("Outputs/org_names_and_locations.rds")


# Connect to ODA RI Projects database on development server

# con_dev <- DBI::dbConnect(odbc::odbc(),
#                       Driver = "SQL Server", 
#                       Server   = "hed-sql-200",
#                       Database = "ODARIProjects",
#                       Trusted_Connection = "False")

  con_live <- DBI::dbConnect(odbc::odbc(),
                            Driver = "SQL Server", 
                            Server   = "hel-sql-120",
                            Database = "ODARIProjects",
                            Trusted_Connection = "True")
  

# View data extracts from current tables

recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Project]")
project_first_10 <- dbFetch(recordSet, n = -1)

recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Funder]")
funder_first_10 <- dbFetch(recordSet, n = -1)

recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Organisation]")
org_first_10 <- dbFetch(recordSet, n = -1)

recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Country]")
country_first_10 <- dbFetch(recordSet, n = -1)


# 1) Create master project table ----
project_table <- all_projects %>% 
  # remove all fields that can have multiple entries for a project
  select(-Funder, -Fund, -iati_id,
         -recipient_country, 
         -lead_org_name, -lead_org_country,
         -partner_org_name, -partner_org_country,
         -period_start, -period_end,
         -last_updated) %>% 
  rename(project_id = id) %>% 
  unique()

# check uniqueness
test <- unique(project_table$id)

# find duplicate project ids
duplicates <- project_table %>% 
  group_by(project_id) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  inner_join(project_table, by = "project_id")

# remove duplicate rows (rough)
project_table <- project_table[!duplicated(project_table$project_id), ] 


# 2) Create funder table ----

funder_table <- all_projects %>% 
  # remove all fields that can have multiple entries for a project
  select(project_id = id, funder = Funder, 
         fund = Fund, funder_iati_id = iati_id) %>% 
  unique()


# 3) Create organisation table ----

organisation_table <- org_names_and_locations


# 4) Create country table ----

# Distinguish location and beneficiary countries in main dataset
country_table <- all_projects_tidied %>%
  mutate(beneficiary_country = recipient_country,
         location_country = paste0(coalesce(lead_org_country, ""), ", ", coalesce(partner_org_country, ""))) %>% 
  select(project_id = id, location_country, beneficiary_country) %>% 
  unique()

# Convert location vs. beneficiary country data to long format
country_table <- country_table %>% 
  gather(key = "country_type", value = "country", -project_id) %>% 
  mutate(country_type = if_else(country_type == "beneficiary_country", 1, 2),
         country = str_to_lower(country))

# Clean country names
country_table_cleaned <- country_table %>%
  mutate(country = str_replace_all(country, "\\(the\\)", ""),  # remove (the)
         country = gsub("[()]", "", country),                  # remove all parentheses
         country = str_replace_all(country, "tanzania, united republic of", "tanzania"),
         country = str_replace_all(country, "congo the democratic republic of the|drc|democratic republic of congo", 
                                   "democratic republic of the congo"),
         country = str_replace_all(country, "china people's republic of", "china"),
         country = str_replace_all(country, "democratic people's republic of korea", "democratic people’s republic of korea")) %>% 
  # standardise separators
  mutate(country = gsub("\\s*\\([^\\)]+\\)","", country))

# Convert dataset to long with one row per country entry
country_table_cleaned <- country_table_cleaned %>% 
  separate_rows(country, sep = ",|;", convert = FALSE) %>% 
  mutate(country = str_trim(country)) %>% 
  unique() %>% 
  filter(country != "")

# Further country cleaning
country_table_cleaned <- country_table_cleaned %>% 
  mutate(country = str_trim(country)) %>%
  mutate(country = case_when(
    str_detect(country, "uk|scotland|wales|united kingdom|england|ireland") ~ "united kingdom",
    str_detect(country, "usa|united states") ~ "united states",
    country == "us" ~ "united states",
    str_detect(country, "ivoire") ~ "ivory coast",
    str_detect(country, "viet") ~ "vietnam",
    str_detect(country, "lao") ~ "laos",
    TRUE ~ country)) %>% 
  unique() 

# Replace country with "unknown" if not recognised against Tableau's accepted list
country_table_final <- country_table_cleaned %>%
  mutate(country = if_else(country %in% dac_lookup$country_name, country, "unknown")) %>% 
  mutate(country = tools::toTitleCase(country)) %>% 
  unique()

# Save datasets for testing
saveRDS(country_table, file = "Outputs/country_table.rds")
saveRDS(country_table_cleaned, file = "Outputs/country_table_cleaned.rds")
saveRDS(country_table_final, file = "Outputs/country_table_final.rds")

# 5) Delete and write data in database table ----

dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Project]")
dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Funder]")
dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Organisation]")
dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Country]")

dbAppendTable(con_live, "Project", project_table, row.names = NULL)
dbAppendTable(con_live, "Funder", funder_table, row.names = NULL)
dbAppendTable(con_live, "Organisation", organisation_table, row.names = NULL)
dbAppendTable(con_live, "Country", country_table_final, row.names = NULL)

# Disconnect from database
dbDisconnect(con_live)
