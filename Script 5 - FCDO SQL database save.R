# --------------------------------------------------------------- #
# Script 5 
# Script to split output dataset into 4 relational tables for FCDO SQL database
# --------------------------------------------------------------- #

# Read in collated dataset from previous script
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 
org_names_and_locations <- readRDS("Outputs/org_names_and_locations.rds")


# Connect to ODA RI Projects database on development server
# (need to be connected to DFID VPN)

con_live <- DBI::dbConnect(odbc::odbc(),
                            Driver = "SQL Server", 
                            Server   = "hel-sql-120",
                            Database = "ODARIProjects",
                            Trusted_Connection = "True")
  

# View data extracts from current tables

# causes error (?)
# recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Project]") 
# project_first_10 <- dbFetch(recordSet, n = -1)

recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Funder]")
funder_first_10 <- dbFetch(recordSet, n = -1)

recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Organisation]")
org_first_10 <- dbFetch(recordSet, n = -1)

recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Country]")
country_first_10 <- dbFetch(recordSet, n = -1)


# 1) Create master project table ----
project_table <- all_projects_tidied %>% 
  # remove all fields that can have multiple entries for a project
  select(-Funder, -Fund, -iati_id,
         -recipient_country, 
         -lead_org_name, -lead_org_country,
         -partner_org_name, -partner_org_country,
         -period_start, -period_end,
         -last_updated) %>% 
  # limit character fields to 255 chars
  mutate(title = if_else(nchar(title) > 255, substr(title, 1, 255), title),
         abstract = if_else(nchar(abstract) > 5000, substr(abstract, 1, 5000), abstract),
         subject = if_else(nchar(subject) > 255, substr(subject, 1, 255), subject),
         link = if_else(nchar(link) > 255, "", link)) %>%
  # remove special characters
  mutate(title = str_replace_all(title, "‘|’|´", ""),
         title = str_replace_all(title, "´", "'"),
         abstract = str_replace_all(abstract, "‘|’", ""),
         abstract = str_replace_all(abstract, "´", "'")) %>%
  rename(project_id = id) %>% 
  unique()

# find duplicate project ids
duplicates <- project_table %>% 
  group_by(project_id) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  inner_join(project_table, by = "project_id")

# remove duplicate rows (rough)
project_table <- project_table[!duplicated(project_table$project_id), ] 

saveRDS(project_table, file = "Outputs/project_table.rds")


# 2) Create funder table ----

funder_table <- all_projects_tidied %>% 
  select(project_id = id, funder = Funder, 
         fund = Fund, funder_iati_id = iati_id) %>% 
  unique()

saveRDS(funder_table, file = "Outputs/funder_table.rds")


# 3) Create organisation table ----

organisation_table <- org_names_and_locations


# 4) Create country table ----

# Distinguish location and beneficiary countries in main dataset
country_table <- all_projects_tidied %>%
  mutate(beneficiary_country = coalesce(recipient_country, "Unknown"),
         location_country = paste0(coalesce(lead_org_country, "Unknown"), ", ", coalesce(partner_org_country, "Unknown"))) %>%
  select(project_id = id, location_country, beneficiary_country) %>% 
  unique()

# Convert location vs. beneficiary country data to long format
country_table <- country_table %>% 
  gather(key = "country_type", value = "Country", -project_id) %>% 
  mutate(country_type = if_else(country_type == "beneficiary_country", 1, 2),
         Country = str_to_lower(Country))

# Clean country names
country_table_cleaned <- country_table %>%
  mutate(Country = str_replace_all(Country, "\\(the\\)", ""),  # remove (the)
         Country = gsub("[()]", "", Country),                  # remove all parentheses
         Country = str_replace_all(Country, "tanzania, united republic of", "tanzania"),
         Country = str_replace_all(Country, "congo the democratic republic of the|drc|democratic republic of congo", 
                                   "democratic republic of the congo"),
         Country = str_replace_all(Country, "china people's republic of", "china"),
         Country = str_replace_all(Country, "democratic people's republic of korea", "democratic people’s republic of korea")) %>% 
  # standardise separators
  mutate(Country = gsub("\\s*\\([^\\)]+\\)","", Country))

# Convert dataset to long with one row per country entry
country_table_cleaned <- country_table_cleaned %>% 
  separate_rows(Country, sep = ",|;", convert = FALSE) %>% 
  mutate(Country = str_trim(Country)) %>% 
  unique()

# Further country cleaning
country_table_cleaned <- country_table_cleaned %>% 
  mutate(Country = str_trim(Country)) %>%
  mutate(Country = case_when(
    str_detect(Country, "uk|scotland|wales|united kingdom|england|ireland") ~ "united kingdom",
    str_detect(Country, "usa|united states") ~ "united states",
    Country == "us" ~ "united states",
    str_detect(Country, "ivoire") ~ "ivory coast",
    str_detect(Country, "viet") ~ "vietnam",
    str_detect(Country, "lao") ~ "laos",
    str_detect(Country, "bolivia") ~ "bolivia",
    str_detect(Country, "syria") ~ "syria",
    TRUE ~ Country)) %>% 
  unique() 

# Replace country with "unknown" if not recognised against Tableau's accepted list
country_table_final <- country_table_cleaned %>%
  mutate(Country = if_else(Country %in% dac_lookup$country_name, Country, "unknown")) %>% 
  mutate(Country = tools::toTitleCase(Country)) %>%
  unique()

# Remove unecessary unknowns
  country_table_final$row_id <- seq.int(nrow(country_table_final))

  # identify records with more than one country for a country_type
  identify_multiples <- country_table_final %>% 
    group_by(project_id, country_type) %>% 
    summarise(n = n()) %>% 
    filter(n > 1)

  # identify "unknown" records for projects in the dataset above
  identify_unknowns_to_delete <- country_table_final %>% 
    filter(Country == "Unknown") %>% 
    left_join(identify_multiples, by = c("project_id", "country_type")) %>% 
    filter(!is.na(n))
  
  # remove these "unknowns" from the country table
  country_table_final <- country_table_final %>% 
    filter(!(row_id %in% identify_unknowns_to_delete$row_id)) %>% 
    select(-row_id)

  # Save datasets for testing
saveRDS(country_table, file = "Outputs/country_table.rds")
saveRDS(country_table_cleaned, file = "Outputs/country_table_cleaned.rds")
saveRDS(country_table_final, file = "Outputs/country_table_final.rds")


# 5) Delete and write data in database table ----

dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Project]")
dbAppendTable(con_live, "Project", project_table, row.names = NULL)

dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Funder]")
dbAppendTable(con_live, "Funder", funder_table, row.names = NULL)

dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Organisation]")
dbAppendTable(con_live, "Organisation", organisation_table, row.names = NULL)

dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Country]")
dbAppendTable(con_live, "Country", country_table_final, row.names = NULL)


# Disconnect from SQL server 
dbDisconnect(con_live)


