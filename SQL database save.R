# Script to split output dataset into 4 relational tables to save in SQL

# Read in final dataset from scripts 4 and 5
all_projects <- readRDS("Outputs/all_projects.rds") 
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 

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

organisation_table <- all_projects %>% 
  select(project_id = id, 
         lead_org_name, 
         lead_org_country,
         partner_org_name,
         partner_org_country) %>% 
  unique()

organisation_table <- organisation_table %>% 
  gather(organisation_role, organisation_name, -project_id, -lead_org_country, -partner_org_country) %>% 
  mutate(organisation_country = coalesce(lead_org_country, partner_org_country),
         organisation_role = if_else(str_detect(organisation_role, "lead"), 1,2)) %>% 
  select(-lead_org_country, -partner_org_country) %>%
  filter(organisation_name != "") %>% 
  unique()


# 4) Create country table ----

country_table <- all_projects_tidied %>% 
  mutate(country_type = if_else(country_type == "beneficiary_country", 1, 2)) %>% 
  select(project_id = id, 
         country_type,
         country = Country) %>% 
  unique()


# Delete and write data in database table
dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Project]")
dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Funder]")
dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Organisation]")
dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Country]")

dbAppendTable(con_live, "Project", project_table, row.names = NULL)
dbAppendTable(con_live, "Funder", funder_table, row.names = NULL)
dbAppendTable(con_live, "Organisation", organisation_table, row.names = NULL)
dbAppendTable(con_live, "Country", country_table, row.names = NULL)


# Disconnect from database
dbDisconnect(con_live)

# Write output csvs (if required)
# write.csv(project_table, "Outputs/1_project_table.csv")
# write.csv(funder_table, "Outputs/2_funder_table.csv")
# write.csv(organisation_table, "Outputs/3_organisation_table.csv")
# write.csv(country_table, "Outputs/4_country_table.csv")