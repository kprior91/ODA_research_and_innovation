# Script to split output dataset into 3 relational tables for SQL

library(DBI)
library(tidyverse)

# Read in final dataset from scripts 4 and 5
all_projects_transactions <- readRDS(file = "Outputs/all_projects_transactions.rds")
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 


# Connect to ODA RI Projects database on development server

con_dev <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server", 
                      Server   = "hed-sql-200",
                      Database = "ODARIProjects",
                      Trusted_Connection = "False")

con_live <- DBI::dbConnect(odbc::odbc(),
                          Driver = "SQL Server", 
                          Server   = "hel-sql-120",
                          Database = "ODARIProjects",
                          Trusted_Connection = "True")

dbListTables(con_live)


# Run SQL query to extract output data

# Example - reading in SQL script
sql_query <- getSQL("RED programme outputs.sql")
recordSet <- dbSendQuery(con, sql_query)
first_100 <- dbFetch(recordSet, n = -1)

# Example - direct query
recordSet <- dbSendQuery(con_live, "SELECT TOP 10 * FROM [Country]")
first_10 <- dbFetch(recordSet, n = -1)



# Test creating a table in the database

create_table_query <- "CREATE TABLE TestTable AS SELECT TOP 10 * FROM Country"

insert_query <- paste0("insert into [ODARIProjects].[dbo].Country ",
                       "(project_id, country_type, country) ",
                       "values (99999, 2, 'UK')")

dbSendQuery(con_live, insert_query)

delete_query <- paste0("DELETE FROM [ODARIProjects].[dbo].Country WHERE project_id = '99999'")

dbSendQuery(con_live, delete_query)


# 1) Create master project table ----
project_table <- all_projects_transactions %>% 
  # remove all fields that can have multiple entries for a project
  select(-Funder, -Fund, -iati_id,
         -recipient_country, 
         -lead_org_name, -lead_org_country,
         -partner_org_name, -partner_org_country) %>% 
  unique()

# check uniqueness
test <- unique(project_table$id)

# find duplicate project ids
duplicates <- project_table %>% 
  group_by(id) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  inner_join(project_table, by = "id")

# remove duplicate rows (rough)
project_table <- project_table[!duplicated(project_table$id), ] 

write.csv(duplicates, "Outputs/duplicates.csv")

write.csv(project_table, "Outputs/1_project_table.csv")


# 2) Create funder table ----

funder_table <- all_projects_transactions %>% 
  # remove all fields that can have multiple entries for a project
  select(id, Funder, Fund, iati_id) %>% 
  unique()

write.csv(funder_table, "Outputs/2_funder_table.csv")


# 3) Create organisation table ----

organisation_table <- all_projects_transactions %>% 
  select(project_id = id, 
         lead_org_name, 
         lead_org_country,
         partner_org_name,
         partner_org_country) %>% 
  unique()

write.csv(organisation_table, "Outputs/3_organisation_table.csv")


# 4) Create country table ----

country_table <- all_projects_tidied %>% 
  mutate(country_type = if_else(country_type == "beneficiary_country", 1, 2)) %>% 
  select(project_id = id, 
         country_type,
         country = Country) %>% 
  unique()

write.csv(country_table, "Outputs/4_country_table.csv")

# Overwrite data in country table
dbSendQuery(con_live, "DELETE FROM [ODARIProjects].[dbo].[Country]")
dbAppendTable(con_live, "Country", country_table, row.names = NULL)


# Disconnect from database
dbDisconnect(con_live)
