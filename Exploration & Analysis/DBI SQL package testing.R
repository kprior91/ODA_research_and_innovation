#########################
# DBI package - testing
#########################

library(DBI)
library(tidyverse)

# Read in formatted project dataset
all_projects <- readRDS("Outputs/all_projects.rds") 

# Set database connection
con_live <- DBI::dbConnect(odbc::odbc(),
                           Driver = "SQL Server", 
                           Server   = "hel-sql-120",
                           Database = "ODARIProjects",
                           Trusted_Connection = "True")

dbListTables(con_live)


# Example - reading in SQL script
sql_query <- getSQL("RED programme outputs.sql")
recordSet <- dbSendQuery(con, sql_query)
first_10 <- dbFetch(recordSet, n = -1)

# Test creating and inserting into a table in the database
create_table_query <- "CREATE TABLE TestTable AS SELECT TOP 10 * FROM Country"

insert_query <- paste0("insert into [ODARIProjects].[dbo].Country ",
                       "(project_id, country_type, country) ",
                       "values (99999, 2, 'UK')")

dbSendQuery(con_live, insert_query)

delete_query <- paste0("DELETE FROM [ODARIProjects].[dbo].Country")

dbSendQuery(con_live, delete_query)



