### Script to read in the Open Data Service's version of the IATI datastore as an
### SQL database for analysis 

# install.packages("RSQLite")

library("RSQLite")

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="C:/Users/clegge/Downloads/iati.sqlite/iati.sqlite")

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- 

# Extract IATI IDs for Chevening activities
chevening_iati_ids <- gov_list_base$iati_identifier 

test <- data.frame()

for (id in chevening_iati_ids) {
# Extract transactions for Chevening activities

print(id)
extract <- dbGetQuery(conn=con, 
                   statement = paste0("SELECT * FROM activity WHERE iatiidentifier = ", id, sep=""))

test <- rbind(test, extract)

}

test <- dbGetQuery(conn=con, 
                   statement = "SELECT * FROM transaction_breakdown
                                WHERE _link_activity = '40372'")

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

View(tables)


# Extract information on one FCDO activity:

# EXAMPLE
test <- dbGetQuery(conn=con,statement = "SELECT * FROM activity LIMT 5")
test <- dbGetQuery(conn=con,statement = "SELECT * FROM description LIMIT 5")

# TEST ONE ACTIVITY
partner_activity_id <- "GB-CHC-287287-DFID-PEDL"

activity_detail <- dbGetQuery(conn=con, 
                              statement = paste0("SELECT * FROM activity
                                                  WHERE iatiidentifier LIKE '%", partner_activity_id, "%'"))

activity_link <- activity_detail$`_link_activity`

activity_description <- dbGetQuery(conn=con, 
                              statement = paste0("SELECT * FROM description
                                                  WHERE _link_activity = ", activity_link))

activity_partners <- dbGetQuery(conn=con, 
                                   statement = paste0("SELECT * FROM participatingorg
                                                  WHERE _link_activity = ", activity_link))

activity_transactions <- dbGetQuery(conn=con, 
                                statement = paste0("SELECT * FROM transaction_breakdown
                                                     WHERE _link_activity = ", activity_link))

activity_transactions_country <- dbGetQuery(conn=con, 
                                    statement = paste0("SELECT * FROM transaction_recipientcountry
                                                     WHERE _link_activity = ", activity_link))

activity_budget <- dbGetQuery(conn=con, 
                                            statement = paste0("SELECT * FROM budget
                                                     WHERE _link_activity = ", activity_link))

activity_disbursement <- dbGetQuery(conn=con, 
                              statement = paste0("SELECT * FROM planneddisbursement
                                                     WHERE _link_activity = ", activity_link))

activity_country <- dbGetQuery(conn=con, 
                                    statement = paste0("SELECT * FROM recipientcountry
                                                     WHERE _link_activity = ", activity_link))

activity_location <- dbGetQuery(conn=con, 
                               statement = paste0("SELECT * FROM location
                                                     WHERE _link_activity = ", activity_link))



