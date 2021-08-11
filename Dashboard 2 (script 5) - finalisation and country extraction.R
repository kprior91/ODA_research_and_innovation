#####################################
# Script 4 
# Add data collected by spreadsheet from FCDO partners
#####################################

if (!("googlesheets4" %in% installed.packages())) {
  install.packages("googlesheets4")
}
if (!("gargle" %in% installed.packages())) {
  install.packages("gargle")
}
if (!("geonames" %in% installed.packages())) {
  install.packages("geonames")
}
if (!("RgoogleMaps" %in% installed.packages())) {
  install.packages("RgoogleMaps")
}
if (!("rworldmap" %in% installed.packages())) {
  install.packages("rworldmap")
}
if (!("ggmap" %in% installed.packages())) {
  install.packages("ggmap")
}
if (!("jsonlite" %in% installed.packages())) {
  install.packages("jsonlite")
}
if (!("rvest" %in% installed.packages())) {
  install.packages("rvest")
}
if (!("stringi" %in% installed.packages())) {
  install.packages("stringi")
}
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")
}
if (!("readxl" %in% installed.packages())) {
  install.packages("readxl")
}
if (!("writexl" %in% installed.packages())) {
  install.packages("writexl")
}

# Load packages -----
library(geonames) 
library(RgoogleMaps)
library(rworldmap)
library(ggmap)
library(jsonlite)
library(rvest)
library(stringi)
library(googlesheets4)
library(gargle)
library(httr)
library(tidyverse)
library(writexl)
library(readxl)

# Read in data from script 4
all_projects_transactions <- readRDS(file = "Outputs/all_projects_transactions.rds")

# 1) Extract countries -----------------------------------

# Add the country mentioned field onto main dataset
all_projects_final <- all_projects_transactions %>% 
  mutate(location_country = paste0(coalesce(lead_org_country, ""), ", ", coalesce(partner_org_country, "")),
         beneficiary_country = recipient_country)

# Convert location vs. recipient country data to long format
countries_data <- all_projects_final %>% 
  select(id, beneficiary_country, location_country) %>%
  gather(key = "country_type", value = "Country", -id) %>% 
  right_join(select(all_projects_final, -beneficiary_country, -location_country), by = "id") %>% 
  mutate(Country = str_replace_all(Country, "NA", "Unknown"),
         Country = str_replace_all(Country, ",,", ","))

# Create one row per country
all_projects_split_country <- countries_data %>%
  select(id, extending_org, country_type, Country) %>% 
  mutate(Country = str_replace_all(Country, "Tanzania, United Republic Of,|Tanzania, United Republic of,", "Tanzania,")) %>%
  mutate(Country = str_replace_all(Country, ";", ",")) %>%
  mutate(Country = gsub("\\s*\\([^\\)]+\\)","", as.character(Country))) %>%
  separate_rows(Country, sep = ",", convert = FALSE) %>%
  mutate(Country = str_trim(Country)) %>% 
  mutate(Country = str_replace_all(Country, c("UK|Scotland|Wales|United kingdom|England|Northern Ireland|UNITED KINGDOM"), "United Kingdom"),
         Country = str_replace_all(Country, c("USA|UNITED STATES|United states"), "United States"),
         Country = str_replace(Country, "N/A", "Unknown"),
         Country = str_replace(Country, "The Netherlands", "Netherlands"),
         Country = str_replace(Country, "The Philippines", "Philippines"),
         Country = if_else(str_detect(Country, "Ivoire"), "Ivory Coast", Country),
         Country = str_replace(Country, "Republic of Congo", "Congo Republic"),
         Country = str_replace(Country, "DRC", "Democratic Republic of the Congo"),
         Country = if_else(str_detect(Country, "Hong Kong"), "Hong Kong", Country),
         Country = str_replace_all(Country, "é", "e")) %>% 
  unique() %>% 
  filter(!(Country %in% c("", "NA", "Unknown")) & !is.na(Country)) %>% 
  arrange(id)

# Read in DAC country lookup and Tableau accepted country list
dac_lookup <- read_xlsx("Inputs/Country lookup - Tableau and DAC Income Group.xlsx")

# Check countries that are unmatched (this information will be lost)
unmatched_countries <- all_projects_split_country %>%
  filter(!(Country %in% dac_lookup$country_name)) %>% 
  select(Country) %>% 
  unique()

# Replace country with "Unknown" if not recognised against Tableau's 
# accepted list
all_projects_split_country <- all_projects_split_country %>%
  mutate(Country = if_else(Country %in% dac_lookup$country_name, Country, "Unknown")) %>% 
  unique()


# Join countries to project data
all_projects_final <- countries_data %>% 
  # remove commas at start
  mutate(Country = if_else(substr(Country, 1, 1) == ",", substr(Country, 2, length(Country)-1), Country)) %>% 
  rename(all_countries = Country) %>% 
  left_join(all_projects_split_country, by = c("id", "extending_org", "country_type")) %>% 
  mutate(date_refreshed = Sys.Date())


# Add row ID field to dataset
all_projects_final$row_id <- seq.int(nrow(all_projects_final))


# 2) Tidy country info (i.e. remove unnecessary duplicate records) ---------------

# Extract project records with unknown or missing country field
missing_country_projects <- filter(all_projects_final, 
                                   Country %in% c("Unknown") | is.na(Country)) %>% 
  select(row_id, id, country_type) %>% 
  unique() %>% 
  mutate(exclude_flag = 1)

# Identify projects that have both a populated and missing country field 
duplicate_country_projects <- filter(all_projects_final, 
                                     !(Country %in% c("Unknown") | is.na(Country))) %>% 
  select(row_id, id, country_type) %>% 
  unique() %>% 
  filter(id %in% missing_country_projects$id) 

# Exclude project records with unknown/missing abstract or beneficiary country AND
# a populated other country record
all_projects_tidied <- all_projects_final %>% 
  left_join(missing_country_projects, by = c("row_id", "id", "country_type")) %>% 
  filter(!(exclude_flag == 1 & country_type == "beneficiary_country" & (Country %in% c("Unknown") | is.na(Country))))

# Label unknown/missing countries as "Unknown" to remove NULLs from Tableau map
all_projects_tidied <- all_projects_tidied %>% 
  mutate(Country = if_else(is.na(Country), "Unknown", Country)) %>% 
  select(-exclude_flag)

# Add FCDO programme ID
all_projects_tidied <- all_projects_tidied %>% 
    # remove any text before "-1-" in the FCDO IATI ID
  mutate(fcdo_programme_id = if_else(Funder == "Foreign, Commonwealth and Development Office"
                                     & str_detect(iati_id, "-1-"),
                                     sub(".*-1-", "", iati_id), "")) %>% 
    # remove any FCDO component numbers
  mutate(fcdo_programme_id = sub("-.*", "", fcdo_programme_id))


# 3) Check data -----------------------------------

all_projects_tidied <- all_projects_tidied %>% 
  mutate(Fund = if_else(str_detect(Fund, "FCDO Research"), "FCDO fully funded", Fund),
         Funder = if_else(Funder == "Foreign, Commonwealth & Development Office", "Foreign, Commonwealth and Development Office", Funder)) 

# check list of ODA R&I funds
unique(all_projects_tidied$Fund)

test <- filter(all_projects_tidied, is.na(Fund))

nrow(test)

# check list of ODA R&I funders
unique(all_projects_tidied$Funder)
test <- filter(all_projects_tidied, is.na(Funder))

# Look at data from a particular delivery partner
test <- filter(all_projects_tidied, extending_org == "Bill & Melinda Gates Foundation")

# Look for a particular award (from keyword in title)
test <- filter(all_projects_tidied, str_detect(title, "under-five"))

# Check countries
unique(all_projects_tidied$Country)

test <- filter(all_projects_tidied, 
               is.na(Country) | str_detect(Country, ","))

test2 <- filter(all_projects_tidied, 
                Country == "Unknown")


test3 <- filter(all_projects_tidied, 
                Country == "Niger")

# Unknown country should be for the activity location only
unique(test2$country_type)

# TEMPORARY ***
# Remove IDRC DHSC IATI data
all_projects_tidied <- all_projects_tidied %>% 
  filter(!(Funder == "Department of Health and Social Care" & extending_org == "International Development Research Centre"))

# 4) Write data --------------------------------

# Write to RDS 
saveRDS(all_projects_tidied, "Outputs/all_projects_tidied.rds")
# all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 

# Limit size and number of columns for writing
all_projects_tidied <- all_projects_tidied %>% 
 # select(-subject, -all_countries, -date_refreshed) %>% 
  mutate(country_type = if_else(country_type == "beneficiary_country", 1, 2)) %>% 
  unique()

# Write data to EC google drive 
# Authorise googlesheets4 to view and manage sheets on EC Drive
# (using saved authentication token in folder)

ODA_RI_url <- "https://docs.google.com/spreadsheets/d/1ByVBWb3LNSoqAUzKlddd537DleQ-y9MINwY_SuuZEbY/edit#gid=2024786204"
results <- as_sheets_id(ODA_RI_url)

results_sheet <- sheet_write(all_projects_tidied,
                             ss = results,
                             sheet = "ODA_RI_projects")



# 5) Testing ---------------------------------------
test <- filter(all_projects, extending_org == "Global Disability Innovation Hub")

test2 <- filter(all_projects_final, extending_org == "Institute of Development Studies")

test <- filter(all_projects_tidied, str_detect(fcdo_programme_id, "202766"))

test <- filter(all_projects_transactions, str_detect(iati_id, "202766"))

test <- filter(all_projects_tidied, str_detect(id, "MR/N006267/1"))










