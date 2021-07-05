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

# Save as R file (from previous script)
all_projects <- readRDS("Outputs/all_projects.rds") 


# 1) Extract countries -----------------------------------

# Extract countries mentioned in abstract or title
countries <- countrycode::codelist$country.name.en
countries_string <- paste0(countries, collapse = "|")

countries_in_description <- all_projects %>% 
  mutate(text = paste0(title, " ", abstract)) %>% 
  select(title, text) %>% 
  mutate(countries_abstract = str_extract_all(text, countries_string)) %>% 
  unnest(cols = countries_abstract) %>% 
  unique() 

# Correct Niger / Nigeria problem
countries_in_description <- countries_in_description %>% 
  mutate(countries_abstract = if_else(str_detect(text, "Nigeria") & !str_detect(text, "Niger ") & countries_abstract == "Niger",
                                      "Nigeria", countries_abstract))

# Aggregate list of countries up
countries_in_description <- countries_in_description %>% 
  group_by(title) %>% 
  summarise(countries_abstract = paste(countries_abstract[!is.na(countries_abstract)], collapse = ", "))


# Add the country mentioned field onto main dataset
all_projects_final <- all_projects %>% 
  left_join(countries_in_description, by = "title") %>% 
  mutate(partner_countries = paste0(coalesce(lead_org_country, ""), ", ", coalesce(partner_org_country, "")),
         countries_abstract = paste0(coalesce(recipient_country, ""), ", ", coalesce(countries_abstract, "")))


# Distinguish between abstract and partner countries
countries_data <- all_projects_final %>% 
  select(ID, countries_abstract, partner_countries) %>%
  gather(key = "country_type", value = "Country", -ID) %>% 
  right_join(select(all_projects_final, -countries_abstract, -partner_countries), by = "ID") %>% 
  mutate(Country = str_replace_all(Country, "NA", "Unknown"),
         Country = str_replace_all(Country, ",,", ","))


# Separate out lead, partner countries 
all_projects_split_country <- countries_data %>%
  select(ID, country_type, Country) %>% 
  mutate(Country = str_replace_all(Country, "Tanzania, United Republic Of,|Tanzania, United Republic of,", "Tanzania,")) %>%
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
  arrange(ID)

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
  rename(all_countries = Country) %>% 
  # select(-country_type) %>% 
  left_join(all_projects_split_country, by = c("ID", "country_type")) %>% 
  mutate(date_refreshed = Sys.Date())


# 2) Tidy country info (i.e. remove unnecessary duplicate records) ---------------

# Extract project records with unknown or missing country field
missing_country_projects <- filter(all_projects_final, 
                                   Country %in% c("Unknown") | is.na(Country)) %>% 
  select(ID, id, country_type) %>% 
  unique() %>% 
  mutate(exclude_flag = 1)

# Identify projects that have both a populated and missing country field 
duplicate_country_projects <- filter(all_projects_final, 
                                     !(Country %in% c("Unknown") | is.na(Country))) %>% 
  select(ID, id, country_type) %>% 
  unique() %>% 
  filter(id %in% missing_country_projects$id) 

# Exclude project records with unknown/missing abstract or beneficiary country AND
# a populated other country record
all_projects_tidied <- all_projects_final %>% 
  left_join(missing_country_projects, by = c("ID", "id", "country_type")) %>% 
  filter(!(exclude_flag == 1 & country_type == "countries_abstract" & (Country %in% c("Unknown") | is.na(Country))))

# Label unknown/missing countries as "Unknown" to remove NULLs from Tableau map
all_projects_tidied <- all_projects_tidied %>% 
  mutate(Country = if_else(is.na(Country), "Unknown", Country))



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


# 4) Write data --------------------------------

# Write to RDS 
saveRDS(all_projects_tidied, "Outputs/all_projects_tidied.rds")
# all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 


# Write data to EC google drive 
# Authorise googlesheets4 to view and manage sheets on EC Drive
# (using saved authentication token in folder)

ODA_RI_url <- "https://docs.google.com/spreadsheets/d/1ByVBWb3LNSoqAUzKlddd537DleQ-y9MINwY_SuuZEbY/edit#gid=2024786204"
results <- as_sheets_id(ODA_RI_url)

results_sheet <- sheet_write(all_projects_tidied,
                             ss = results,
                             sheet = "ODA_RI_projects")



# 5) Testing ---------------------------------------
test <- filter(all_projects_tidied, extending_org == "AgResults")
test <- filter(partner_activity_comb, reporting_org == "AgResults")





















