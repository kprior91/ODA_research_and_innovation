#####################################
# Script 4 
# Split rows by country for Tableau
#####################################

# Read in data from script 3
all_projects <- readRDS("Outputs/all_projects.rds") 

# 1) Extract countries -----------------------------------

# Distinguish location and beneficiary countries in main dataset
all_projects_final <- all_projects %>% 
  mutate(location_country = paste0(coalesce(lead_org_country, ""), ", ", coalesce(partner_org_country, "")),
         beneficiary_country = recipient_country)

# Convert location vs. beneficiary country data to long format
countries_data <- all_projects_final %>% 
  select(id, beneficiary_country, location_country) %>%
  gather(key = "country_type", value = "Country", -id) %>% 
  right_join(select(all_projects_final, -beneficiary_country, -location_country), by = "id")

# Create one row per country
all_projects_split_country <- countries_data %>%
  select(id, extending_org, country_type, Country) %>% 
  mutate(Country = str_replace_all(Country, "Tanzania, United Republic Of|Tanzania, United Republic of", "Tanzania")) %>%
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
         Country = if_else(str_detect(Country, "Hong Kong"), "Hong Kong", Country)) %>% 
  unique() %>% 
  filter(!(Country %in% c("", "NA", "Unknown")) & !is.na(Country)) %>% 
  arrange(id)


# Create one row per country
all_projects_split_country <- countries_data %>%
  select(id, extending_org, country_type, Country) %>% 
  mutate(Country = str_replace_all(Country, "Tanzania, United Republic Of|Tanzania, United Republic of", "Tanzania"),
         Country = str_replace_all(Country, "Congo, Democratic Republic of", "Democratic Republic of the Congo")) %>%
  mutate(Country = gsub("\\s*\\([^\\)]+\\)","", as.character(Country))) %>%
  separate_rows(Country, sep = ",|;|/", convert = FALSE) %>%
  mutate(Country = str_trim(Country),
         Country = if_else(str_detect(tolower(Country), "uk|united kingdom|england|northern ireland|wales|scotland"), "United Kingdom", Country),
         Country = if_else(str_detect(tolower(Country), "usa|united states"), "United States", Country),
         Country = if_else(str_detect(tolower(Country), "netherlands"), "Netherlands", Country),
         Country = if_else(str_detect(tolower(Country), "philippines"), "Philippines", Country),
         Country = if_else(str_detect(tolower(Country), "ivoire"), "Ivory Coast", Country)) %>% 
  unique() %>% 
  filter(!(Country %in% c("", "NA", "Unknown")) & !is.na(Country)) %>% 
  arrange(id)


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
  left_join(all_projects_split_country, by = c("id", "extending_org", "country_type")) 

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

# Exclude project records with unknown/missing location or beneficiary country AND
# a populated other country record
all_projects_tidied <- all_projects_final %>% 
  left_join(missing_country_projects, by = c("row_id", "id", "country_type")) %>% 
  filter(!(exclude_flag == 1 & country_type == "beneficiary_country" & (Country %in% c("Unknown") | is.na(Country))))

# Label unknown/missing countries as "Unknown" to remove NULLs from Tableau map
all_projects_tidied <- all_projects_tidied %>% 
  mutate(Country = if_else(is.na(Country), "Unknown", Country)) %>% 
  select(-exclude_flag)

# Tidy fund and funder labelling
all_projects_tidied <- all_projects_tidied %>% 
  mutate(Fund = if_else(str_detect(Fund, "FCDO Research"), "FCDO Research - Programmes", Fund),
         Funder = if_else(str_detect(Funder, "Foreign, Commonwealth & Development Office|FCDO"), "Foreign, Commonwealth and Development Office", Funder)) 

# Manually edit country info for Chevening
all_projects_tidied <- all_projects_tidied %>% 
  mutate(lead_org_country = if_else(Fund == "Chevening Scholarships", "United Kingdom", lead_org_country),
         Country = if_else(Fund == "Chevening Scholarships" & country_type == 2, "United Kingdom", Country))

# Add FCDO programme ID
all_projects_tidied <- all_projects_tidied %>% 
    # remove any text before "-1-" in the FCDO IATI ID
  mutate(fcdo_programme_id = if_else(Funder %in% c("Foreign, Commonwealth and Development Office", "FCDO")
                                     & str_detect(iati_id, "-1-"),
                                     sub(".*-1-", "", iati_id), "")) %>% 
    # remove any FCDO component numbers
  mutate(fcdo_programme_id = sub("-.*", "", fcdo_programme_id))



# 3) Add funder programme names ------------------

# Create vector of FCDO gov funder programme IATI IDs
gov_funder_iati_ids <- all_projects_tidied %>% 
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
all_projects_tidied <- all_projects_tidied %>%
  left_join(gov_funder_programme_names, by = c("iati_id" = "funder_iati_id")) 


# 4) Check data -----------------------------------

# TEMPORARY ***
# Remove IDRC DHSC IATI data
all_projects_tidied <- all_projects_tidied %>% 
  filter(!(Funder == "Department of Health and Social Care" & extending_org == "International Development Research Centre"))


# check list of ODA R&I funds
unique(all_projects_tidied$Fund)

test <- filter(all_projects_tidied, is.na(Fund))

nrow(test)

# check list of ODA R&I funders
unique(all_projects_tidied$Funder)
test <- filter(all_projects_tidied, is.na(Funder))

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

# Limit size and number of columns for writing
all_projects_tidied <- all_projects_tidied %>% 
  mutate(country_type = if_else(country_type == "beneficiary_country", 1, 2)) %>% 
  filter(status %in% c("Active", "Unknown")) %>% 
  unique()

# Remove Afghanistan projects
all_projects_tidied <- all_projects_tidied %>% 
  filter(Country != "Afghanistan")
  
# Write data to EC google drive 
# Authorise googlesheets4 to view and manage sheets on EC Drive
# (using saved authentication token in folder)

ODA_RI_url <- "https://docs.google.com/spreadsheets/d/1ByVBWb3LNSoqAUzKlddd537DleQ-y9MINwY_SuuZEbY/edit#gid=2024786204"
results <- as_sheets_id(ODA_RI_url)

results_sheet <- sheet_write(all_projects_tidied,
                             ss = results,
                             sheet = "ODA_RI_projects")



# 5) Testing ---------------------------------------

unique(all_projects_tidied$Funder)

# Look at data from a particular delivery partner
test <- filter(all_projects_tidied, extending_org == "Bill & Melinda Gates Foundation")

# Look for a particular award (from keyword in title)
test <- filter(all_projects_tidied, str_detect(title, "under-five"))









