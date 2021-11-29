#####################################
# Script 5 
# Format dataset for Tableau map
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

# Convert dataset to long with one row per country entry
all_projects_split_country <- countries_data %>%
  select(id, extending_org, country_type, Country) %>% 
  mutate(Country = str_replace_all(Country, "Tanzania, United Republic Of|Tanzania, United Republic of", "Tanzania"),
         Country = if_else(Country %in% c("Congo (the Democratic Republic of the)", "DRC"), 
                           "Democratic Republic of the Congo", Country)) %>%
  mutate(Country = str_replace_all(Country, ";", ",")) %>%
  mutate(Country = gsub("\\s*\\([^\\)]+\\)","", as.character(Country))) %>%
  separate_rows(Country, sep = ",", convert = FALSE) 

# Clean country field
all_projects_split_country <- all_projects_split_country %>% 
  mutate(Country = str_trim(Country)) %>% 
  mutate(Country = case_when(
           str_detect(Country, "UK|Uk|Scotland|Wales|United kingdom|England|Northern Ireland|UNITED KINGDOM") == TRUE ~ "United Kingdom",
           str_detect(Country, "USA|Usa|UNITED STATES|United states|United States Of America|US") == TRUE ~ "United States",
           str_detect(Country, "Netherlands") == TRUE ~ "Netherlands",
           str_detect(Country, "Philippines") == TRUE ~ "Philippines",
           str_detect(Country, "Ivoire") == TRUE ~ "Ivory Coast",
           str_detect(Country, "Republic of Congo") == TRUE ~ "Congo Republic",
           str_detect(Country, "Hong Kong") == TRUE ~ "Hong Kong",
           str_detect(Country, "Viet Nam") == TRUE ~ "Vietnam",
           str_detect(Country, "Lao") == TRUE ~ "Laos",
           TRUE ~ Country)) %>% 
  unique() %>% 
  filter(!(Country %in% c("", "NA", "Unknown")) & !is.na(Country)) %>% 
  arrange(id)

# Check countries that are unmatched (this information will be overwritten)
unmatched_countries <- all_projects_split_country %>%
  filter(!(Country %in% dac_lookup$country_name)) %>% 
  select(Country) %>% 
  unique()

# Replace country with "Unknown" if not recognised against Tableau's accepted list
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

rm(all_projects)
rm(all_projects_split_country)

# 2) Tidy country info (i.e. remove unnecessary "unknown"s) ---------------

# Extract project records with unknown or missing country field
unknown_country_projects <- filter(all_projects_final, 
                                   Country %in% c("Unknown") | is.na(Country)) %>% 
  select(row_id, id) %>% 
  unique() %>% 
  mutate(exclude = 1)

# Identify projects that have both a populated and missing country field 
# Restrict to just the populated fields (to keep)
duplicate_country_projects <- filter(all_projects_final, 
                                     !(Country %in% c("Unknown") | is.na(Country))) %>% 
  select(row_id, id) %>% 
  unique() %>% 
  filter(id %in% unknown_country_projects$id) %>% 
  mutate(keep = 1)


# Exclude project records for "Unknown" country when the project has other country info
all_projects_tidied <- all_projects_final %>% 
  left_join(unknown_country_projects, by = c("row_id", "id")) %>% 
  left_join(duplicate_country_projects, by = c("row_id", "id")) %>%
  filter(keep == 1 |
         exclude == 1 & !(id %in% duplicate_country_projects$id) |
         is.na(keep) & is.na(exclude)) %>% 
  select(-keep, -exclude) %>% 
  mutate(Country = coalesce(Country, "Unknown"))

rm(all_projects_final)
rm(unknown_country_projects)
rm(duplicate_country_projects)

# 3) Add funder programme names ------------------

# Tidy fund and funder labelling
all_projects_tidied <- all_projects_tidied %>% 
  mutate(Fund = if_else(str_detect(Fund, "FCDO Research & Innovation"), "FCDO Research - Programmes", Fund))

# Add FCDO programme ID to dataset
all_projects_tidied <- all_projects_tidied %>% 
    # remove any text before "-1-" in the FCDO IATI ID
  mutate(fcdo_programme_id = if_else(Funder %in% c("Foreign, Commonwealth and Development Office", "FCDO")
                                     & str_detect(iati_id, "-1-"),
                                     sub(".*-1-", "", iati_id), "")) %>% 
    # remove any FCDO component numbers
  mutate(fcdo_programme_id = sub("-.*", "", fcdo_programme_id))

# Add FCDO programme name to dataset

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
      left_join(gov_funder_programme_names, by = c("iati_id" = "funder_iati_id")) %>% 
      mutate(funder_programme = if_else(extending_org == "Wellcome Trust", subject, funder_programme))


# 4) Apply manual exclusions/rule ----------------------------

# TEMPORARY ***
# Remove IDRC DHSC IATI data (this has been provided by spreadsheet)
all_projects_tidied <- all_projects_tidied %>% 
  filter(!(Funder == "Department of Health and Social Care" & extending_org == "International Development Research Centre"))

# Remove Afghanistan projects (added Sep 21)
all_projects_tidied <- all_projects_tidied %>% 
  filter(Country != "Afghanistan")

# Manually edit country info for Chevening Scholarships
all_projects_tidied <- all_projects_tidied %>% 
  mutate(lead_org_country = if_else(Fund == "Chevening Scholarships", "United Kingdom", lead_org_country),
         Country = if_else(Fund == "Chevening Scholarships" & country_type == 2, "United Kingdom", Country),
         start_date = if_else(Fund == "Chevening Scholarships", "", start_date))

# Remove non-research partners
# (linked partner data from non-RED managed programmes)
all_projects_tidied <- all_projects_tidied %>% 
  filter(!(extending_org %in% c("Sightsavers",
                                "Coffey International Development Limited, a Tetra Tech Company")))

# Correct missing IDS name (ARPA activity)
all_projects_tidied <- all_projects_tidied %>% 
  mutate(extending_org = if_else(extending_org == "GB-COH-877338", 
                                 "Institute of Development Studies", extending_org))

# Remove WHO non-research/innovation activities
all_projects_tidied <- all_projects_tidied %>% 
  filter(!(extending_org == "World Health Organization") |
         str_detect(title, "research|innovation"))


# 5) Check data ---------------------

# check list of ODA R&I funds
table(all_projects_tidied$Fund)

# check list of ODA R&I funders
table(all_projects_tidied$Funder)

# check list of ODA R&I funders
table(all_projects_tidied$currency)

# 6) Write data --------------------------------

# Restrict to active projects for Tableau
all_projects_tidied <- all_projects_tidied %>% 
  mutate(country_type = if_else(country_type == "beneficiary_country", 1, 2)) %>% 
  filter(status %in% c("Active", "Unknown")) %>% 
  unique()

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


# 7) Testing ---------------------------------------

# Number of unique projects
length(unique(all_projects_tidied$id))

# Look at data from a particular delivery partner
test <- filter(all_projects_tidied, extending_org == "Bill & Melinda Gates Foundation")

# Look for a particular award (from keyword in title)
test <- filter(all_projects_tidied, str_detect(extending_org, "Elrha"))

# Test country unknown exclusion logic
test1 <- filter(all_projects_final, id == "GB-CHC-209131-A05500")
test2 <- filter(all_projects_tidied, id == "GB-CHC-209131-A05500")

test1 <- filter(all_projects_final, id == "GB-GOV-3-Chevening-Scholarships-SI")
test2 <- filter(all_projects_tidied, id == "GB-GOV-3-Chevening-Scholarships-SI")

test2 <- filter(duplicate_country_projects, str_detect(id, "GB-GOV-7-MOHC-001"))
test3 <- filter(all_projects_final, str_detect(Funder, "Rural"))
test4 <- filter(all_projects_tidied, str_detect(Funder, "Rural"))

test <- filter(all_projects_split_country, str_detect(id, "GCRFNGR6"))
test <- filter(all_projects_tidied, id == "GCRFNGR6\1548")
test <- filter(all_projects_tidied, str_detect(title, "UMURINZI"))




test <- all_projects_tidied %>% 
  filter((str_detect(extending_org, "Academy") | str_detect(extending_org, "Society")),
         Funder == "Department for Business, Energy and Industrial Strategy") %>% 
  select(id, amount) %>% 
  unique()

sum(test$amount, na.rm = TRUE)

