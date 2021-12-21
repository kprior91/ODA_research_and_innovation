# --------------------------------------------------------------- #
# Script 8 
# Run automated tests on final ODA R&I project dataset
# 
# A) SET UP FUNCTIONS 
# B) Script 1 - GOV FUNDER IATI RESEARCH ACTIVITIES
# C) Script 4 - MASTER PROJECT DATASET
# D) Script 5 - SQL tables
# E) Script 6 - ACTIVE PROJECT EXTRACT FOR TABLEAU
# 
# --------------------------------------------------------------- #

### Create test comparisons

gov_funders_expected <- c(
                    "Cross-government Prosperity Fund",
                    "Department for Business, Energy and Industrial Strategy",
                    "Department for Environment, Food, and Rural Affairs",
                    "Department of Health and Social Care",
                    "Foreign, Commonwealth and Development Office")

funds_expected <- c("Chevening Scholarships",
                    "FCDO Research - Programmes",                 
                    "Global Challenges Research Fund (GCRF)", "Global Health Research - Partnerships",      
                    "Global Health Research - Programmes", "Global Health Security - GAMRIF",            
                    "Global Health Security - UK Vaccine Network", "International Climate Finance (ICF)",        
                    "Newton Fund", "Other")


### A) SET UP FUNCTIONS ----

## LOOKUPS ##
expect_equal(org_country_lookup("International Development Research Centre"), "Canada")


## IATI ##

# Error with this CGIAR page
# org_code <- "XM-DAC-47015"
# page <- 77
# org_activity_extract(page, org_code, data.frame())

## UKRI ##

# fund_name <- "GCRF"
# test <- extract_ukri_projects_by_fund(1, fund_name)

# person_id <- "6E394347-A44B-4868-8EC3-06CA4D034BDA"
# test <- extract_staff_org(person_id)

# org_id <- "C3F01E23-4A38-46BD-B3CE-8983CE6DBF36"
# test <- extract_org_country(org_id)

# id <- "AH/T007362/1"
# test <- extract_ukri_projects_by_id(id)


### B) GOV FUNDER IATI RESEARCH ACTIVITIES
gov_list_final <- readRDS("Outputs/gov_list_final.rds")
ri_iati_activities <- readRDS(file = "Outputs/ri_iati_activities.rds")

test_that("UK government funder names are as expected", {
  
  gov_funders_actual <- gov_list_final %>%
    select(reporting_org) %>% 
    unique() %>% 
    arrange(reporting_org)
  
  expect_equal(gov_funders_expected, gov_funders_actual$reporting_org)
  
})

# Check fund names

test_that("fund names are as expected", {
  
  gov_funds_actual <- gov_list_final %>%
    select(fund) %>% 
    unique() %>% 
    arrange(fund)
  
  expect_equal(funds_expected, gov_funds_actual$fund)
  
})


# Check how many UK gov funders are using the "RI" IATI tag

ri_tag_users <- unique(ri_iati_activities$reporting_org.ref)
View(ri_tag_users)


# Check problematic IATI activity IDs 

test_that("South Asia Research Country Fund data can be extracted", {
  id <- "GB-1-205053"
  data <- iati_activity_extract(id)
  expect_true(length(data) > 0)
})  # not working

test_that("Evidence Fund data can be extracted", {
  id <- "GB-GOV-1-300708"
  data <- iati_activity_extract(id)
  expect_true(length(data) > 0)
})  # working

test_that("LSE Int. Growth Centre data can be extracted", {
  id <- "GB-COH-00070527-IGC-P3"
  data <- iati_activity_extract(id)
  expect_true(length(data) > 0)
})  # not working

test_that("IDS example data can be extracted", {
  id <- "GB-COH-877338-GV-GOV-1-300708-124" 
  data <- iati_activity_extract(id)
  expect_true(length(data) > 0)
})  # not working


### C) MASTER DATASET ----
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 

# Check for specific project ID

test_that("Project with known ID is present in master dataset", {
  
  test_id <- "NE/V009591/1"   
  test_data <- filter(all_projects_tidied, str_detect(id, test_id))
  expect_true(nrow(test_data) > 0)
  
})


# Check activities with recipient country at transaction level have 
# this info picked up

test_that("IATI recipient countries in transactions are picked up", {
  
  detact_example <- activity_list %>% 
    filter(str_detect(iati_identifier, "GB-UKPRN-10007774-DeTACT")) 

  expect_false(is.na(detact_example$recipient_country))

})

# Check no duplicates when organisations have names in multiple languages in the
# IATI registry

test_that("IDRC has no duplicate projects and its English name in dataset only", {
  
  idrc_example <- all_projects_tidied %>% 
    filter(str_detect(id, "XM-DAC-301-2")) %>% 
    select(extending_org) %>% 
    unique()
  
  expect_equal(idrc_example$extending_org, "International Development Research Centre")
  
})

test_that("UN Refugee Agency", {
  
  un_ref_example <- all_projects_tidied %>% 
    filter(str_detect(id, "XM-DAC-41121-2017-GLOBALPROG")) %>% 
    select(extending_org) %>% 
    unique()
  
  expect_equal(un_ref_example$extending_org, "The UN Refugee Agency")
  
})


# Check funder names

test_that("check all UK government funders are present in dataset", {
  
  gov_funders_actual <- sort(unique(all_projects_tidied$Funder))
  expect_equal(gov_funders_actual, gov_funders_expected)

})


# 2) Check example MRC project with multiple funders

test_that("co-funded MRC project processed correctly", {
  
  mrc_cofunded_example <- filter(all_projects_tidied,
                               id == "MR/M009211/1")

  mrc_common_info <- mrc_example_actual %>% 
    select(-Funder, -Fund, -iati_id) %>% 
    unique()
  
  expect_equal(nrow(mrc_cofunded_example), 3)
  expect_equal(nrow(mrc_common_info), 1)

})


# 3) Check unique project IDs 

test_that("check each project has a unique ID", {
  
  unique_projects <- all_projects_tidied %>% 
    select(-Funder, -Fund, -iati_id) %>% 
    unique()
  
  unique_project_ids <- select(unique_projects, id) %>% unique()
  
      ### Investigate duplicates (partner org order)
      duplicates_ids <- unique_projects %>% 
        group_by(id) %>% 
        summarise(n = n()) %>% 
        filter(n > 1)
      
      duplicates <- unique_projects %>% 
        filter(id %in% duplicates_ids$id)
      ###

  expect_equal(nrow(unique_projects), nrow(unique_project_ids))
  
  })


# 4) Check Wellcome Grant amounts

test_that("check Wellcome grants with no ODA spend are excluded", {
  
  wellcome_zero_grants <- all_projects_tidied %>% 
    filter(extending_org == "Wellcome Trust",
           (amount == 0 | is.na(amount))) 
  
  expect_equal(nrow(wellcome_zero_grants), 0)
  
})


# 5) Check non-research partner exclusion

test_that("non-research partners have been excluded", {
  
  non_research_org <- all_projects_tidied %>% 
    filter(extending_org == "Sightsavers") 
  
  expect_equal(nrow(non_research_org), 0)
  
})


test_that("South Asia Research Fund and Evidence Fund are present", {
  
  evidence_funds <- all_projects_tidied %>% 
    filter(str_detect(id, "205053|300708")) %>% 
    select(iati_id) %>% unique()
  
  expect_equal(nrow(evidence_funds), 2) 
  
})


### D) SQL tables ----

# Connect to ODA RI Projects database on development server
# (need to be connected to DFID VPN)

con_live <- DBI::dbConnect(odbc::odbc(),
                           Driver = "SQL Server", 
                           Server   = "hel-sql-120",
                           Database = "ODARIProjects",
                           Trusted_Connection = "True")

# Read in files for testing
project_table <- readRDS("Outputs/project_table.rds")
funder_table <- readRDS("Outputs/funder_table.rds")
org_names_and_locations <- readRDS("Outputs/org_names_and_locations.rds")
country_table <- readRDS(file = "Outputs/country_table.rds")
country_table_cleaned <- readRDS(file = "Outputs/country_table_cleaned.rds")

# QA SQL tables

test_that("number of rows in SQL tables is as expected", {
  
  recordSet <- dbSendQuery(con_live, "(SELECT 'PROJECT TABLE' AS LABEL
                                              ,COUNT(*) FROM [Project])
                                         UNION
                                      (SELECT 'FUNDER TABLE' AS LABEL
                                              ,COUNT(*) FROM [Funder])
                                         UNION
                                      (SELECT 'ORG TABLE' AS LABEL
                                              ,COUNT(*) FROM [Organisation])
                                                                  UNION
                                      (SELECT 'COUNTRY TABLE' AS LABEL
                                              ,COUNT(*) FROM [Country])")
  
  table_row_counts <- dbFetch(recordSet, n = -1)
  
  table_row_comp <- c(nrow(project_table), nrow(funder_table), nrow(org_names_and_locations),
                      nrow(country_table_cleaned))
  
  expect_equal(table_row_counts, table_row_comp)

})


# Check country cleaning

test_that("check common country formatting issues", {
  
  print("Checking: DRC")
  
  original_1 <- country_table %>% 
    filter(str_detect(project_id, "COD-20180"), country_type == 1)
  
  corrected_1 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "COD-20180"), country_type == 1)
  
  original_2 <- country_table %>% 
    filter(str_detect(project_id, "XM-DAC-301-2-107350-001"), country_type == 1)
  
  corrected_2 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "XM-DAC-301-2-107350-001"), country_type == 1)
  
  original_3 <- country_table %>% 
    filter(str_detect(project_id, "300211-4"), country_type == 1)
  
  corrected_3 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "300211-4"), country_type == 1)
  
  print(paste0("Original text: ", original_1$Country, " / ", original_2$Country, " / ", original_3$Country))
  expect_equal(corrected_1$Country, corrected_2$Country, corrected_3$Country,
               "democratic republic of the congo")
  
  print("Checking: Tanzania")
  
  original_4 <- country_table %>% 
    filter(str_detect(project_id, "BB/S014586/1"), country_type == 1)
  
  corrected_4 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "BB/S014586/1"), country_type == 1)
  
  print(paste0("Original text: ", original_4$Country))
  expect_equal(corrected_4$Country, "tanzania")

  
  print("Checking: China")
  
  original_5 <- country_table %>% 
    filter(str_detect(project_id, "NF-BCCNPDEP-202"), country_type == 1)
  
  corrected_5 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "NF-BCCNPDEP-202"), country_type == 1)
  
  print(paste0("Original text: ", original_5$Country))
  expect_equal(corrected_5$Country, "china")
  
  
  print("Checking: Korea")
  
  original_6 <- country_table %>% 
    filter(str_detect(project_id, "GCRF-CICA-R12017-IC170195"), country_type == 1)
  
  corrected_6 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "GCRF-CICA-R12017-IC170195"), country_type == 1)
  
  print(paste0("Original text: ", original_6$Country))
  countries <- paste0(corrected_6$Country, collapse = "|")
  expect_true(str_detect(countries, "democratic people’s republic of korea"))
  
  
  print("Checking: United States")
  
  original_7 <- country_table %>% 
    filter(str_detect(project_id, "GB-GOV-1-300126-MO-5"), country_type == 2)
  
  corrected_7 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "GB-GOV-1-300126-MO-5"), country_type == 2)
  
  original_8 <- country_table %>% 
    filter(str_detect(project_id, "GB-COH-03122495-EEG-29"), country_type == 2)
  
  corrected_8 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "GB-COH-03122495-EEG-29"), country_type == 2)
  
  original_9 <- country_table %>% 
    filter(str_detect(project_id, "us-ein-522044704-NewVaccinesForTB"), country_type == 1)
  
  corrected_9 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "us-ein-522044704-NewVaccinesForTB"), country_type == 1)
  
  print(paste0("Original text: ", original_7$Country, " / ", original_8$Country))
  expect_equal(corrected_7$Country, corrected_8$Country, "united states")
  
  print(paste0("Original text: ", original_9$Country))
  countries <- paste0(corrected_9$Country, collapse = "|")
  expect_true(str_detect(countries, "united states"))
  
  
  print("Checking: (the) removed")
  
  original_6 <- country_table %>% 
    filter(str_detect(project_id, "GB-CHC-222655-LIGHT"), country_type == 1)
  
  corrected_6 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "GB-CHC-222655-LIGHT"), country_type == 1)
  
  print(paste0("Original text: ", original_6$Country))
  countries <- paste0(corrected_6$Country, collapse = "|")
  expect_false(str_detect(countries, "\\(the\\)"))

})


# Check country separation
# (sense check manually)

test_that("all separating characters identified", {
 
  # ; separator
  test1 <- country_table %>% 
    filter(str_detect(project_id, "RWA-20054"))
  
  test2 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "RWA-20054")) 
  
  View(test2)
  View(test1)
  
  # , separator
  test1 <- country_table %>% 
    filter(str_detect(project_id, "MR/S004769/1"))
  
  test2 <- country_table_cleaned %>% 
    filter(str_detect(project_id, "MR/S004769/1")) 
  
  View(test2)
  View(test1)
  
})
  
# Check unrecognised countries
# (these will be overwritten as "unknown")

unmatched_countries <- country_table_cleaned %>%
  filter(!(Country %in% dac_lookup$country_name)) %>% 
  select(Country) %>% 
  unique()

print(paste0("No. of unrecognised countries: ", nrow(unmatched_countries)))
View(unmatched_countries)


# Test country unknown exclusion logic

test_that("only projects with no country information whatsoever are labelled unknown", {
  
  print("example 1: genuine unknown - Defra")
  test1 <- filter(country_table_final, str_detect(project_id, "GB-GOV-7-ICF-P0011-RD"))
  expect_equal(unique(test1$Country), "Unknown")
  expect_equal(nrow(test1), 2)
  
  print("example 2: known beneficiary, no known location")
  test2 <- filter(country_table_final, project_id == "GCRF-RAECHEPSSA-1819-3-HEPSSA2\\71") %>% arrange(country_type)
  expect_equal(test2$Country, c("Nigeria", "Unknown"))
  expect_equal(nrow(test2), 2)
  
  print("example 3: known location, no known beneficiary")
  test3 <- filter(country_table_final, project_id == "BB/R019819/1") %>% arrange(country_type)
  expect_equal(test3$Country, c("Unknown", "United Kingdom"))
  expect_equal(nrow(test3), 2)
  
  print("example 4: known beneficiary and known location") 
  test4 <- filter(country_table_final, project_id == "ES/P010245/1")
  expect_equal(length(unique(test4$country_type)), 2)
  expect_equal(nrow(test4), 2)  
  
  print("example 5: check Chevening country location")
  test5 <- filter(country_table_final, str_detect(project_id, "Chev"), country_type == 2)
  expect_equal(unique(test5$Country), "United Kingdom")
  
})



### E) ACTIVE PROJECT EXTRACT FOR TABLEAU ----
tableau_projects_tidied <- readRDS("Outputs/tableau_projects_tidied.rds") 

# Check fund names

test_that("fund names are as expected", {
  
  funds_actual <- tableau_projects_tidied$Fund %>% 
    unique() %>% sort()
  
  expect_equal(funds_actual, funds_expected)
  
})

# Check country_type field
test_that("country_type field has 3 types", {
  
  country_types <- tableau_projects_tidied$country_type %>% 
    unique() %>% 
    sort()
  
  expect_equal(country_types, c(1,2,3))
  
})

# Output final list of countries (to sense-check for duplicates)
country_list_final <- tableau_projects_tidied %>% 
  select(Country) %>% 
  unique() %>% 
  arrange(Country)

View(country_list_final)


# Check funder names

test_that("funder names are as expected", {
  
  funders_actual <- tableau_projects_tidied$Funder %>% 
    unique() %>% sort()
  
  expect_equal(funders_actual, gov_funders_expected)
  
})

# Check no GBP currency typo

test_that("no GBP currency typo", {
  
  currencies_actual <- tableau_projects_tidied %>%
    select(currency) %>% 
    filter(!is.na(currency)) %>% 
    unique()
  
  expect_false("GDP" %in% currencies_actual$currency)
  
})

# Check active projects

test_that("check active projects are included only", {
  
  status_actual <- unique(all_projects_tidied$status)
  status_expected <- c("Active", "Unknown")
  
  expect_equal(status_actual, status_expected)
  
})



