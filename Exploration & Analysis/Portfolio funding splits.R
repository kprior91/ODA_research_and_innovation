##########
# Estimate total funding across ODA R&I portfolio
##########

### 1) UKRI - GCRF/Newton ---
ukri_gcrf_newton <- ukri_projects_final %>% 
  filter(Fund %in% c("Global Challenges Research Fund (GCRF)", "Newton Fund"),
         status == "Active") 

# check funder
unique(ukri_gcrf_newton$Funder)

# Sum total award amounts
sum(ukri_gcrf_newton$amount)
length(unique(ukri_gcrf_newton$id))


### 2) UKRI - FCDO ---
ukri_fcdo <- ukri_projects_final %>% 
  filter(Funder == "Foreign, Commonwealth and Development Office",
         status == "Active") 

# check fund
unique(ukri_fcdo$Fund)

# Sum total award amounts
sum(ukri_fcdo$amount)
length(unique(ukri_fcdo$id))


### 3) FCDO Research & Evidence Division (non-UKRI)

# Use Power BI IATI dashboard
# Status: Implementation
# Fund: "FCDO Research"
# Budget: £2.9 bn

fcdo_non_ukri <- iati_activity_list %>% 
  filter(fund == "FCDO Research & Evidence Division",
         activity_status == "Implementation",
         end_date >= Sys.Date())

# check hierarchy (should be 2 = component only to avoid double-counting spend)
unique(fcdo_non_ukri$hierarchy)
# check no duplicate records
test <- unique(fcdo_non_ukri$iati_identifier)
length(test)
# check budget status
table(fcdo_non_ukri$budget_status)

sum(fcdo_non_ukri$amount, na.rm = TRUE)


### 4) NIHR GHR Programmes ---
nihr_active <- nihr_projects_final %>% 
  filter(status == "Active")

sum(nihr_active$amount)


### 5) NIHR GHR Partnerships

# Use Power BI IATI dashboard
# Status: Implementation
# Fund: 
# Budget: 


### DHSC GAMRIF and UK Vaccine Network

# Use Power BI IATI dashboard
# Status: Implementation
# Funder: DHSC
# Budget: 86 million

ghs_awards <- all_projects %>% 
  filter(Fund %in% c("Global Health Security - UK Vaccine Network", "Global Health Security - GAMRIF"),
         status == "Active")

sum(ghs_awards$amount, na.rm = TRUE)

### Wellcome

sum(wellcome_grants_final$amount)


### UK Space Agency

# Check a delivery partner
test <- all_projects %>% 
  filter(extending_org == "UK Space Agency",
         status == "Active") 

sum(test$amount, na.rm = TRUE)


### Met Office

test <- all_projects %>% 
  filter(extending_org == "Met Office",
         status == "Active") 

sum(test$amount, na.rm = TRUE)


### British Council

test <- all_projects %>% 
  filter(Funder == "Department for Business, Energy and Industrial Strategy",
         extending_org == "British Council",
         status == "Active",
         end_date >= Sys.Date()) 

test <- iati_activity_list %>% 
  filter(reporting_org_ref == "GB-GOV-13",
         partner %in% c("British Council"),
         activity_status == "Implementation") 

sum(test$amount, na.rm = TRUE)


### Academies

test <- iati_activity_list %>% 
  filter(reporting_org_ref == "GB-GOV-13",
         partner %in% c("Royal Society", "Royal Academy of Engineering", "Academy of Medical Sciences", "British Academy"),
         activity_status == "Implementation") 

sum(test$amount, na.rm = TRUE)


