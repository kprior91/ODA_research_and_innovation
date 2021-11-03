##########
# Estimate total funding across ODA R&I portfolio
##########

### 1) UKRI - GCRF/Newton ---
ukri_gcrf_newton <- ukri_projects_final %>% 
  filter(Fund %in% c("Global Challenges Research Fund (GCRF)", "Newton Fund"),
         status == "Active") %>% 
  select(id, amount) %>% 
  unique()

# Sum total award amounts
sum(ukri_gcrf_newton$amount, na.rm = TRUE)


### 2) UKRI - FCDO ---
ukri_fcdo <- ukri_projects_final %>% 
  filter(Funder == "Foreign, Commonwealth and Development Office",
         status == "Active") %>% 
  select(id, amount) %>% 
  unique()

# Sum total award amounts
sum(ukri_fcdo$amount, na.rm = TRUE)


### 3) UKRI - DHSC ---
ukri_dhsc <- ukri_projects_final %>% 
  filter(Funder == "Department of Health and Social Care",
         status == "Active") %>% 
  select(id, amount) %>% 
  unique()

# Sum total award amounts
sum(ukri_dhsc$amount, na.rm = TRUE)


### 3) FCDO Research & Evidence Division (non-UKRI, Wellcome)

fcdo_non_ukri <- all_projects_tidied %>% 
  filter(Funder == "Foreign, Commonwealth and Development Office",
         !(extending_org %in% c("AHRC", "BBSRC", "EPSRC", "ESRC", "Innovate UK", "MRC", "NERC", "STFC")), 
         currency == "GBP"
         ) %>% 
  select(id, amount) %>% 
  unique()

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

ghs_awards <- all_projects_tidied %>% 
  filter(Fund %in% c("Global Health Security - UK Vaccine Network", "Global Health Security - GAMRIF"),
         status == "Active") %>% 
  select(id, amount) %>% 
  unique()

sum(ghs_awards$amount, na.rm = TRUE)


### Wellcome

wellcome_grant_sum <- all_projects_tidied %>% 
  filter(str_detect(extending_org, "Wellcome"),
         amount > 0) %>% 
  select(id, amount) %>% 
  unique()

sum(wellcome_grant_sum$amount, na.rm = TRUE)


### UK Space Agency

# Check a delivery partner
test <- all_projects_tidied %>% 
  filter(extending_org == "UK Space Agency",
         status == "Active") %>% 
  select(id, amount) %>% 
  unique()

sum(test$amount, na.rm = TRUE)


### Met Office

test <- all_projects_tidied %>% 
  filter(extending_org == "Met Office",
         status == "Active") %>% 
  select(id, amount) %>% 
  unique()

sum(test$amount, na.rm = TRUE)


### British Council

test <- all_projects_tidied %>% 
  filter(Funder == "Department for Business, Energy and Industrial Strategy",
         extending_org == "British Council",
         status == "Active") %>% 
  select(id, amount) %>% 
  unique()

sum(test$amount, na.rm = TRUE)


### Academies

test <- all_projects_tidied %>% 
  filter(Funder == "Department for Business, Energy and Industrial Strategy",
         extending_org %in% c("Royal Society", "Royal Academy of Engineering", "Academy of Medical Sciences", "British Academy"),
         status == "Active") %>% 
  select(id, amount) %>% 
  unique()

sum(test$amount, na.rm = TRUE)


