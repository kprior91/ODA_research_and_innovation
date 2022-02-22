##########
# Estimate total funding across ODA R&I portfolio
##########

# Read in active project data
tableau_projects_tidied <- readRDS("Outputs/tableau_projects_tidied.rds") 


### 1) UKRI - GCRF/Newton ---
ukri_gcrf_newton <- tableau_projects_tidied %>% 
  filter(Fund %in% c("Global Challenges Research Fund (GCRF)", "Newton Fund"),
         extending_org %in% c(
           "Arts and Humanities Research Council (AHRC)",
           "Biotechnology and Biological Sciences Research Council (BBSRC)",
           "Engineering and Physical Sciences Research Council (EPSRC)",
           "Economic and Social Research Council (ESRC)",
           "Medical Research Council (MRC)",
           "Natural Environment Research Council (NERC)",
           "Science and Technology Facilities Council (STFC)",
           "Innovate UK"
         )) %>% 
  select(id, amount) %>% 
  unique()

# Sum total award amounts
sum(ukri_gcrf_newton$amount, na.rm = TRUE)


### 2) UKRI - FCDO ---
ukri_fcdo <- tableau_projects_tidied %>% 
  filter(Funder == "Foreign, Commonwealth and Development Office",
        extending_org %in% c(
                  "Arts and Humanities Research Council (AHRC)",
                  "Biotechnology and Biological Sciences Research Council (BBSRC)",
                  "Engineering and Physical Sciences Research Council (EPSRC)",
                  "Economic and Social Research Council (ESRC)",
                  "Medical Research Council (MRC)",
                  "Natural Environment Research Council (NERC)",
                  "Science and Technology Facilities Council (STFC)",
                  "Innovate UK"
                )) %>% 
    select(id, amount) %>% 
    unique()

# Sum total award amounts
sum(ukri_fcdo$amount, na.rm = TRUE)


### 3) UKRI - DHSC ---
ukri_dhsc <- tableau_projects_tidied %>% 
           filter(Funder == "Department of Health and Social Care",
                  extending_org %in% c(
                    "Arts and Humanities Research Council (AHRC)",
                    "Biotechnology and Biological Sciences Research Council (BBSRC)",
                    "Engineering and Physical Sciences Research Council (EPSRC)",
                    "Economic and Social Research Council (ESRC)",
                    "Medical Research Council (MRC)",
                    "Natural Environment Research Council (NERC)",
                    "Science and Technology Facilities Council (STFC)",
                    "Innovate UK"
                  )) %>% 
           select(id, amount) %>% 
           unique()

# Sum total award amounts
sum(ukri_dhsc$amount, na.rm = TRUE)


### 3) FCDO Research & Evidence Division (non-UKRI, Wellcome)

fcdo_non_ukri_wellcome <- tableau_projects_tidied %>% 
  filter(Funder == "Foreign, Commonwealth and Development Office",
         !(extending_org %in% c(
           "Arts and Humanities Research Council (AHRC)",
           "Biotechnology and Biological Sciences Research Council (BBSRC)",
           "Engineering and Physical Sciences Research Council (EPSRC)",
           "Economic and Social Research Council (ESRC)",
           "Medical Research Council (MRC)",
           "Natural Environment Research Council (NERC)",
           "Science and Technology Facilities Council (STFC)",
           "Innovate UK",
           "Wellcome Trust"
         )), currency == "GBP"
         ) %>% 
  select(id, amount) %>% 
  unique()

sum(fcdo_non_ukri_wellcome$amount, na.rm = TRUE)


### 4) NIHR GHR Programmes ---
nihr_active <- tableau_projects_tidied %>% 
  filter(Fund == "Global Health Research - Programmes") %>% 
  select(id, amount) %>% 
  unique()

sum(nihr_active$amount)


### 5) NIHR GHR Partnerships


### 6) DHSC GAMRIF and UK Vaccine Network

ghs_awards <- tableau_projects_tidied %>% 
  filter(Fund %in% c("Global Health Security - UK Vaccine Network", "Global Health Security - GAMRIF")) %>% 
  select(id, amount) %>% 
  unique()

sum(ghs_awards$amount, na.rm = TRUE)


### 7) Wellcome

wellcome_grant_sum <- tableau_projects_tidied %>% 
  filter(str_detect(extending_org, "Wellcome"),
         amount > 0) %>% 
  select(id, amount) %>% 
  unique()

sum(wellcome_grant_sum$amount, na.rm = TRUE)


### UK Space Agency

# Check a delivery partner
uksa <- tableau_projects_tidied %>% 
  filter(extending_org == "UK Space Agency") %>% 
  select(id, amount) %>% 
  unique()

sum(uksa$amount, na.rm = TRUE)


### Met Office

mo <- tableau_projects_tidied %>% 
  filter(extending_org == "Met Office") %>% 
  select(id, amount) %>% 
  unique()

sum(mo$amount, na.rm = TRUE)


### British Council

bc <- tableau_projects_tidied %>% 
  filter(Funder == "Department for Business, Energy and Industrial Strategy",
         extending_org == "British Council") %>% 
  select(id, amount) %>% 
  unique()

sum(bc$amount, na.rm = TRUE)


### Academies

academies <- tableau_projects_tidied %>% 
  filter(Funder == "Department for Business, Energy and Industrial Strategy",
         extending_org %in% c("Royal Society", "Royal Academy of Engineering", "Academy of Medical Sciences", "British Academy")) %>% 
  select(id, amount) %>% 
  unique()

sum(academies$amount, na.rm = TRUE)


