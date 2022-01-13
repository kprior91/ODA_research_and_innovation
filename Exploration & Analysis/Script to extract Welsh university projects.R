tableau_projects_tidied <- readRDS("Outputs/tableau_projects_tidied.rds") 

welsh_projects <- tableau_projects_tidied %>% 
  mutate(orgs = paste0(partner_org_name, " ", lead_org_name)) %>% 
  select(-country_type, -Country) %>% 
  unique() %>% 
  filter(Funder == "Foreign, Commonwealth and Development Office",
         str_detect(orgs, "Aberystwyth|Bangor|Cardiff|Swansea|Wales"))

write_csv(welsh_projects, "welsh_red_projects.csv")
