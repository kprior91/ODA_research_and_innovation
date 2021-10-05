###
# Script to output list of RED programmes that involve an Indo-Pacific country
###

# Read in project dataset
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 

# Output list of Countries to check formats/spelling
countries <- select(all_projects_tidied, Country) %>%  unique() %>% arrange(Country)
View(countries)

# Define list of Indo-Pacific countries

indo_pac_countries <- c("Bangladesh", 
                        "Cambodia",
                        "China",
                        "India",
                        "Indonesia",
                        "Laos",
                        "Malaysia",
                        "Maldives",
                        "Myanmar",
                        "Naura",
                        "Nepal",
                        "Samoa",
                        "Solomon Islands",
                        "Sri Lanka",
                        "Thailand",
                        "Tonga",
                        "Vietnam",
                        "Philippines",
                        "Mongolia",
                        "Fiji",
                        "Papua New Guinea",
                        "Vanuatu")

                      
# Filter for FCDO programmes
fcdo_indo_activities <- all_projects_tidied %>% 
  filter(Country %in% indo_pac_countries,
         Funder == "Foreign, Commonwealth and Development Office") %>% 
  select(-row_id, -Country, -country_type) %>% 
  unique()


write.csv(fcdo_indo_activities, "fcdo_indo_activities.csv")



activity_check <- all_projects_tidied %>% 
  filter(fcdo_programme_id %in% fcdo_indo_activities$fcdo_programme_id) %>% 
  select(fcdo_programme_id, country_type, Country) %>% 
  unique() %>% 
  group_by(fcdo_programme_id, country_type) %>% 
  summarise(Country = paste(Country, collapse = ", ")) %>% 
  filter(fcdo_programme_id != "") %>% 
  arrange(1,2,3)

write.csv(nihr_projects, "nihr_projects.csv")

