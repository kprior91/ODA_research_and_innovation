# --------------------------------------------------------------- #
# Script 10
# Format prev dataset for MODARI page archive
# --------------------------------------------------------------- #

# Read in the last dataset iteration

tableau_projects_tidied <- readRDS("Outputs/tableau_projects_tidied.rds")

tableau_projects_tidied_dwnld <- tableau_projects_tidied %>% 
  select(!country_type) %>%
  unique() %>%
  group_by(id) %>%
  summarise(Country = paste(coalesce(Country, ""), collapse = ", "))

tableau_projects_tidied_tosave <- tableau_projects_tidied %>%
  select(-country_type, -Country) %>%
  unique() %>%
  left_join(tableau_projects_tidied_dwnld, by = "id") %>%
  rename(`Country (beneficiary plus activity location)` = Country, `Beneficiary country` = recipient_country)

write.xlsx(tableau_projects_tidied_tosave, file = "Outputs/ARCHIVE_Dec21_tableau_projects_tidied.xlsx")
