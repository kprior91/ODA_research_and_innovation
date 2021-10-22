# Script to output Excel sheet of active projects in a country #

# Define countries for reports
country_list <- c("Kenya", "Uganda")

# Read in datasets
all_projects <- readRDS("Outputs/all_projects.rds") 
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 

# Extract project data for each country
country_project_ids <- all_projects_tidied %>% 
  filter(Country == country_list[1]) %>% 
  select(id) %>% 
  unique()

country_project_data <- all_projects %>% 
  filter(id %in% country_project_ids$id)

# Prepare output report fields
output_report <- country_project_data %>% 
  mutate(Start = format(as.Date(start_date), format = "%Y"),
         End = format(as.Date(end_date), format = "%Y"),
         Funder = case_when(
           Funder == "Foreign, Commonwealth and Development Office" ~ "FCDO",
           Funder == "Department of Health and Social Care" ~ "DHSC",
           Funder == "Department for Business, Energy and Industrial Strategy" ~ "BEIS"
         )) %>% 
  select(Funder, Fund, Title = title, Start, End, Description = abstract,
         `Lead Organisation` = lead_org_name, `Partner Organisations` = partner_org_name) %>% 
  group_by(Title, Funder) %>% 
  slice(1) %>% 
  ungroup() %>% 
  unique()

# Summarise funders on co-funded projects
co_funded_projects <- output_report %>% 
  group_by(Title) %>% 
  summarise(n = n(),
            comb_funder = paste(coalesce(Funder, ""), collapse = ", ")) %>% 
  filter(n > 1) %>% 
  select(-n)

output_report <- output_report %>% 
  left_join(co_funded_projects, by = "Title") %>% 
  mutate(Funder = coalesce(comb_funder, Funder)) %>% 
  group_by(Title) %>% 
  slice(1)
  


all_outputs <- list(Kenya = output_report, 
                    Uganda = output_report)

# Write to Excel

write_xlsx(all_outputs, path = "Outputs//Active ODA programmes - Oct21.xlsx")
