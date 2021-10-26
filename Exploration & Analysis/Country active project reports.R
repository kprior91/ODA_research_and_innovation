# Script to output Excel sheet of active projects in a country #

### 1) Set up ---

# Define countries for reports
country_list <- c("Burundi", "Comoros", "Djibouti", "Ethiopia", "Eritrea", "Kenya",
                  "Madagascar", "Malawi", "Mauritius", "Mozambique", "Réunion", "Rwanda", 
                  "Seychelles", "Somalia", "Somaliland", "Tanzania", "Uganda", "Zambia", "Zimbabwe")

# Read in datasets
all_projects <- readRDS("Outputs/all_projects.rds") 
all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 


### 2) Output reports for each country ----

wb <- openxlsx::createWorkbook()

for(i in 1:length(country_list)) {

    print(paste0(i, " - ", country_list[i]))
  
    # Extract project data for selected country
    country_project_ids <- all_projects_tidied %>% 
      filter(Country == country_list[i]) %>% 
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
             ),
             link = coalesce(link, "https://devtracker.fcdo.gov.uk")) %>% 
      select(Funder, Fund, Title = title, Start, End, Description = abstract,
             `Lead Organisation` = lead_org_name, `Partner Organisations` = partner_org_name,
             `Web Link` = link) %>% 
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
    
    # Add summarised funders to main report
    output_report <- output_report %>% 
      left_join(co_funded_projects, by = "Title") %>% 
      mutate(Funder = coalesce(comb_funder, Funder)) %>% 
      select(-comb_funder) %>% 
      group_by(Title) %>% 
      # Keep one row per project title
      slice(1)
  
    # Add country dataset to output list
    openxlsx::addWorksheet(wb, sheetName = country_list[i])
    openxlsx::writeData(wb, sheet = i, x = output_report)
    
    # Identify titles and hyperlinks
    hyperlinks <- output_report$`Web Link`
    names(hyperlinks) <- output_report$Title
    class(hyperlinks) <- "hyperlink"
    
    # Write hyperlinks
    writeData(wb, sheet = i, x = hyperlinks, startRow = 2, startCol = 3, colNames = FALSE)
    
    # Set column widths
    setColWidths(wb, sheet = i, cols = 1:8, widths = c(8,25,40,6,6,40,30,30))
    
}

# Resave Excel file
saveWorkbook(wb, "Outputs//East Africa ODA programmes - Oct21.xlsx", overwrite = TRUE)






