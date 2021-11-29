# Script to output Excel sheet of active projects in a country #

### 1) Set up ---

# Define countries for reports

## East Africa
# country_list <- c("Burundi", "Comoros", "Djibouti", "Ethiopia", "Eritrea", "Kenya",
#                   "Madagascar", "Malawi", "Mauritius", "Mozambique", "Réunion", "Rwanda", 
#                   "Seychelles", "Somalia", "Somaliland", "Tanzania", "Uganda", "Zambia", "Zimbabwe")

## Indo-Pacific
country_list <- c("Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines",
                  "Singapore", "Thailand", "Vietnam")


# Read in datasets and abbrieviate funder names

nihr_projects_final <- readRDS("Outputs/nihr_projects_final.rds") 

all_projects <- readRDS("Outputs/all_projects.rds") %>% 
  mutate(Funder = case_when(
    Funder == "Foreign, Commonwealth and Development Office" ~ "FCDO",
    Funder == "Department of Health and Social Care" ~ "DHSC",
    Funder == "Department for Business, Energy and Industrial Strategy" ~ "BEIS"
  ))

all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") %>% 
  mutate(Funder = case_when(
    Funder == "Foreign, Commonwealth and Development Office" ~ "FCDO",
    Funder == "Department of Health and Social Care" ~ "DHSC",
    Funder == "Department for Business, Energy and Industrial Strategy" ~ "BEIS"
  ),
    extending_org = case_when(
      extending_org == "BC" ~ "British Council",
      extending_org == "MO" ~ "Met Office",
      extending_org == "AMS" ~ "Academy of Medical Sciences",
      extending_org == "RS" ~ "Royal Society",
      extending_org == "BA" ~ "British Academy",
      extending_org == "RAE" ~ "Royal Academy of Engineering",
      extending_org == "UKSA" ~ "UK Space Agency"
    )) 


### 2) Output reports for each country ----

wb <- openxlsx::createWorkbook()

# Set header style (bold)
header_st <- createStyle(textDecoration = "Bold",
                         fontSize = 8, fontName = "Arial",
                         halign = "center")

# Set table style
table_st <- createStyle(fontSize = 8, fontName = "Arial", 
                        wrapText = TRUE,
                        valign = "top")

for(i in 1:length(country_list)) {

    print(paste0(i, " - ", country_list[i]))
  
    # (workaround until overseas partner info on NIHR Open Data)
    nihr_country_projects <- nihr_projects_final %>% 
      filter(str_detect(title, country_list[i]) | str_detect(abstract, country_list[i]),
             status == "Active")
  
    # Extract project data for selected country
    country_project_ids <- all_projects_tidied %>% 
      filter(Country == country_list[i] |
             id %in% nihr_country_projects$id) %>% 
      mutate(Start = as.numeric(as.character(format(as.Date(start_date), format = "%Y"))),
             End = as.numeric(as.character(format(as.Date(end_date), format = "%Y"))),
             link = coalesce(link, ""))
    
    output_report <- country_project_ids %>% 
      mutate(lead_org_name = coalesce(lead_org_name, extending_org)) %>% 
      select(Funder, Fund, Programme = funder_programme,
             Title = title, Start, End, Description = abstract,
             `Beneficiary Country` = recipient_country,
             `Lead Organisation` = lead_org_name, `Partner Organisations` = partner_org_name,
             `Value (£)` = amount,
             `Web Link` = link,
             currency) %>% 
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
      group_by(Title) %>% 
      # Keep one row per project title
      slice(1) %>% 
      select(Funder, Fund, Programme, Title, Start, End, Description, `Beneficiary Country`,
             `Lead Organisation`, `Partner Organisations`, `Value (£)`, `Web Link`, currency)
  
    # Format numerical value column
    class(output_report$`Value (£)`) <- "comma"
    
    # Add country dataset to output list
    openxlsx::addWorksheet(wb, sheetName = country_list[i])
    openxlsx::writeData(wb, sheet = i, x = output_report, 
                        headerStyle = header_st,
                        borderStyle = "thin")
    # Add font style
    addStyle(wb, sheet = i, table_st, rows = 2:200, cols = 1:12, gridExpand = TRUE, stack = TRUE)
    
    # Identify titles and hyperlinks
    hyperlinks <- output_report$`Web Link`
    names(hyperlinks) <- output_report$`Web Link`
    class(hyperlinks) <- "hyperlink"
    
    # Write hyperlinks
    writeData(wb, sheet = i, x = hyperlinks, startRow = 2, startCol = 12, colNames = FALSE)
    
    # Set column widths
    setColWidths(wb, sheet = i, cols = 1:12, widths = c(10,25,30,40,6,6,60,30,30,30,10,50))
    
}

# Resave Excel file
saveWorkbook(wb, "Outputs//Indo-Pacific ODA programmes - Nov21.xlsx", overwrite = TRUE)





