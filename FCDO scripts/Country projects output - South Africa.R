# Extract South Africa projects

# 1) Narrow - where South Africa is the location of activity

narrow_south_africa_projects <- all_projects_final %>% 
  filter(Country == "South Africa",
         country_type == "partner_countries",
         status %in% c("Active", "Pipeline/identification")) %>% 
  rename(`Funder ID` = id,
         Title = title,
         `Start Date` = start_date,
         `End Date` = end_date,
         Status = status,
         `Extending Organisation` = extending_org,
         `Lead Organisation` = lead_org_name,
         `Lead Organisation Country` = lead_org_country,
         `Partner Organisation` = partner_org_name,
         `Partner Organisation Country` = partner_org_country,
         `Award amount (£)` = amount,
         `Abstract` = abstract,
         `Subject area` = subject,
         `Beneficiary Country` = recipient_country,
         `Link to data source` = link,
         `IATI ID` = iati_id
         ) %>% 
  select(`Funder ID`, Funder, Fund, Title, `Start Date`, `End Date`, Status,
         `Extending Organisation`,
         `Lead Organisation`, `Lead Organisation Country`,
         `Partner Organisation`, `Partner Organisation Country`,
         `Award amount (£)`, `Subject area`,
         Abstract, 
         `Beneficiary Country`, `Link to data source`, `IATI ID`)



# 2) Broad - where South Africa is referenced / a beneficiary / location of research

broad_south_africa_projects <- all_projects_final %>% 
  filter(Country == "South Africa",
         status %in% c("Active", "Pipeline/identification")) %>% 
  rename(`Funder ID` = id,
         Title = title,
         `Start Date` = start_date,
         `End Date` = end_date,
         Status = status,
         `Extending Organisation` = extending_org,
         `Lead Organisation` = lead_org_name,
         `Lead Organisation Country` = lead_org_country,
         `Partner Organisation` = partner_org_name,
         `Partner Organisation Country` = partner_org_country,
         `Award amount (£)` = amount,
         `Abstract` = abstract,
         `Subject area` = subject,
         `Beneficiary Country` = recipient_country,
         `Link to data source` = link,
         `IATI ID` = iati_id
  ) %>% 
  select(`Funder ID`, Funder, Fund, Title, `Start Date`, `End Date`, Status,
         `Extending Organisation`,
         `Lead Organisation`, `Lead Organisation Country`,
         `Partner Organisation`, `Partner Organisation Country`,
         `Award amount (£)`, `Subject area`,
         Abstract, 
         `Beneficiary Country`, `Link to data source`, `IATI ID`) %>% 
  unique()


# Write to Excel
write_xlsx(x = list(narrow_south_africa_projects, broad_south_africa_projects),
           path = "Outputs/ODA R&I awards - South Africa.xlsx")
