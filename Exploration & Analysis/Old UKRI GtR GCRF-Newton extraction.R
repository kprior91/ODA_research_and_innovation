# Create empty dataset to hold projects
ukri_projects_by_fund <- data.frame()

for (fund in c("GCRF", "Newton")) {
  
  for (page in c(1:15)) {
    
    projects <- extract_ukri_projects_by_fund(page, fund)
    
    # Check if data exists, label country column and append to master
    # project list if so
    if(!is.null(nrow(projects))) {
      (Fund = fund,
       Funder = "Department for Business, Energy and Industrial Strategy")
      
      if("participantValues" %in% names(projects)) {
        projects <- projects %>% 
          select(-participantValues)
      }
      
      if(!("participantValues.participant" %in% names(projects))) {
        projects <- projects %>% 
          mutate(participantValues.participant = "")
      }   
      
      ukri_projects_by_fund <- ukri_projects_by_fund %>% 
        rbind(projects)
      
    } 
    
  }
}

saveRDS(ukri_projects_by_fund, file = "Outputs/ukri_projects_by_fund.rds")
# ukri_projects_by_fund <- readRDS("Outputs/ukri_projects_by_fund.rds") 