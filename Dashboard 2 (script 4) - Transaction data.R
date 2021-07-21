#####################################
# Script 4
# Extract transactions for activities covering multiple awards
#####################################

# A) Extraction ----
# Manually specify the IATI activities to extract transactions for

id_list <- c("US-EIN-042103594-PPE3978774",
             "US-EIN-042103594-GCCI-3978870",
             "GB-CHC-1177110-R2HC",
             "GB-CHC-1177110-HIF")

# Function to extract transactions for a specified activity

transactions_extract <- function(iati_id, page) {
  path <- paste0("https://iati.cloud/api/transactions/?iati_identifier=", iati_id, "&fields=value,transaction_date,description,currency,receiver_organisation&format=json&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results

  results <- plyr::rbind.fill(transaction_list, new_data)
  
  return(results)
}

# Prepare results data frame and counters
transaction_list <- data.frame()
new_rows <- 0
page <- 1

# Run extraction, stopping when no new transactions are returned

for (id in id_list) {
  new_rows <- 0
  page <- 1
  
  while (page == 1 | new_rows > 0) {
    print(paste0(id, "-", page))
    x <- nrow(transaction_list)
    transaction_list <- transactions_extract(id, page)
    page <- page + 1
    y <- nrow(transaction_list)
    new_rows = y - x
  }
}


# B) Unnest information -----

transactions_unnest <- transaction_list %>% 
  rename(currency = currency.code) %>% 
  
  # 1 - Country
  unnest(cols = recipient_countries,
         keep_empty = TRUE) %>%
  select(-country.url, -country.code, -recipient_countries, -recipient_regions) %>% 
  rename(recipient_country = country.name) %>% 
  
  # 2 - Description
  unnest(cols = description.narrative,
         keep_empty = TRUE) %>%
  select(-lang.code, -lang.name) %>% 
  rename(title = text) %>% 
  
  # 3 - Sectors
  unnest(cols = sectors,
         keep_empty = TRUE) %>%
  rename(subject = sector.name) %>% 
  
  # 4 - Receiver org
  unnest(cols = receiver_organisation.narrative,
         keep_empty = TRUE) %>%
  mutate(lead_org_name = coalesce(text, receiver_organisation.ref)) %>% 
  filter(!is.na(lead_org_name)) %>%   # remove transactions without a recipient
  filter(!(lead_org_name == "Elrha")) %>%  # remove Elrha income
  
  # select fields to keep
  select(iati_identifier, start_date = transaction_date, 
         title, recipient_country, subject, currency, amount = value, lead_org_name) %>% 
  mutate(partner_org_name = "",
         partner_org_country = "",
         lead_org_country = "",
         end_date = "")

# C) Join information back to master dataset ----

# Read in R file (from previous script)
all_projects <- readRDS("Outputs/all_projects.rds") 

# Subset for transaction based activities
all_projects_subset <- all_projects %>% 
  filter(id %in% id_list) %>% 
  select(id, abstract, extending_org, iati_id, Fund, Funder, status, link) %>% 
  left_join(transactions_unnest, by = c("id" = "iati_identifier"))

# Replace original single line activities in master file
all_projects_transactions <- all_projects %>% 
  filter(!(id %in% id_list)) %>%
  rbind(all_projects_subset)

# D) Save information ----

saveRDS(all_projects_transactions, file = "Outputs/all_projects_transactions.rds")
# Restore the object
# all_projects_transactions <- readRDS(file = "Outputs/all_projects_transactions.rds")

# E) Check ----

test <- filter(all_projects_transactions, str_detect(extending_org, "Elrha"))
