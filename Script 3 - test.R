#testing ideas for Script 3 activity_list_unnest_4 where i am having trouble matching up org narratives and roles


activity_list_unnest_4test <- activity_list_unnest_4 %>%
  separate_rows(participating_org_role, sep = ";") %>%
  separate_rows(participating_org_narrative, sep = ";")

activity_list_unnest_4test$participating_org_role <- trimws(activity_list_unnest_4test$participating_org_role)
activity_list_unnest_4test$participating_org_narrative <- trimws(activity_list_unnest_4test$participating_org_narrative)

activity_list_unnest_4 <- activity_list_unnest_4test %>% 
  filter(participating_org_role == "4") %>% 
  unique()

activity_list_unnest_4_test2 <- partner_activity_comb %>%
  select(iati_identifier, participating_org_narrative_xml_lang, participating_org_narrative,participating_org_role)

activity_list_unnest_4_test2$participating_org_narrative_xml_lang <- sapply(activity_list_unnest_4_test2$participating_org_narrative_xml_lang, paste, collapse = ";")
activity_list_unnest_4_test2$participating_org_narrative <- sapply(activity_list_unnest_4_test2$participating_org_narrative, paste, collapse = ";")
activity_list_unnest_4_test2$participating_org_role <- sapply(activity_list_unnest_4_test2$participating_org_role, paste, collapse = ";")

activity_list_unnest_4_test2$narrativecount <- str_count(activity_list_unnest_4_test2$participating_org_narrative, ";")
activity_list_unnest_4_test2$rolecount <- str_count(activity_list_unnest_4_test2$participating_org_role, ";")
activity_list_unnest_4_test2$Problem <- ifelse(activity_list_unnest_4_test2$rolecount == activity_list_unnest_4_test2$narrativecount, "Fine", "Problem")
unique(activity_list_unnest_4_test2[activity_list_unnest_4_test2$Problem=="Problem",]$iati_identifier)
activity_list_unnest_4_test2_problem <- activity_list_unnest_4_test2[activity_list_unnest_4_test2$Problem=="Problem",]


activity_list_unnest_4_test3 <- partner_activity_comb %>%
  select(iati_identifier, reporting_org_ref, participating_org_narrative_xml_lang, participating_org_narrative,participating_org_role) %>%
  filter(reporting_org_ref == "XM-DAC-301-2")

activity_list_unnest_4_test3$participating_org_narrative_xml_lang <- data.frame(lapply(activity_list_unnest_4_test3$participating_org_narrative_xml_lang, as.character), stringsAsFactors = FALSE)

lang_indices <- activity_list_unnest_4_test3$participating_org_narrative[activity_list_unnest_4_test3$participating_org_narrative_xml_lang %in% "en"]
lang_indices <- which(activity_list_unnest_4_test3$participating_org_narrative_xml_lang == "en")


select_elements <- function(row) {
  en_indices <- which(row[["participating_org_narrative_xml_lang"]] == "en")
  row[["participating_org_narrative"]][en_indices]
}

selected_elements <- apply(activity_list_unnest_4_test3, 1, select_elements)
print(selected_elements)


# Hannes's function idea
# takes a row number, processes row from activity_list_unnest_4_test3 dataframe
# returns English lang narrative for role = 4 (implementing partner)

process_line <- function(l) {
  # these are strings that look like vectors
  langs <- activity_list_unnest_4_test3$participating_org_narrative_xml_lang[l]
  narrs <- activity_list_unnest_4_test3$participating_org_narrative[l]
  roles <- activity_list_unnest_4_test3$participating_org_role[l]
  # overwrite strings with actual vectors
  langs <- eval(str2expression(langs))
  narrs <- eval(str2expression(narrs))
  if(length(langs) != length(narrs)) stop(paste0("line ",l, ", langs and narrs must be equal length"))
  roles <- eval(str2expression(roles))
  
  # English lang narratives only
  narrs_en <- narrs[langs=="en"]
  if(length(roles) != length(narrs_en)) stop(paste0("line ",l, ", roles and narrs_en must be equal length"))
  # print(narrs_en)
  return(narrs_en[roles=="4"])
}

sapply(c(1:6,8:3960), process_line)

eval(str2expression("c(1,2,3)"))