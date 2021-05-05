# Additional formatting and analysis of IATI data 

if (!("textrank" %in% installed.packages())) {
  install.packages("textrank")
}
if (!("udpipe" %in% installed.packages())) {
  install.packages("udpipe")
}

library(textrank)
library(udpipe)
library(tidyverse)

# Download pre-trained model for English language tagging 
tagger <- udpipe_download_model("english")
tagger <- udpipe_load_model(tagger$file_model)
saveRDS(tagger, file = "tagger.rds")
#tagger <- readRDS(file = "tagger.rds")

# Read in list of IATI activities
activity_list <- readRDS(file = "activity_list.rds")

# Extract keywords from activity title/description
test_keywords <- activity_list %>%
                     head(2) %>% 
                     mutate(keywords = map(activity_description, udpipe_annotate, object = tagger)) %>% 
                     mutate(keywords = map(keywords, as.data.frame)) %>% 
                     mutate(lemma = map(keywords, select, lemma),
                            upos = map(keywords, select, upos)) %>% 
                     mutate(keywords2 = list(textrank_keywords(x = lemma,
                                                          relevant = upos %in% c("NOUN", "VERB", "ADJ", "PROPN")))) %>% 
  
                     mutate(keywords2 = list(subset((keywords2[[1]])$keywords, 
                                               ngram > 1 & freq > 1 | freq > 1))) %>% 
                     unnest(cols = keywords2) %>% 
                     group_by(iati_identifier) %>% 
                     summarise(words = paste(coalesce(keyword, ""), collapse = ", "))
  

                                            
description <- test_keywords$keywords[[1]] 
  
result <- textrank_keywords(x = description$lemma,
                    relevant = description$upos %in% c("NOUN", "VERB", "ADJ", "PROPN"))
  


paste(collapse = "\n")

description <- udpipe_annotate(tagger, test_keywords)
description <- as.data.frame(description)

# Apply keyword relevance extractor
keyw <- textrank_keywords(description$lemma,
                          relevant = description$upos %in% c("NOUN", "VERB", "ADJ", "PROPN"))
subset(keyw$keywords, 
       ngram > 1 & freq > 1 | freq > 4)

