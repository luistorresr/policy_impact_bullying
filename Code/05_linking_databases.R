
load("./Data/Working_data/policy_ready.rda") 
load("./Data/Working_data/practices_ready.rda") 
load("./Data/Working_data/JDR_clean.rda") 


## Selecting only stress relevant variables 

### from policy index (indirect = yes) 

policy_bullying <- policy_ready %>% select(country, law_bullying_yes) 

rm(policy_ready)

policy_bullying %>% arrange(country) %>% group_by(as_label(country)) %>% print(n=35)

### from practices_ready

practices_bullying <- practices_ready

rm(practices_ready)

### from JDR clean

JDR_scores <- JDR_clean %>% as_tibble()

rm(JDR_clean)


## Calculating country index for stress practices in the ESENER

practices_bullying <- practices_bullying  %>% rename(size = size_esener) # change size name

practices_country <- practices_bullying %>% 
  group_by(country) %>%
  summarise(country_index = mean(na.omit(practice_bullying)))  # per country

practices_industry <- practices_bullying  %>% 
  group_by(country, industry) %>%
  summarise(industry_index = mean(na.omit(practice_bullying))) # per country and industry

practices_size <- practices_bullying  %>% 
  group_by(country, industry, size) %>%
  summarise(csize_index = mean(na.omit(practice_bullying)))  # per country, industry and size


## Joining tables with replacement of NA following if industry NA = country; if size NA = industry.

practices_index_temp <- left_join(practices_country, practices_industry, by = "country")

practices_index_temp$industry_index <- ifelse(is.na(practices_index_temp$industry_index), 
                                          practices_index_temp$country_index, practices_index_temp$industry_index) # replace "NA" in industry


practices_index <- left_join(practices_index_temp, practices_size, by = c("country", "industry"))

practices_index$csize_index <- ifelse(is.na(practices_index$csize_index), 
                                          practices_index$industry_index, practices_index$csize_index) # replace "NA" in industry


### linking practices and JDR by country, industry and size

JDR_scores <- JDR_scores %>% rename(size = size_ewcs)

class(practices_index$country)
class(JDR_scores$country) <- "numeric"
class(practices_index$industry) <- "numeric"
class(JDR_scores$industry) <- "numeric"

#practices_index$country <- as_labelled(practices_index$country) # change the type so they can be linked

comb_JDR_practice <- left_join(JDR_scores, practices_index, by = c("country", "industry", "size"))


### linking combined practices and JDR dataset with policy table

combined_bullying <- left_join(comb_JDR_practice, policy_bullying, by = c("country")) # this is the working dataset for the stress outcome analysis

l_combined_bullying <- get_labels(combined_bullying, values = "n") # value labels


### saving the final working database

save(combined_bullying, file = "./Data/Working_data/combined_bullying.rda") # r data
write.xlsx(combined_bullying, file = "./Data/Working_data/combined_bullying.xlsx")

capture.output(l_combined_bullying, file = "./Data/Working_data/l_combined_bullying.txt") # variable labels

### delete workspace

rm(list = ls())
