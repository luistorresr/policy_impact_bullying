
# 4 # filtering datasets

load(file = "./Data/Working_data/ESENER14_items.rda") 
l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels

load(file = "./Data/Working_data/EWCS2015_items.rda") 
l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels


### filtering databases by size and employment criteria 

#### ESENER

ESENER14_filtered <- ESENER14_items %>% filter(size_esener != 1) # equal or more than 10 employees

summary_esener <- ESENER14_filtered %>% 
  group_by(as_label(country), as_label(size_esener)) %>% summarise(total = n()) %>% 
  pivot_wider(names_from = `as_label(size_esener)`, values_from = total) %>%
  adorn_totals("col") %>%
  rename(country = `as_label(country)`)


#### EWCS 

EWCS2015_filtered <- EWCS2015_items %>% filter(size_ewcs != 1) # more than 10 employees company size

EWCS2015_filtered <- EWCS2015_filtered %>% filter(employment_status == 1 # in employment
                                                    & worker_type == 1 # employee
                                                  ) 

summary_ewcs <- EWCS2015_filtered %>% group_by(as_label(country), as_label(size_ewcs)) %>% 
  summarise(total = n()) %>%
  pivot_wider(names_from = `as_label(size_ewcs)`, values_from = total) %>%
  adorn_totals("col") %>%
  rename(country = `as_label(country)`) %>%
  filter(!is.na(country))

summary_ewcs_sex <- EWCS2015_filtered %>% filter(!is.na(sex) & !is.na(country)) %>% 
  group_by(as_label(country), as_label(sex)) %>% 
  summarise(total = n()) %>%
  pivot_wider(names_from = `as_label(sex)`, values_from = total) %>%
  adorn_totals("col") %>%
  rename(country = `as_label(country)`) %>%
  mutate(Female_percent = (Female / Total) *100, Male_percent = (Male / Total) *100) %>%
  select(country, Female_percent, Male_percent)

summary_ewcs_contract <- EWCS2015_filtered %>% filter(!is.na(contract) & !is.na(country)) %>% 
  group_by(as_label(country), as_label(contract)) %>% 
  summarise(total = n()) %>%
  pivot_wider(names_from = `as_label(contract)`, values_from = total) %>%
  adorn_totals("col") %>%
  rename(country = `as_label(country)`) %>%
  mutate(part_percent = (`Part time` / Total) *100, full_percent = (`Full time` / Total) *100) %>%
  select(country, part_percent, full_percent)

summary_ewcs_age <- EWCS2015_filtered %>% filter(!is.na(age) & !is.na(country)) %>% 
  group_by(as_label(country)) %>% 
  summarise(mean_age = mean(age), sd_age = sd(age)) %>% 
  rename(country = `as_label(country)`)

summary_ewcs_hours <- EWCS2015_filtered %>% filter(!is.na(hours) & !is.na(country)) %>% 
  group_by(as_label(country)) %>% 
  summarise(mean_hours = mean(hours), sd_hours = sd(hours)) %>%
  rename(country = `as_label(country)`)

EWCS2015_filtered %>% count(sex) %>% summarise(percentage = n / sum(n) * 100) # sex percentage

EWCS2015_filtered %>% filter(!is.na(age)) %>% summarise(mean_age = mean(age), sd_age = sd(age)) # sex percentage


### combined summary table

summary <- left_join(summary_esener, summary_ewcs, by = "country")
summary <- left_join(summary, summary_ewcs_sex, by = "country")
summary <- left_join(summary, summary_ewcs_age, by = "country")
summary <- left_join(summary, summary_ewcs_hours, by = "country")
summary <- left_join(summary, summary_ewcs_contract, by = "country")

write.xlsx(summary, file = "./Data/Working_data/data_summary.xlsx")


### saving ready to link datasets


JDR_ready <- EWCS2015_filtered

practices_ready <- ESENER14_filtered

save(JDR_ready, file = "./Data/Working_data/JDR_ready.rda") # r data for CFA

save(practices_ready, file = "./Data/Working_data/practices_ready.rda") # r data

# End data wrangling 

rm(list = ls())

