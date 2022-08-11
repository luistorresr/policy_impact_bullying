
# 1 # Cleaning the datasets: ESENER 2015 and EWCS2015
# 2 # Creating a policy dataset
# 3 # Standardising relevant variables for linking datasets


# 1 # Cleaning the datasets: ESENER 2015 and EWCS2015


## EWCS2015 - for JDR model 

### loading the original dataset 

EWCS2015_base <- read_spss("./Data/UKDA-8098-spss/spss/spss19/ewcs6_2015_ukda_1904.sav")

l_EWCS2015_base <- get_labels(EWCS2015_base, values = "n") # value labels
q_EWCS2015_base <- as.data.frame(label(EWCS2015_base))

EWCS2015_base %>% nrow() # number of cases 

### items 

items_EWCS2015 <- c(
  
  # filtering variables
  
  "Q16b", # full company size (na 88, 99)
  "nace_rev2_1", # industry 
  
  "Q2a", # gender (dont know 9)
  "Q2b", # age as a numeric value (na 888, 999)
  "Q2c", # filter for 1 = in employment (na 88, 99)
  "Q7", # filter for 1 = employee (na 8 and 9)
  "Q2d", # 1 = full time, 2 = part-time (na 8 and 9)
  "Q24", # hours  (na 888, 999)
  
  # Linking variables 
  
  "Country", # country 

  # Job Demands
  
  # Emotional demands
  
  "Q30g", # Handling angry clients
  "Q30h", # Emotionally disturbing situations 
  "Q61o", # Your job requires that you hide your feelings?
  
  # Quantitative demands 
  
  "Q49a", # Working at very high speed
  "Q49b", # Working to tight deadlines 
  "Q61g", # You have enough time to get the job done?
  "Q51", # How often do you have to interrupt a task you are doing in order to take on an unforeseen task?
  
  # Pace determinants 
  "Q50a", 
  "Q50b",
  "Q50c",
  "Q50d",
  "Q50e",
  
  # Job resources
  
  # Social resources
  
  "Q61a", # Colleague social support index
  "Q70e", 
  "Q89d", 
  "Q61b", # Supervisor social support index
  "Q63a",
  "Q63b",
  "Q63c",
  "Q63d",
  "Q63e",
  "Q63f",
  
  # Work resources
  
  "Q54a", # Job control index 
  "Q54b", 
  "Q54c",
  "Q61f",
  "Q53c", # Skill discretion index
  "Q53e",
  "Q53f",
  "Q61i", 
  "Q61c", # Participation index
  "Q61d",
  "Q61n",
  
  # outcome 
  
  "Q81c" # experience bullying/ harassment  
  )

### select variables from EWCS 2015

EWCS2015_items <- EWCS2015_base %>% select(all_of(items_EWCS2015)) %>% as_tibble()

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels
q_EWCS2015_items <- as.data.frame(label(EWCS2015_items))

## cleaning the variables

EWCS2015_sub0 <- EWCS2015_items %>% select(Country, nace_rev2_1, Q24, Q2b) %>%
  set_na(na = c(Q24 = c(888, 999), Q2b = c(888, 999))) %>%
  as_tibble(.)

EWCS2015_sub1 <- EWCS2015_items %>% select(-c(Country, nace_rev2_1, Q24, Q2b, Q30g, Q30h, Q49a, Q49b)) %>%
  set_na(na =c(8, 9), drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)

EWCS2015_sub1 <- EWCS2015_sub1 %>% 
  set_na(na =c(Q61o = 7, Q61g = 7, Q61a = 7, Q70e = 7, Q89d = 7, Q61b = 7, Q63a = 7,
               Q63b = 7, Q63c = 7, Q63d = 7, Q63e = 7, Q63f = 7, Q61f = 7, Q61i = 7,
               Q61c = 7, Q61d = 7, Q61n = 7, Q61m = 7,
               Q50a = 7 , Q50b = 7, Q50c = 7, Q50d = 7, Q50e = 7,
               Q16b = c(88, 99), Q2c = c(88, 99)), 
               drop.levels = TRUE, as.tag = FALSE) %>% as_tibble(.)

EWCS2015_sub2 <- EWCS2015_items %>% 
  select(Q30g, Q30h, Q49a, Q49b) %>% 
  set_na(na =c(8, 9), drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)


## working dataset

EWCS2015_items <- cbind(EWCS2015_sub0, EWCS2015_sub1, EWCS2015_sub2) # working dataset

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels
q_EWCS2015_items <- as.data.frame(label(EWCS2015_items))


### Recode variables - the higher, the more

#### outcomes = Bullying/ harassment

get_label(EWCS2015_items$Q81c) # survey question
get_labels(EWCS2015_items$Q81c) # question options
describe(EWCS2015_items$Q81c) # descriptives
sum(is.na(EWCS2015_items$Q81c)) # total NA
EWCS2015_items %>% group_by(as_label(Q81c)) %>% summarise(Total = n()) # Warning = data is to0 unbalanced 

class(EWCS2015_items$Q81c) <- "numeric"

EWCS2015_items$Q81c <- dplyr::recode(EWCS2015_items$Q81c, `1` = 100, `2` = 0,) %>% 
  replace_labels(EWCS2015_items$Q81c, labels = c("Yes" = 100,
                                                 "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# 

##### rename the outcomes variables

EWCS2015_items <- EWCS2015_items %>% rename(bullying = Q81c)
EWCS2015_items$bullying

#### recode and rename gender, age, size, sector, and job type

EWCS2015_items  <- EWCS2015_items %>% rename(country = Country, industry = nace_rev2_1, size_ewcs = Q16b, 
                                             sex = Q2a, age = Q2b, 
                                             employment_status = Q2c, worker_type = Q7, contract = Q2d, hours = Q24) 

class(EWCS2015_items$sex) <- "numeric"

EWCS2015_items$sex <- dplyr::recode_factor(EWCS2015_items$sex, `1` = 0, `2` = 1) %>% 
  replace_labels(EWCS2015_items$sex, labels = c("Female" = 1, "Male" = 0)) %>% as_numeric(., drop.levels = TRUE)  # female = 1

class(EWCS2015_items$contract) <- "numeric"
EWCS2015_items$contract <- dplyr::recode_factor(EWCS2015_items$contract, `1` = 0, `2` = 1) %>% 
  replace_labels(EWCS2015_items$contract, labels = c("Full time" = 1, "Part time" = 0)) %>% as_numeric(., drop.levels = TRUE)  # full time = 1

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # update value labels


#### change variable class for items

class(EWCS2015_items$Q30g) <- "numeric"
class(EWCS2015_items$Q30h) <- "numeric"
class(EWCS2015_items$Q61o) <- "numeric"
class(EWCS2015_items$Q49a) <- "numeric"
class(EWCS2015_items$Q49b) <- "numeric"
class(EWCS2015_items$Q61g) <- "numeric"
class(EWCS2015_items$Q51) <- "numeric"
class(EWCS2015_items$Q50a) <- "numeric"
class(EWCS2015_items$Q50b) <- "numeric"
class(EWCS2015_items$Q50c) <- "numeric"
class(EWCS2015_items$Q50d) <- "numeric"
class(EWCS2015_items$Q50e) <- "numeric"
class(EWCS2015_items$Q61a) <- "numeric"
class(EWCS2015_items$Q70e) <- "numeric"
class(EWCS2015_items$Q89d) <- "numeric"
class(EWCS2015_items$Q61b) <- "numeric"
class(EWCS2015_items$Q63a) <- "numeric"
class(EWCS2015_items$Q63b) <- "numeric"
class(EWCS2015_items$Q63c) <- "numeric"
class(EWCS2015_items$Q63d) <- "numeric"
class(EWCS2015_items$Q63e) <- "numeric"
class(EWCS2015_items$Q63f) <- "numeric"
class(EWCS2015_items$Q54a) <- "numeric"
class(EWCS2015_items$Q54b) <- "numeric"
class(EWCS2015_items$Q54c) <- "numeric"
class(EWCS2015_items$Q61f) <- "numeric"
class(EWCS2015_items$Q53c) <- "numeric"
class(EWCS2015_items$Q53e) <- "numeric"
class(EWCS2015_items$Q53f) <- "numeric"
class(EWCS2015_items$Q61i) <- "numeric"
class(EWCS2015_items$Q61c) <- "numeric"
class(EWCS2015_items$Q61d) <- "numeric"
class(EWCS2015_items$Q61n) <- "numeric"

#### job  resources

##### work resources 
###### job control index

EWCS2015_items$Q54a <- dplyr::recode_factor(EWCS2015_items$Q54a, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54a, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q54b <- dplyr::recode_factor(EWCS2015_items$Q54b, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54b, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q54c <- dplyr::recode_factor(EWCS2015_items$Q54c, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q61f <- dplyr::recode_factor(EWCS2015_items$Q61f, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61f, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Skill discretion index
EWCS2015_items$Q53c <- dplyr::recode_factor(EWCS2015_items$Q53c, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q53e <- dplyr::recode_factor(EWCS2015_items$Q53e, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53e, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q53f <- dplyr::recode_factor(EWCS2015_items$Q53f, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53f, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q61i <- dplyr::recode_factor(EWCS2015_items$Q61i, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61i, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                  "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Participation index
EWCS2015_items$Q61c <- dplyr::recode_factor(EWCS2015_items$Q61c, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61c, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61d <- dplyr::recode_factor(EWCS2015_items$Q61d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61d, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61n <- dplyr::recode_factor(EWCS2015_items$Q61n, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61n, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### social resources
###### Colleague social support index
EWCS2015_items$Q61a <- dplyr::recode_factor(EWCS2015_items$Q61a, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61a, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q70e <- dplyr::recode_factor(EWCS2015_items$Q70e, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q70e, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q89d <- dplyr::recode_factor(EWCS2015_items$Q89d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q89d, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Supervisor social support index
EWCS2015_items$Q61b <- dplyr::recode_factor(EWCS2015_items$Q61b, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61b, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63a <- dplyr::recode_factor(EWCS2015_items$Q63a, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63a, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63b <- dplyr::recode_factor(EWCS2015_items$Q63b, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63b, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63c <- dplyr::recode_factor(EWCS2015_items$Q63c, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63c, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                  "Tend to disagree" = 25, 
                                                  "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63d <- dplyr::recode_factor(EWCS2015_items$Q63d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63d, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63e <- dplyr::recode_factor(EWCS2015_items$Q63e, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63e, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63f <- dplyr::recode_factor(EWCS2015_items$Q63f, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63f, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

#### job demands
##### emotional demands
EWCS2015_items$Q30g <- dplyr::recode_factor(EWCS2015_items$Q30g, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q30g, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q30h <- dplyr::recode_factor(EWCS2015_items$Q30h, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q30h, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61o <- dplyr::recode_factor(EWCS2015_items$Q61o, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61o, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### Quantitative demands 
EWCS2015_items$Q49a <- dplyr::recode_factor(EWCS2015_items$Q49a, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q49a, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q49b <- dplyr::recode_factor(EWCS2015_items$Q49b, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q49b, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)


EWCS2015_items$Q51 <- dplyr::recode_factor(EWCS2015_items$Q51, `1` = 100, `2` = 70, `3` = 30, `4` = 0) %>% 
  replace_labels(EWCS2015_items$Q51, labels = c("Very often" = 100, 
                                                "Fairly often" = 70, 
                                                "Occasionally" = 30, 
                                                "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61g <- dplyr::recode_factor(EWCS2015_items$Q61g, `1` = 0, `2` = 25, `3` = 50, `4` = 75, `5` = 100) %>% 
  replace_labels(EWCS2015_items$Q61g, labels = c("Never" = 100, 
                                                 "Rarely" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Most of the time" = 25, 
                                                 "Always" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### Pace determinats
EWCS2015_items$Q50a <- dplyr::recode_factor(EWCS2015_items$Q50a,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50a, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50b <- dplyr::recode_factor(EWCS2015_items$Q50b,`1` = 100, `2` = 0) %>% 
   replace_labels(EWCS2015_items$Q50b, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50c <- dplyr::recode_factor(EWCS2015_items$Q50c,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50d <- dplyr::recode_factor(EWCS2015_items$Q50d,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50d, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50e <- dplyr::recode_factor(EWCS2015_items$Q50e,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50e, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

### updating variable names and values list

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels

#####################################################

## ESENER 2014 - To estimate organisational practices

### Load the original dataset

ESENER14_base <- read_spss("./Data/UKDA-8690-spss/spss/spss19/esener2_data_q100rev.sav")

l_ESENER14_base <- get_labels(ESENER14_base, values = "n") # value labels
q_ESENER14_base <- as.data.frame(label(ESENER14_base)) # questions

ESENER14_base %>% nrow()

### Selecting items to estimate organisational practices

items_ESENER14 <- c(
  
  # merging variables
  
  "country",
  
  # filtering variable 
  
  "Size", # full company size based on directly on directly employed people (2 = 10-49)
  "Nace1", # industry 
  
  # Organisational practices indicators for bullying
  
  "Q301" # Is there a procedure in place to deal with possible cases of bullying or harassment? [If Q104 >19 and <99999]
  
)


### selecting the variables from the original dataset

ESENER14_items <- ESENER14_base %>% select(all_of(items_ESENER14)) %>% as_tibble()

### rename the variables

ESENER14_items <- ESENER14_items %>% 
  dplyr::rename(size_esener = Size, industry = Nace1,
                practice_bullying = Q301)


### extracting variable labels 

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels
q_ESENER14_items <- as.data.frame(label(ESENER14_items)) # questions


### creating no answers as NA's only for items

ESENER14_sub1 <- ESENER14_items %>% select(country, size_esener, industry) %>% 
  set_na(na = c(industry = 99), drop.levels = TRUE, as.tag = FALSE) %>% as_tibble(.)  

ESENER14_sub2 <- ESENER14_items %>% select(-c(country, size_esener, industry)) %>% 
  set_na(na = c(practice_bullying = 9), 
         drop.levels = TRUE, as.tag = FALSE) %>% as_tibble(.)

ESENER14_items <- cbind(ESENER14_sub1, ESENER14_sub2)

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels


### recoding variables (yes = 1, no = 0)

class(ESENER14_items$practice_bullying) <- "numeric"
ESENER14_items$practice_bullying <- dplyr::recode_factor(ESENER14_items$practice_bullying, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$practice_bullying, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels


#############################################################

# 2 # Creating a policy dataset

# Policy index 

## Base with indirect: No` = 0, `Yes, indirect` = 1, `Yes, direct` = 2

policy_index <- tibble(country = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
                                   19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36),  # based on ESENER labels
  law_bullying0 = c(1, 2, 2, 1, 2, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 1))
  
policy_index$law_bullying0 <- add_labels(policy_index$law_bullying0, labels = c(`Yes, indirect` = 1, `Yes, direct` = 2))


policy_index$country <- dplyr::recode(policy_index$country, `1` = 35, `2` = 1, `3` = 2, `4` = 3, `5` = 34, `6` = 5, `7` = 6, `8` = 11, 
                                               `9` = 7, `10`= 8, `11`= 12, `12`= 26, `13`= 9, `14`= 10, `15`= 4, `16`= 13, `17`= 14,  
                                               `19`= 15, `20`= 17, `21`= 18, `22`= 16, `23`= 29, `24`= 30, `25`= 19, `26`= 20, `27`= 33, `28`= 21,
                                               `29`= 22, `30`= 23, `31`= 31, `32`= 27, `33`= 25, `34`= 24, `35`= 32, `36`= 28) # aligning to EWCS labels

policy_index$country <- add_labels(policy_index$country, 
                                   labels = c( "Albania"=35, "Austria"=1, "Belgium"=2, "Bulgaria"=3, 
                                               "Switzerland"=34, "Cyprus"=5, "Czech Republic"=6, "Germany"=11, 
                                               "Denmark"=7, "Estonia"=8, "Greece"=12, "Spain"=26, "Finland"=9, 
                                               "France"=10, "Croatia"= 4, "Hungary"=13, "Ireland"=14, "Italy"=15, 
                                               "Lithuania"=17, "Luxembourg"=18, "Latvia"=16, "Montenegro"=29, 
                                               "FYROM"=30, "Malta"=19, "Netherlands"=20, "Norway"=33, "Poland"=21, 
                                               "Portugal"=22, "Romania"=23, "Serbia"=31, "Sweden"=27, "Slovenia"=25, 
                                               "Slovakia"=24, "Turkey"=32, "UK"=28)) %>% as_numeric(.)


## Recoding indirect as 0 = `Yes, indirect` = 0, `Yes, direct` = 100

policy_index <- policy_index %>% mutate(law_bullying_yes = recode_factor(law_bullying0, `1` = 0, `2` = 100)) %>% 
  add_labels(law_bullying_yes, labels = c(`Yes, indirect` = 0, `Yes, direct` = 100)) %>% 
  as_numeric(.)

get_labels(policy_index$law_bullying_yes, values = "n") # check

## create a working database

policy_ready <- policy_index

## save policy index database

save(policy_ready, file = "./Data/Working_data/policy_ready.rda") # r data
write.xlsx(policy_ready, file = "./Data/Working_data/policy_ready.xlsx") # excel file


# 3 # Standardising relevant variables for linking datasets

## Country - taking EWCS2015 as the baseline values

get_labels(ESENER14_items$country, values = "n")

get_labels(EWCS2015_items$country, values = "n")

class(ESENER14_items$country) <- "numeric"
ESENER14_items$country <- dplyr::recode_factor(ESENER14_items$country, `1` = 35, `2` = 1, `3` = 2, `4` = 3, `5` = 34, `6` = 5, `7` = 6, `8` = 11, 
                                               `9` = 7, `10`= 8, `11`= 12, `12`= 26, `13`= 9, `14`= 10, `15`= 4, `16`= 13, `17`= 14, `18`= 999, 
                                               `19`= 15, `20`= 17, `21`= 18, `22`= 16, `23`= 29, `24`= 30, `25`= 19, `26`= 20, `27`= 33, `28`= 21,
                                               `29`= 22, `30`= 23, `31`= 31, `32`= 27, `33`= 25, `34`= 24, `35`= 32, `36`= 28) 

ESENER14_items$country <- replace_labels(ESENER14_items$country, labels = c( "Albania"=35, "Austria"=1, "Belgium"=2, "Bulgaria"=3, 
                                                                             "Switzerland"=34, "Cyprus"=5, "Czech Republic"=6, "Germany"=11, 
                                                                             "Denmark"=7, "Estonia"=8, "Greece"=12, "Spain"=26, "Finland"=9, 
                                                                             "France"=10, "Croatia"= 4, "Hungary"=13, "Ireland"=14, "Iceland"=999,
                                                                             "Italy"=15, "Lithuania"=17, "Luxembourg"=18, "Latvia"=16, "Montenegro"=29, 
                                                                             "FYROM"=30, "Malta"=19, "Netherlands"=20, "Norway"=33, "Poland"=21, 
                                                                             "Portugal"=22, "Romania"=23, "Serbia"=31, "Sweden"=27, "Slovenia"=25, 
                                                                             "Slovakia"=24, "Turkey"=32, "UK"=28)) %>% as_numeric()

get_labels(ESENER14_items$country, values = "n") %in% get_labels(EWCS2015_items$country, values = "n")

ESENER14_items <- ESENER14_items %>% filter(country != 999) # removing Iceland

ESENER14_items %>% nrow()

## company size - ESENER and EWCS need to be standardised 

get_labels(ESENER14_items$size_esener, values = "n")
get_labels(EWCS2015_items$size_ewcs, values = "n")

class(ESENER14_items$size_esener) <- "numeric"
ESENER14_items$size_esener <- dplyr::recode_factor(ESENER14_items$size_esener, `1` = 1, `2` = 2, `3` = 2, `4` = 3) %>% 
  replace_labels(ESENER14_items$size_esener, labels = c("9 or less" = 1, "10-249" = 2, "250+" = 3)) %>% as_labelled()

class(EWCS2015_items$size_ewcs) <- "numeric"
EWCS2015_items$size_ewcs <- dplyr::recode_factor(EWCS2015_items$size_ewcs, `1` = 1, `2` = 1, `3` = 2, `4` = 3) %>% 
  replace_labels(EWCS2015_items$size_ewcs, labels = c("9 or less" = 1, "10-249" = 2, "250+" = 3)) %>% as_labelled()

get_labels(ESENER14_items$size_esener, values = "n") %in% get_labels(EWCS2015_items$size_ewcs, values = "n")


## industry

get_labels(ESENER14_items$industry, values = "n") 
get_labels(EWCS2015_items$industry, values = "n")

class(ESENER14_items$industry) <- "numeric"
ESENER14_items$industry <- replace_labels(ESENER14_items$industry, labels = c("A Agriculture, forestry and fishing" = 1 ,
                                                                              "B Mining and quarrying" = 2,
                                                                              "C Manufacturing" = 3,
                                                                              "D Electricity, gas, steam and air conditioning supply" = 4,
                                                                              "E Water supply; sewerage, waste management and remediation activities" = 5,
                                                                              "F Construction" = 6,
                                                                              "G Wholesale and retail trade; repair of motor vehicles and motorcycles" = 7,
                                                                              "H Transportation and storage" = 8,
                                                                              "I Accommodation and food service activities" = 9,
                                                                              "J Information and communication" = 10,
                                                                              "K Financial and insurance activities" = 11,
                                                                              "L Real estate activities" = 12,
                                                                              "M Professional, scientific and technical activities" = 13,
                                                                              "N Administrative and support service activities" = 14,
                                                                              "O Public administration and defence; compulsory social security" = 15,
                                                                              "P Education" = 16,
                                                                              "Q Human health and social work activities" = 17,
                                                                              "R Arts, entertainment and recreation" = 18,
                                                                              "S Other service activities" = 19)) %>% as_numeric()

get_labels(ESENER14_items$industry, values = "n") %in% get_labels(EWCS2015_items$industry, values = "n")

# Save datasets

save(ESENER14_items, file = "./Data/Working_data/ESENER14_items.rda") 
write.xlsx(ESENER14_items, file = "./Data/Working_data/ESENER14_items.xlsx") # excel file

save(EWCS2015_items, file = "./Data/Working_data/EWCS2015_items.rda") 
write.xlsx(EWCS2015_items, file = "./Data/Working_data/EWCS2015_items.xlsx") # excel file

rm(list = ls())
