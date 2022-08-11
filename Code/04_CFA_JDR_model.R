
##### Data reduction, index calculation and merging datasets

options(digits = 2)

# load clean datasets

load("./Data/Working_data/JDR_ready.rda") 

wdata <- JDR_ready # working dataset


#########################################################################################

#### JDR MODEL AND OUTCOMES

## filtering for those in employment who are employees 

l_wdata <- get_labels(wdata, values = "n") # value labels 

#### JDR model for CFA

##### step 1) CFA JDR full model including all subfactors
##### step 2) removed: skill_r =~ Q53e + Q53c + Q53f + Q61i
##### step 3) removed: quantitative_d =~  Q51
##### step 4) removed: pace =~ Q50a + Q50b

m_jdr <- 'emotional_demands =~ Q30g + Q30h + Q61o
          quantitative_demands =~ Q49a + Q49b + Q61g
          pace_d =~ Q50c + Q50d + Q50e
          job_control_resources =~ Q54a + Q54b + Q54c + Q61f           
          participation_resources =~ Q61c + Q61d + Q61n 
          coleague_resources =~ Q61a + Q70e + Q89d
          supervisor_resources =~ Q61b + Q63a + Q63b + Q63c + Q63d + Q63e + Q63f'

fit_jdr <- lavaan::cfa(m_jdr, data = wdata, 
                      estimator = "ULSM", mimic = "Mplus",
                      ordered = c("Q30g", "Q30h", "Q61o", 
                                  "Q49a", "Q49b", "Q61g", 
                                  "Q50c", "Q50d", "Q50e",
                                  "Q54a", "Q54b", "Q54c", "Q61f",
                                  "Q61c", "Q61d", "Q61n",
                                  "Q61a", "Q70e", "Q89d",
                                  "Q61b", "Q63a", "Q63b", "Q63c", "Q63d", "Q63e", "Q63f"))

summary(fit_jdr, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
fitMeasures(fit_jdr)
standardizedSolution(fit_jdr, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE)
resid(fit_jdr, "cor") # covariance of the residuals
fitted(fit_jdr) # implied moments
semTools::reliability(fit_jdr)

### composite index (mean per person)

JDR_clean <- wdata %>% 
  select(-c(employment_status, worker_type, Q53e, Q53c, Q53f, Q61i, Q51, Q50a, Q50b)) %>%
  mutate(avg_emot_d = (Q30g + Q30h + Q61o) / 3,
         avg_quant_d = (Q49a + Q49b + Q61g) / 3,
         avg_pace_d = (Q50c + Q50d + Q50e) / 3, 
         avg_jobctrl_r = (Q54a + Q54b + Q54c + Q61f) / 4,
         avg_parti_r = (Q61c + Q61d + Q61n) / 3,
         avg_col_r = (Q61a + Q70e + Q89d) / 3,
         avg_super_r = (Q61b + Q63a + Q63b + Q63c + Q63d + Q63e + Q63f) / 7,
         avg_demands = (avg_emot_d + avg_quant_d + avg_pace_d) / 3,
         avg_resources = (avg_jobctrl_r + avg_parti_r + avg_col_r + avg_super_r) / 4) 

# save data with scores

save(JDR_clean, file = "./Data/Working_data/JDR_clean.rda") # r data
write.xlsx(JDR_clean, file = "./Data/Working_data/JDR_clean.xlsx")

# end data reduction

rm(list = ls()) # to save memory

