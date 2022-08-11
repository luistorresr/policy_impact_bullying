########################### Load packages ##############################################

### R version
# R version 4.2.1

### R version
# R 4.0 or above

#### packages needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(sjlabelled)) install.packages("sjlabelled", repos = "http://cran.us.r-project.org")
if(!require(sjmisc)) install.packages("sjmisc", repos = "http://cran.us.r-project.org")
if(!require(flextable)) install.packages("flextable", repos = "http://cran.us.r-project.org")
if(!require(lavaan)) install.packages("lavaan", repos = "http://cran.us.r-project.org")
if(!require(semTools)) install.packages("semTools", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(xlsx)) install.packages("xlsx", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(gvlma)) install.packages("gvlma", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(lme4)) install.packages("lme4", repos = "http://cran.us.r-project.org")
if(!require(lmerTest)) install.packages("lmerTest", repos = "http://cran.us.r-project.org")
if(!require(sjstats)) install.packages("sjstats", repos = "http://cran.us.r-project.org")
if(!require(multilevelTools)) install.packages("multilevelTools", repos = "http://cran.us.r-project.org")
if(!require(mlma)) install.packages("mlma", repos = "http://cran.us.r-project.org")
if(!require(misty)) install.packages("misty", repos = "http://cran.us.r-project.org")
if(!require(labelled)) install.packages("labelled", repos = "http://cran.us.r-project.org")

library(lavaan) # CFA and SEM
library(semTools) # additional toold for CFA and SEM 
library(sjmisc) # useful to set Na's
library(haven) # read SPSS, Stata, and SAS data
library(tidyverse) # several tools for data manipulation
library(Hmisc) # many functions useful for data analysis
library(sjlabelled) # get_labels function
library(psych) # descriptives
library(labelled) # working with labelled data
library(flextable) # formatting tables 
library(xlsx) # export data into excel 
library(openxlsx) 
library(expss)
library(kableExtra) # for SEM plotting
library(gvlma) # assumptions checks
library(e1071) # skewness
library(janitor) # add totals to tables
library(lme4) # multilevel modeling 
library(lmerTest)
library(sjstats) # intraclass correlation
library(multilevelTools)
library(misty)

# End load packages
