library(tidyverse)

nhanes = readRDS('nhanes_mort_complete.rds')

# Selecting target population 

cvd_data = 
  nhanes %>% 
  filter(RIAGENDR == 2) %>% # Females
  filter(RIDAGEYR >= 40, RIDAGEYR <= 79) %>% # Age 40-79
  filter(MCQ160B != 1, MCQ160C != 1, MCQ160D != 1, MCQ160E != 1, MCQ160F != 1) %>% # No CVD history
  filter(RHQ140 == 1 | RHQ141 == 1 | RHD143 == 1 | RHQ131 == 1 | RHD130 == 1) # Pregnancy history

saveRDS(cvd_data, file = 'cvd_data.rds')