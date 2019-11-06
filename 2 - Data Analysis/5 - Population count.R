library(tidyverse)
library(survey)

cvd_data = readRDS(file = '../1 - Data Assembly/cvd_final.rds')

cps = data.table::fread('cvd_cps.csv')

# COUNTS OF NUMBER OF WOMEN WITH PREGNANCY HISTORY

women_cohort1 = 
  cvd_data %>% 
  filter(gender == 2 & cohort == 1) %>% 
  mutate(flag = ifelse(  age >= 40 &
                         age <= 79 &
                         !(flag_hst_cvd %in% 1) &
                         flag_preg_eli == 1, 1, 0))

# Proportion of interest
p = prop.table(table(women_cohort1$flag))[2]
summarytools::freq(women_cohort1$flag)

cps %>% 
  filter(cycle %in% c("1999-2002","2003-2004","2005-2006")) %>% 
  mutate(wght = c(0.5,0.25,0.25)) %>% 
  summarise(count=sum(wght*p*cps_women))


