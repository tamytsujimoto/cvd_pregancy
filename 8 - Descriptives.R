library(tidyverse)
library(survey)
library(summarytools)

cvd_final = 
  readRDS(file = 'cvd_final.rds') %>% 
  mutate(cohort = ifelse(cycle %in% c('1999-2000', '2001-2002', '2003-2004', '2005-2006'), 1, 2))

# Define Survey Design
nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC, 
                    data=cvd_final)

# Number of missing

cvd_final %>% 
  select(-c(SEQN:SDMVSTRA,WTMEC)) %>% 
  group_by(cohort) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  select(-cohort) %>% 
  gather(key = var, value = n_missing) %>% 
  mutate(cohort = rep(c('c1','c2'), length(var)/2)) %>% 
  spread(cohort, n_missing)

