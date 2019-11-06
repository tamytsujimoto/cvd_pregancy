library(tidyverse)
library(survey)

#####################################
# Computing ACC-AHA PCE risk scores #
#####################################

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  filter(cohort == 1) %>% 
  mutate(flag_infnt_sga2 = ifelse(flag_preg_eli == 0, 2, flag_infnt_sga), # Creating non-preg category
         sga_pretrm2 = ifelse(flag_preg_eli == 0, 3, sga_pretrm), # Creating non-preg category
         flag_any_brstfd2 = ifelse(flag_preg_eli == 0, 2, flag_any_brstfd) # Creating non-preg category
  ) %>% 
  mutate_at(vars(pce_risk_cat,
                 flag_infnt_sga,
                 flag_infnt_sga2,
                 flag_any_brstfd,
                 flag_any_brstfd2,
                 flag_rhmtd_arth), funs(as.factor(.))) %>% 
  mutate(flag_infnt_sga2 = relevel(flag_infnt_sga2, ref='2'),
         flag_any_brstfd2 = relevel(flag_any_brstfd2, ref='2'))

##########################
# DEFINING SURVEY DESIGN #
##########################

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC_C1, 
                    data=cvd_data)


#######################
# LOGISTIC REGRESSION #
#######################

source('cvd_logistic.R')

# Preg. Women #

preg.wom <- 
  cvd_logistic(cov = c('flag_infnt_sga','flag_any_brstfd','flag_rhmtd_arth'),
                         subpop = 'flag_subpop') 

# All Women #

wom <- cvd_logistic(cov = c('flag_infnt_sga2','flag_any_brstfd2','flag_rhmtd_arth'),
                    subpop = 'flag_subpop_w')

# All Men #

men <- cvd_logistic(cov = c('flag_rhmtd_arth'),
                    subpop = 'flag_subpop_m')

# All Sample #

all <- cvd_logistic(cov = c('flag_rhmtd_arth'),
                    subpop = 'flag_subpop_t')

bind_rows(preg.wom, wom, men, all) %>% 
  write.csv('Output/logistic.csv', row.names = FALSE)
