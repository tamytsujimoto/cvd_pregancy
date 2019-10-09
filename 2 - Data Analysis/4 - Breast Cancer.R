library(tidyverse)
library(survey)

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  mutate(bpxsy_avg_trt = ifelse(bpxsy_avg_trt == 0, NA, bpxsy_avg_trt),
         bpxsy_avg_untrt = ifelse(bpxsy_avg_untrt == 0, NA, bpxsy_avg_untrt),
         flag_infnt_sga2 = ifelse(flag_preg_eli == 0, 2, flag_infnt_sga), # Creating non-preg category (2)
         flag_any_brstfd2 = ifelse(flag_preg_eli == 0, 2, flag_any_brstfd) # Creating non-preg category (2)
  ) %>% 
  mutate_at(vars(race,
                 educ_level,
                 marit_stat,
                 flag_hst_htn,
                 flag_htn_trt,
                 flag_smkng_cur,
                 flag_diab,
                 mortstat,
                 ucod_leading,
                 flag_mdeath_diab,
                 flag_mdeath_htn,
                 cvd_outcome,
                 cvd_outcome2,
                 flag_any_brstfd,
                 flag_rhmtd_arth,
                 flag_infnt_sga2,
                 flag_any_brstfd2,
                 pce_risk_cat), funs(as.factor(.))) 

##########################
# DEFINING SURVEY DESIGN #
##########################

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC, 
                    data=cvd_data)

#################
# BIV. ANALYSIS #
#################

source('cvd_biv.R')

cat = c('race',
        'educ_level',
        'marit_stat',
        'flag_hst_htn',
        'flag_htn_trt',
        'flag_smkng_cur',
        'flag_diab',
        'flag_infnt_sga2',
        'flag_any_brstfd2',
        'mortstat',
        'ucod_leading',
        'flag_mdeath_diab',
        'flag_mdeath_htn',
        'cvd_outcome',
        'cvd_outcome2',
        'pce_risk_cat')

cont = c('age',
         'bpxsy_avg',
         'bpxsy_avg_trt',
         'bpxsy_avg_untrt',
         'cho_total',
         'cho_hdl',
         #'age_brst_cancer',
         'time_int',
         'time_exm',
         'pce_risk')

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_w',
        by = 'flag_hst_brst_cancer') %>% 
  write.csv('Output/biv_bc.csv', row.names = FALSE)





