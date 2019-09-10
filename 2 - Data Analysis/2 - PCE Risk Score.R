library(tidyverse)
library(survey)
library(ggplot2)

#####################################
# Computing ACC-AHA PCE risk scores #
#####################################

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  filter(cohort == 1) %>% 
  mutate_at(vars(age,
                 cho_total,
                 cho_hdl), list(ln = log)) %>% 
  mutate(bpxsy_avg_trt_ln = ifelse(bpxsy_avg_trt == 0, 0, log(bpxsy_avg_trt)),
         bpxsy_avg_untrt_ln = ifelse(bpxsy_avg_untrt == 0, 0, log(bpxsy_avg_untrt)),
         pce_score_white = (-29.799)*age_ln+
           4.884*age_ln^2+ 
           13.540*cho_total_ln+ 
           (-3.114)*age_ln*cho_total_ln+
           (-13.578)*cho_hdl_ln+
           3.149*age_ln*cho_hdl_ln+
           2.019*bpxsy_avg_trt_ln+
           1.957*bpxsy_avg_untrt_ln+
           7.574*flag_smkng_cur+
           (-1.665)*age_ln*flag_smkng_cur+
           0.661*flag_diab-(-29.18),
         pce_score_black = 17.114*age_ln+
           0.940*cho_total_ln+ 
           (-18.920)*cho_hdl_ln+
           4.475*age_ln*cho_hdl_ln+
           29.291*bpxsy_avg_trt_ln+
           (-6.432)*age_ln*bpxsy_avg_trt_ln+
           27.820*bpxsy_avg_untrt_ln+
           (-6.087)*age_ln*bpxsy_avg_untrt_ln+
           0.691*flag_smkng_cur+
           0.874*flag_diab-86.61,
         pce_risk_white = (1-0.9665^exp(pce_score_white)),
         pce_risk_black = (1-0.9533^exp(pce_score_black)),
         pce_risk = ifelse(race == 4, pce_risk_black, pce_risk_white),
         pce_risk_cat = ifelse(pce_risk < 0.05, 1, 
                               ifelse(pce_risk < 0.075, 2, 
                                      ifelse(pce_risk < 0.2, 3, 4)))) %>% 
  mutate(bpxsy_avg_trt = ifelse(bpxsy_avg_trt == 0, NA, bpxsy_avg_trt),
         bpxsy_avg_untrt = ifelse(bpxsy_avg_untrt == 0, NA, bpxsy_avg_untrt)) %>% 
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
                 pce_risk_cat), funs(as.factor(.))) 

##########################
# DEFINING SURVEY DESIGN #
##########################

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC_C1, 
                    data=cvd_data)

#####################################
# FUNCTION TO COMPUTE BIV. ANALYSIS #
#####################################

source('../1 - Data Assembly/cvd_desc.R')
source('cvd_biv.R')

########
# DESC #
########

cvd_final = 
  cvd_data 

cvd_desc(cat = c('pce_risk_cat'),
         cont = c('pce_risk'),
         filename = 'pce')

#############
# BIVARIATE #
#############

cat = c('race',
        'educ_level',
        'marit_stat',
        'flag_hst_htn',
        'flag_htn_trt',
        'flag_smkng_cur',
        'flag_diab',
        'mortstat',
        'ucod_leading',
        'flag_mdeath_diab',
        'flag_mdeath_htn',
        'cvd_outcome',
        'cvd_outcome2')

cont = c('age',
         'bpxsy_avg',
         'bpxsy_avg_trt',
         'bpxsy_avg_untrt',
         'cho_total',
         'cho_hdl',
         'time_int',
         'time_exm')

cvd_data %>% 
  filter(flag_subpop == 1) %>% 
  group_by(pce_risk_cat) %>% 
  summarise(n = n())

cvd_biv(cat = cat, cont = cont, by = 'pce_risk_cat') %>% 
  write.csv('Output/biv_pce_risk_cat.csv', row.names = FALSE)

cvd_biv(cat = 'pce_risk_cat', cont = 'pce_risk', by = 'flag_infnt_sga') %>% 
  write.csv('Output/biv_sga_pce.csv', row.names = FALSE)

cvd_biv(cat = 'pce_risk_cat', cont = 'pce_risk', by = 'sga_pretrm') %>% 
  write.csv('Output/biv_sga_pret_pce.csv', row.names = FALSE)

cvd_biv(cat = 'pce_risk_cat', cont = 'pce_risk', by = 'flag_any_brstfd') %>% 
  write.csv('Output/biv_brstfd_pce.csv', row.names = FALSE)

cvd_biv(cat = 'pce_risk_cat', cont = 'pce_risk', by = 'flag_rhmtd_arth') %>% 
  write.csv('Output/biv_ra_pce.csv', row.names = FALSE)

  