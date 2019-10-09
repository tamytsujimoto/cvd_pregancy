library(tidyverse)
library(survey)

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
                                      ifelse(pce_risk < 0.2, 3, 4))),
         pce_risk = pce_risk*100) %>%
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
