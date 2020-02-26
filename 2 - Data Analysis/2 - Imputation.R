library(tidyverse)
library(survey)
source('cvd_impute_var.R')

cvd_data =
  readRDS(file = '../1 - Data Assembly/Datasets/cvd_final.rds') %>% 
  filter(cohort == 1, diet_recall == 1) 

##########################
# DEFINING SURVEY DESIGN #
##########################

# Using sampling weight from diet data

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTDR_C1, 
                    data=cvd_data)

##############
# IMPUTATION #
##############

var <- 'flag_marit_1'
cov.model <- c('pce_risk',
               'HEI2015_TOTAL_SCORE',
               'bmi',
               'flag_marit_1',
               'flag_educ_hs',
               'flag_parity_gt1',
               'age_fst_live_brth')
cov.pce <- c('age',
             'cho_hdl',
             'cho_total',
             'bpxsy_avg_untrt',
             'bpxsy_avg_trt',
             'flag_smkng_cur',
             'flag_diab')

# CATEGORICAL VARIABLES

flag_marit_1_IMP <- impute_var(var = 'flag_marit_1', 
                               cov = cov.model[!cov.model %in% 'flag_marit_1'], 
                               family = 'binomial')

flag_educ_hs_IMP <- impute_var(var = 'flag_educ_hs', 
                               cov = cov.model[!cov.model %in% 'flag_educ_hs'],
                               family = 'binomial')

flag_parity_gt1_IMP <- impute_var(var = 'flag_parity_gt1', 
                                  cov = cov.model[!cov.model %in% 'flag_parity_gt1'],
                                  family = 'binomial')

flag_smkng_cur_IMP <- impute_var(var = 'flag_smkng_cur', 
                                 cov = cov.pce[!cov.pce %in% 'flag_smkng_cur'],
                                 family = 'binomial')

flag_htn_trt_IMP <- impute_var(var = 'flag_htn_trt', 
                               cov = cov.pce[!cov.pce %in% c('bpxsy_avg_untrt', 'bpxsy_avg_trt')],
                                 family = 'binomial')

# CONTINUOUS VARIABLES

bpxsy_avg_IMP <- impute_var(var = 'bpxsy_avg',
                            cov = cov.pce[!cov.pce %in% c('bpxsy_avg_untrt', 'bpxsy_avg_trt')],
                            family = 'gaussian')

cho_total_IMP <- impute_var(var = 'cho_total',
                            cov = cov.pce[!cov.pce %in% c('cho_total', 'cho_hdl')],
                            family = 'gaussian')

cho_hdl_IMP <- impute_var(var = 'cho_hdl',
                          cov = cov.pce[!cov.pce %in% c('cho_total', 'cho_hdl')],
                          family = 'gaussian')

bmi_IMP <- impute_var(var = 'bmi',
                      cov = cov.model[!cov.model %in% 'bmi'],
                      family = 'gaussian')

age_fst_IMP <- impute_var(var = 'age_fst_live_brth',
                          cov = cov.model[!cov.model %in% 'age_fst_live_brth'],
                          family = 'gaussian')

##############################
# MERGING ALL IMPUTED VALUES #
##############################

cvd_data_imp = 
  cvd_data %>% 
  left_join(flag_marit_1_IMP, by = 'SEQN') %>% 
  left_join(flag_educ_hs_IMP, by = 'SEQN') %>% 
  left_join(flag_parity_gt1_IMP, by = 'SEQN') %>% 
  left_join(flag_smkng_cur_IMP, by = 'SEQN') %>% 
  left_join(flag_htn_trt_IMP, by = 'SEQN') %>% 
  left_join(bpxsy_avg_IMP, by = 'SEQN') %>% 
  left_join(cho_total_IMP, by = 'SEQN') %>% 
  left_join(cho_hdl_IMP, by = 'SEQN') %>% 
  left_join(bmi_IMP, by = 'SEQN') %>% 
  left_join(age_fst_IMP, by = 'SEQN') %>% 
  # COMBINING THE TRUE AND PREDICTED INFORMATION #
  mutate(flag_marit_1_IMP = ifelse(is.na(flag_marit_1), flag_marit_1_IMP, flag_marit_1),
         flag_educ_hs_IMP = ifelse(is.na(flag_educ_hs), flag_educ_hs_IMP, flag_educ_hs),
         flag_parity_gt1_IMP = ifelse(is.na(flag_parity_gt1), flag_parity_gt1_IMP, flag_parity_gt1),
         flag_smkng_cur_IMP = ifelse(is.na(flag_smkng_cur), flag_smkng_cur_IMP, flag_smkng_cur),
         flag_htn_trt_IMP = ifelse(is.na(flag_htn_trt), flag_htn_trt_IMP, flag_htn_trt),
         bpxsy_avg_trt_IMP = flag_htn_trt_IMP*bpxsy_avg_IMP,
         bpxsy_avg_untrt_IMP = (1-flag_htn_trt_IMP)*bpxsy_avg_IMP,
         bpxsy_avg_trt_IMP = ifelse(is.na(bpxsy_avg_trt), bpxsy_avg_trt_IMP, bpxsy_avg_trt),
         bpxsy_avg_untrt_IMP = ifelse(is.na(bpxsy_avg_untrt), bpxsy_avg_untrt_IMP, bpxsy_avg_untrt),
         cho_total_IMP = ifelse(is.na(cho_total), cho_total_IMP, cho_total),
         cho_hdl_IMP = ifelse(is.na(cho_hdl), cho_hdl_IMP, cho_hdl),
         bmi_IMP = ifelse(is.na(bmi), bmi_IMP, bmi),
         age_fst_live_brth_IMP = ifelse(is.na(age_fst_live_brth), age_fst_live_brth_IMP, age_fst_live_brth)) %>% 
  # COMPUTING IMPUTED PCE RISK
  mutate_at(vars(age,
                 cho_total_IMP,
                 cho_hdl_IMP), list(ln = log)) %>% 
  mutate(bpxsy_avg_trt_IMP_ln = ifelse(bpxsy_avg_trt_IMP == 0, 0, log(bpxsy_avg_trt_IMP)),
         bpxsy_avg_untrt_IMP_ln = ifelse(bpxsy_avg_untrt_IMP == 0, 0, log(bpxsy_avg_untrt_IMP)),
         pce_score_w_white_IMP= 
           (-29.799)*age_ln+
           4.884*age_ln^2+ 
           13.540*cho_total_IMP_ln+ 
           (-3.114)*age_ln*cho_total_IMP_ln+
           (-13.578)*cho_hdl_IMP_ln+
           3.149*age_ln*cho_hdl_IMP_ln+
           2.019*bpxsy_avg_trt_IMP_ln+
           1.957*bpxsy_avg_untrt_IMP_ln+
           7.574*flag_smkng_cur_IMP+
           (-1.665)*age_ln*flag_smkng_cur_IMP+
           0.661*flag_diab-(-29.18),
         pce_score_w_black_IMP = 
           17.114*age_ln+
           0.940*cho_total_IMP_ln+ 
           (-18.920)*cho_hdl_IMP_ln+
           4.475*age_ln*cho_hdl_IMP_ln+
           29.291*bpxsy_avg_trt_IMP_ln+
           (-6.432)*age_ln*bpxsy_avg_trt_IMP_ln+
           27.820*bpxsy_avg_untrt_IMP_ln+
           (-6.087)*age_ln*bpxsy_avg_untrt_IMP_ln+
           0.691*flag_smkng_cur_IMP+
           0.874*flag_diab-86.61,
         pce_score_m_white_IMP = 
           12.344*age_ln+
           11.853*cho_total_IMP_ln+
           (-2.664)*age_ln*cho_total_IMP_ln+
           (-7.990)*cho_hdl_IMP_ln+
           1.769*age_ln*cho_hdl_IMP_ln+
           1.797*bpxsy_avg_trt_IMP_ln+
           1.764*bpxsy_avg_untrt_IMP_ln+
           7.837*flag_smkng_cur_IMP+
           (-1.795)*age_ln*flag_smkng_cur_IMP+
           0.658*flag_diab-(61.18),
         pce_score_m_black_IMP = 
           2.469*age_ln+
           0.302*cho_total_IMP_ln+
           (-0.307)*cho_hdl_IMP_ln+
           1.916*bpxsy_avg_trt_IMP_ln+
           1.809*bpxsy_avg_untrt_IMP_ln+
           0.549*flag_smkng_cur_IMP+
           0.645*flag_diab-(19.54),
         pce_risk_white_IMP = ifelse(gender == 1, 
                                 (1-0.9144^exp(pce_score_m_white_IMP)), 
                                 (1-0.9665^exp(pce_score_w_white_IMP))),
         pce_risk_black_IMP = ifelse(gender == 1,
                                 (1-0.8954^exp(pce_score_m_black_IMP)),
                                 (1-0.9533^exp(pce_score_w_black_IMP))),
         pce_risk_IMP = ifelse(race == 4, pce_risk_black_IMP, pce_risk_white_IMP),
         pce_risk_IMP = ifelse(is.na(pce_risk), pce_risk_IMP, pce_risk),
         pce_risk_cat_IMP = ifelse(pce_risk_IMP < 0.05, 1, 
                               ifelse(pce_risk_IMP < 0.075, 2, 
                                      ifelse(pce_risk_IMP < 0.2, 3, 4))))


##################
# SAVING DATASET #
##################

cvd_data_imp %>% 
  select(SEQN, cycle, 
         flag_marit_1_IMP,
         flag_educ_hs_IMP,
         flag_parity_gt1_IMP,
         flag_smkng_cur_IMP,
         flag_htn_trt_IMP,
         bpxsy_avg_IMP,
         cho_total_IMP,
         cho_hdl_IMP,
         bmi_IMP,
         age_fst_live_brth_IMP,
         bpxsy_avg_trt_IMP,
         bpxsy_avg_untrt_IMP,
         pce_risk_IMP,
         pce_risk_cat_IMP) %>% 
  saveRDS(file = '../1 - Data Assembly/Datasets/cvd_IMP.rds')

###########################
# RESULTS FROM IMPUTATION #
###########################

cvd_data_imp %>% 
  filter(flag_subpop == 1) %>% 
  select(cvd_outcome,
         cvd_outcome2,
         flag_infnt_sga,
         flag_any_brstfd_1m,
         flag_marit_1_IMP,
         flag_educ_hs_IMP,
         flag_parity_gt1_IMP,
         age_fst_live_brth_IMP,
         HEI2015_TOTAL_SCORE,
         PAG_MINW,
         bmi_IMP,
         pce_risk_IMP) %>% 
  mutate_at(vars(-c(cvd_outcome, cvd_outcome2)), ~ifelse(is.na(.), 1, 0)) %>% 
  group_by(#flag_infnt_sga,
           #flag_any_brstfd_1m,
           pce_risk_IMP,
           HEI2015_TOTAL_SCORE,
           PAG_MINW,
           bmi_IMP,
           flag_marit_1_IMP,
           flag_educ_hs_IMP,
           flag_parity_gt1_IMP,
           age_fst_live_brth_IMP) %>% 
  summarise(n=n(),
            n_cvd = sum(cvd_outcome),
            n_cvd2= sum(cvd_outcome2)) %>% 
  write.csv('Output/cov_na_IMP.csv', row.names = FALSE)

cvd_data_imp %>% 
  filter(flag_subpop == 1) %>% 
  select(cvd_outcome,
         cvd_outcome2,
         age,
         cho_total_IMP,
         cho_hdl_IMP,
         bpxsy_avg_trt_IMP,
         bpxsy_avg_untrt_IMP,
         flag_smkng_cur_IMP,
         flag_diab) %>% 
  mutate_at(vars(-c(cvd_outcome, cvd_outcome2)), ~ifelse(is.na(.), 1, 0)) %>% 
  group_by(age,
           cho_total_IMP,
           cho_hdl_IMP,
           bpxsy_avg_trt_IMP,
           bpxsy_avg_untrt_IMP,
           flag_smkng_cur_IMP,
           flag_diab) %>% 
  summarise(n=n(),
            n_cvd = sum(cvd_outcome),
            n_cvd2= sum(cvd_outcome2)) %>% 
  write.csv('Output/pce_na_IMP.csv', row.names = FALSE)

source('../1 - Data Assembly/Functions/cvd_desc.R')

cvd_final =
  cvd_data %>% 
  mutate(bpxsy_avg_trt_IMP = ifelse(bpxsy_avg_trt_IMP == 0, NA, bpxsy_avg_trt_IMP),
         bpxsy_avg_untrt_IMP = ifelse(bpxsy_avg_untrt_IMP == 0, NA, bpxsy_avg_untrt_IMP)) %>%
  mutate_at(vars(pce_risk_cat_IMP), funs(as.factor(.)))

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTDR_C1, 
                    data=cvd_final)

cat = c('pce_risk_cat_IMP')
cont = c('flag_marit_1_IMP',
         'flag_educ_hs_IMP',
         'flag_parity_gt1_IMP',
         'flag_smkng_cur_IMP',
         'flag_htn_trt_IMP',
         'bpxsy_avg_IMP',
         'cho_total_IMP',
         'cho_hdl_IMP',
         'bmi_IMP',
         'age_fst_live_brth_IMP',
         'bpxsy_avg_trt_IMP',
         'bpxsy_avg_untrt_IMP',
         'pce_risk_IMP')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop',
         filename = 'imp_9906_2')




  
