library(tidyverse)
library(survey)

cvd_data =
  readRDS(file = '../1 - Data Assembly/Datasets/cvd_final.rds') %>% 
  filter(cohort == 1, diet_recall == 1) %>% 
  separate(cycle, c("cycle1", "cycle2"), remove=FALSE) %>% 
  mutate(exam_date = as.Date(ifelse(RIDEXMON == 1, 
                                    paste("02/01", cycle2, sep="/"),
                                    paste("08/01", cycle2, sep="/")), "%m/%d/%Y"),
         fup_time = as.numeric(round((as.Date("2015-12-31") - exam_date)/(365.25/12))),
         time_exm2 = ifelse(mortstat == 1 & cvd_outcome == 0 & fup_time > time_exm, 
                            fup_time,
                            time_exm))

#######################
# MULTIPLE IMPUTATION #
#######################

cov.model <- c('SEQN',
               'gender',
               'race',
               'age',
               'cho_hdl',
               'cho_total',
               'bpxsy_avg',
               'flag_htn_trt',
               'flag_smkng_cur',
               'flag_diab',
               'pce_risk',
               'HEI2015_TOTAL_SCORE',
               'PAG_MINW',
               'bmi',
               'flag_marit_1',
               'flag_educ_hs',
               'flag_parity_gt1',
               'age_fst_live_brth')

df.imp <-
  cvd_data %>% 
  select(cov.model) %>% 
  mutate_at(vars(gender,
                 race,
                 flag_htn_trt,
                 flag_smkng_cur,
                 flag_diab,
                 flag_marit_1,
                 flag_educ_hs,
                 flag_parity_gt1), list(factor))

ini <- mice::mice(df.imp, maxit=0, print=F)
pred <- ini$pred
pred

# Changing matrix
# pce.risk ~ gender race age cho_hdl cho_total bpxsy_avg flag_htn_trt flag_smkng_cur flag_diab
# pce_risk HEI2015_TOTAL_SCORE PAG_MINW bmi flag_marit_1 flag_educ_hs flag_parity_gt1 age_fst_live_brth

pred[, 'SEQN'] <- rep(0, length(cov.model))
pred[, 2:10] <- c(rep(0,10), 1, rep(0, 7))
pred[2:11, 11:18] <- rep(0,10)

# Multiple Imputation

imp <- mice::mice(df.imp, 
                  pred = pred,
                  meth = c('',
                           '',
                           '',
                           '',
                           'norm',
                           'norm',
                           'norm',
                           'logreg',
                           'logreg',
                           'logreg',
                           'norm',
                           'norm',
                           'norm',
                           'norm',
                           'logreg',
                           'logreg',
                           'logreg',
                           'norm'),
                  seed = 123)

##############################################
# SURVIVAL ANALYSIS WITH MULTIPLE IMPUTATION #
##############################################

source('Functions/cvd_surv_mi.R')

# Covariates for the analysis #
cov = c('flag_marit_1',
        'flag_educ_hs',
        'flag_parity_gt1',
        'age_fst_live_brth',
        'HEI2015_TOTAL_SCORE',
        'PAG_MINW',
        'bmi',
        'pce_risk')

# Cause Specific #
sga_cov <- cvd_surv_mi(imp = imp, var='flag_infnt_sga', cov = cov, time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

sga_cov2 <- cvd_surv_mi(imp = imp, var='flag_infnt_sga', cov = cov, time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

bf_cov <- cvd_surv_mi(imp = imp, var='flag_any_brstfd_1m', cov = cov, time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

bf_cov2 <- cvd_surv_mi(imp = imp, var='flag_any_brstfd_1m', cov = cov, time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

bind_rows(sga_cov, sga_cov2, bf_cov, bf_cov2) %>% 
  write.csv('Output/surv_mi.csv', row.names = FALSE)

# Competing Risks #
sga_cov <- cvd_surv_mi(imp = imp, var='flag_infnt_sga', cov = cov, time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

sga_cov2 <- cvd_surv_mi(imp = imp, var='flag_infnt_sga', cov = cov, time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

bf_cov <- cvd_surv_mi(imp = imp, var='flag_any_brstfd_1m', cov = cov, time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

bf_cov2 <- cvd_surv_mi(imp = imp, var='flag_any_brstfd_1m', cov = cov, time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

bind_rows(sga_cov, sga_cov2, bf_cov, bf_cov2) %>% 
  write.csv('Output/surv_cr_mi.csv', row.names = FALSE)

