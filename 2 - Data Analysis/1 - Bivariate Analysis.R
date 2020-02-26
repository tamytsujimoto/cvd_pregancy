library(tidyverse)
library(survey)

cvd_data =
  readRDS(file = '../1 - Data Assembly/Datasets/cvd_final.rds') %>% 
  filter(cohort == 1, diet_recall == 1) %>% 
  mutate(bpxsy_avg_trt = ifelse(bpxsy_avg_trt == 0, NA, bpxsy_avg_trt),
         bpxsy_avg_untrt = ifelse(bpxsy_avg_untrt == 0, NA, bpxsy_avg_untrt)
         #flag_infnt_sga2 = ifelse(flag_preg_eli == 0, 2, flag_infnt_sga), # Creating non-preg category
         #sga_pretrm2 = ifelse(flag_preg_eli == 0, 3, sga_pretrm), # Creating non-preg category
         #flag_any_brstfd2 = ifelse(flag_preg_eli == 0, 2, flag_any_brstfd), # Creating non-preg category
         #flag_diet_sample = ifelse(!is.na(WTDR), 1, 0)
         ) %>% 
  mutate_at(vars(race,
                 educ_level,
                 flag_educ_hs,
                 marit_stat,
                 flag_marit_1,
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
                 diet_recall,
                 flag_parity_gt1,
                 ADHERENCE,
                 #flag_diet_sample,
                 pce_risk_cat), funs(as.factor(.))) 

##########################
# DEFINING SURVEY DESIGN #
##########################

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTDR_C1, 
                    data=cvd_data)

#####################################
# FUNCTION TO COMPUTE BIV. ANALYSIS #
#####################################

source('cvd_biv.R')

#######
# SGA #
#######

cat = c('race',
        'educ_level',
        'flag_educ_hs',
        'marit_stat',
        'flag_marit_1',
        'flag_parity_gt1',
        'flag_hst_htn',
        'flag_htn_trt',
        'flag_smkng_cur',
        'flag_diab',
        'ADHERENCE',
        'mortstat',
        'ucod_leading',
        'flag_mdeath_diab',
        'flag_mdeath_htn',
        'cvd_outcome',
        'cvd_outcome2',
        'pce_risk_cat')

cont = c('age',
         'age_frst_prd',
         'age_fst_live_brth',
         'bpxsy_avg',
         'bpxsy_avg_trt',
         'bpxsy_avg_untrt',
         'cho_total',
         'cho_hdl',
         'bmi',
         'crp',
         'PAG_MINW',
         'HEI2015C1_TOTALVEG',
         'HEI2015C2_GREEN_AND_BEAN',
         'HEI2015C3_TOTALFRUIT',
         'HEI2015C4_WHOLEFRUIT',
         'HEI2015C5_WHOLEGRAIN',
         'HEI2015C6_TOTALDAIRY',
         'HEI2015C7_TOTPROT',
         'HEI2015C8_SEAPLANT_PROT',
         'HEI2015C9_FATTYACID',
         'HEI2015C10_SODIUM',
         'HEI2015C11_REFINEDGRAIN',
         'HEI2015C12_SFAT',
         'HEI2015C13_ADDSUG',
         'HEI2015_TOTAL_SCORE',
         'time_int',
         'time_exm',
         'pce_risk')

cvd_data %>% 
  filter(flag_subpop == 1) %>% 
  select(flag_infnt_sga) %>% 
  summarytools::freq(.)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop',
        by = 'flag_infnt_sga') %>% 
  write.csv('Output/biv_sga.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_w',
        by = 'flag_infnt_sga') %>% 
  write.csv('Output/biv_sga_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_t',
        by = 'flag_infnt_sga') %>% 
  write.csv('Output/biv_sga_t.csv', row.names = FALSE)

###############
# SGA/PRETERM #
###############

# cvd_data %>% 
#   filter(flag_subpop == 1) %>% 
#   group_by(sga_pretrm) %>% 
#   summarise(n = n())
# 
# cvd_biv(cat = cat, 
#         cont = cont, 
#         subpop = 'flag_subpop',
#         by = 'sga_pretrm2') %>% 
#   write.csv('Output/biv_sga_pret.csv', row.names = FALSE)
# 
# cvd_biv(cat = cat, 
#         cont = cont, 
#         subpop = 'flag_subpop_w',
#         by = 'sga_pretrm2') %>% 
#   write.csv('Output/biv_sga_pret_w.csv', row.names = FALSE)
# 
# cvd_biv(cat = cat, 
#         cont = cont, 
#         subpop = 'flag_subpop_t',
#         by = 'sga_pretrm2') %>% 
#   write.csv('Output/biv_sga_pret_t.csv', row.names = FALSE)

#################
# BREASTFEEDING #
#################

cvd_data %>% 
  filter(flag_subpop == 1) %>% 
  select(flag_any_brstfd_1m) %>% 
  summarytools::freq(.)

cvd_biv(cat = cat, 
        cont = cont,
        subpop = 'flag_subpop',
        by = 'flag_any_brstfd_1m') %>% 
  write.csv('Output/biv_brstfd.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont,
        subpop = 'flag_subpop_w',
        by = 'flag_any_brstfd_1m') %>% 
  write.csv('Output/biv_brstfd_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont,
        subpop = 'flag_subpop_t',
        by = 'flag_any_brstfd_1m') %>% 
  write.csv('Output/biv_brstfd_t.csv', row.names = FALSE)

######
# RA #
######

cvd_data %>% 
  filter(flag_subpop == 1) %>% 
  group_by(flag_rhmtd_arth) %>% 
  summarise(n = n())

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop',
        by = 'flag_rhmtd_arth') %>% 
  write.csv('Output/biv_ra.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_m',
        by = 'flag_rhmtd_arth') %>% 
  write.csv('Output/biv_ra_m.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_w',
        by = 'flag_rhmtd_arth') %>% 
  write.csv('Output/biv_ra_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_t',
        by = 'flag_rhmtd_arth') %>% 
  write.csv('Output/biv_ra_t.csv', row.names = FALSE)

##################
# DIET SUBSAMPLE #
##################

# cvd_biv(cat = cat, 
#         cont = cont, 
#         subpop = 'flag_subpop',
#         by = 'flag_diet_sample') %>% 
#   write.csv('Output/biv_diet.csv', row.names = FALSE)

