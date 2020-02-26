library(tidyverse)
library(survey)
library(summarytools)

################
# LOADING DATA #
################

# Filtering data from cohort 1999-2006 #

cvd_final = 
  readRDS(file = 'Datasets/cvd_final.rds') %>% 
  mutate(bpxsy_avg_trt = ifelse(bpxsy_avg_trt == 0, NA, bpxsy_avg_trt),
         bpxsy_avg_untrt = ifelse(bpxsy_avg_untrt == 0, NA, bpxsy_avg_untrt)) %>% 
  mutate_at(vars(gender,
                 race,
                 educ_level,
                 flag_educ_hs,
                 marit_stat,
                 flag_marit_1,
                 hh_income,
                 fam_income,
                 flag_milit_stat,
                 birth_country,
                 ctzn_stat,
                 yrs_us,
                 flag_reg_prd,
                 flag_regprd_preg,
                 flag_both_ovry_remov,
                 flag_bth_cntrl_hst,
                 flag_bth_cntrl_now,
                 flag_estprog_ptch_hst,
                 flag_cur_preg,
                 flag_hst_preg,
                 flag_preg_eli,
                 flag_parity_gt1,
                 flag_inft_wght_9lb,
                 flag_age_diab_30,
                 flag_infnt_sga,
                 flag_pretrm_dlvry,
                 flag_preg_comp_9906,
                 flag_2preg_comp_9906,
                 flag_gdm,
                 flag_hst_brstfd,
                 flag_cur_brstfd,
                 flag_any_brstfd,
                 flag_any_brstfd_1m,
                 brstfd,
                 flag_any_preg_comp,
                 flag_cons_30m,
                 flag_hst_htn,
                 flag_htn_trt,
                 flag_stg1_htn,
                 flag_smkng_cur,
                 flag_smkng_hst,
                 flag_smkng_nvr,
                 flag_smkng_100,
                 flag_cot_lvl10,
                 flag_diab_hst,
                 flag_diab_trt_pill,
                 flag_diab_trt_ins,
                 flag_diab_fglu,
                 flag_diab_a1c,
                 flag_diab_ogtt,
                 flag_diab,
                 flag_hst_chf,
                 flag_hst_chd,
                 flag_hst_angn,
                 flag_hst_hattck,
                 flag_hst_strk,
                 flag_hst_cvd,
                 flag_rhmtd_arth,
                 mortstat,
                 ucod_leading,
                 flag_leading_hrt,
                 flag_leading_crbr,
                 flag_leading_diab,
                 flag_mdeath_diab,
                 flag_mdeath_htn,
                 cvd_outcome,
                 cvd_outcome2,
                 flag_hst_cancer,
                 flag_hst_brst_cancer,
                 flag_brst_cancer_1y,
                 flag_brst_cancer_5y,
                 flag_crp_1,
                 pce_risk_cat,
                 ADHERENCE), funs(as.factor(.))) %>% 
  filter(cohort == 1, diet_recall == 1) # only considering reliable diet recall (for HEI2015)

##########################
# DEFINING SURVEY DESIGN #
##########################

# Using sampling weight from diet data (because of HEI2015 variables)

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTDR_C1, 
                    data=cvd_final)

####################################
# FUNCTION TO COMPUTE DESCRIPTIVES #
####################################

source('Functions/cvd_desc.R')

#####################
# Sociodemographics #
#####################

cat = c('gender',
        'race',
        'educ_level',
        'flag_educ_hs',
        'marit_stat',
        'flag_marit_1',
        'hh_income',
        'fam_income',
        'flag_milit_stat',
        'birth_country',
        'ctzn_stat',
        'yrs_us')
cont = c('age',
         'hh_size',
         'fam_size',
         'fmpir')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop',
         filename = 'demo_9906')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_w',
         filename = 'demo_9906_w')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_m',
         filename = 'demo_9906_m')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_t',
         filename = 'demo_9906_t')

#############
# Pregnancy #
#############

cat = c('flag_cur_preg',
        'flag_hst_preg',
        'flag_preg_eli',
        'flag_parity_gt1',
        'flag_age_diab_30')

cont = c(
  'n_preg',
  'n_preg_live',
  'n_vgnl_dlvry',
  'age_fst_live_brth',
  'age_lst_live_brth',
  #'age_inft_wght_9lb',
  'age_diab')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop',
         filename = 'preg_9906')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_w',
         filename = 'preg_9906_w')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_t',
         filename = 'preg_9906_t')

#########################
# Pregnancy Risk Factor #
#########################

cat = c('flag_pretrm_dlvry',
        'flag_infnt_sga',
        'flag_preg_comp_9906',
        'flag_2preg_comp_9906',
        #'flag_gdm',
        'flag_hst_brstfd',
        'flag_cur_brstfd',
        'flag_any_brstfd',
        'flag_any_brstfd_1m',
        'brstfd',
        'flag_any_preg_comp')

cont = c('n_pretrm_dlvry',
         'n_infnt_sga',
         #'age_gest_diab',
         'n_infnt_brstfd_1m')

cvd_desc(cat,
         cont,
         subpop='flag_subpop',
         filename = 'pregrisk_9906')

cvd_desc(cat,
         cont,
         subpop='flag_subpop_w',
         filename = 'pregrisk_9906_w')

cvd_desc(cat,
         cont,
         subpop='flag_subpop_t',
         filename = 'pregrisk_9906_t')


##############
# Gynecology #
##############

cat = c('flag_reg_prd',
        'flag_regprd_preg',
        'flag_both_ovry_remov',
        'flag_bth_cntrl_hst',
        'flag_bth_cntrl_now',
        'flag_estprog_ptch_hst')

cont = c('age_frst_prd',
  'age_lst_prd',
  'age_lst_prd2',
  'age_both_ovry_remov')

cvd_desc(cat,
         cont,
         subpop='flag_subpop',
         filename = 'gyn_9906')

cvd_desc(cat,
         cont,
         subpop='flag_subpop_w',
         filename = 'gyn_9906_w')

cvd_desc(cat,
         cont,
         subpop='flag_subpop_t',
         filename = 'gyn_9906_t')

###############
# Traditional #
###############

cat = c('flag_cons_30m',
        'flag_hst_htn',
        'flag_htn_trt',
        'flag_stg1_htn',
        'flag_smkng_cur',
        'flag_smkng_hst',
        'flag_smkng_nvr',
        'flag_smkng_100',
        'flag_cot_lvl10',
        'flag_diab_hst',
        'flag_diab_trt_pill',
        'flag_diab_trt_ins',
        'flag_diab_fglu',
        'flag_diab_a1c',
        'flag_diab_ogtt',
        'flag_diab',
        'flag_rhmtd_arth',
        'flag_crp_1',
        'flag_hst_cancer',
        'flag_hst_brst_cancer',
        'flag_brst_cancer_1y',
        'flag_brst_cancer_5y',
        'ADHERENCE')

cont = c(
  'bpxsy_avg',
  'bpxdi_avg',
  'bpxsy_avg_trt',
  'bpxsy_avg_untrt',
  'cho_total',
  'cho_hdl',
  'cot_lvl',
  'fglu',
  'a1c',
  'ogtt',
  'bmi',
  'crp',
  'age_brst_cancer',
  'PAG_MINW')

cvd_desc(cat, 
         cont, 
         subpop='flag_subpop',
         filename = 'trad_9906')

cvd_desc(cat, 
         cont[-13], 
         subpop='flag_subpop_m',
         filename = 'trad_9906_m')

cvd_desc(cat, 
         cont, 
         subpop='flag_subpop_w',
         filename = 'trad_9906_w')

cvd_desc(cat, 
         cont, 
         subpop='flag_subpop_t',
         filename = 'trad_9906_t')

############
# Followup #
############

cat = c('mortstat',
        'ucod_leading',
        'flag_mdeath_diab',
        'flag_mdeath_htn',
        'cvd_outcome',
        'cvd_outcome2')

cont = c('time_int',
        'time_exm')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop',
         filename = 'follow_9906')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_m',
         filename = 'follow_9906_m')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_w',
         filename = 'follow_9906_w')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_t',
         filename = 'follow_9906_t')

###########
# HEI2015 #
###########

cat = c('race')

cont = c('HEI2015C1_TOTALVEG',
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
         'HEI2015_TOTAL_SCORE')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop',
         filename = 'hei_9906')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_m',
         filename = 'hei_9906_m')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_w',
         filename = 'hei_9906_w')

cvd_desc(cat,
         cont,
         subpop = 'flag_subpop_t',
         filename = 'hei_9906_t')

##################
# PCE Risk Score #
##################

cvd_desc(cat = c('pce_risk_cat'),
         cont = c('pce_risk'),
         subpop = 'flag_subpop',
         filename = 'pce_9906')

cvd_desc(cat = c('pce_risk_cat'),
         cont = c('pce_risk'),
         subpop = 'flag_subpop_m',
         filename = 'pce_9906_m')

cvd_desc(cat = c('pce_risk_cat'),
         cont = c('pce_risk'),
         subpop = 'flag_subpop_w',
         filename = 'pce_9906_w')

cvd_desc(cat = c('pce_risk_cat'),
         cont = c('pce_risk'),
         subpop = 'flag_subpop_t',
         filename = 'pce_9906_t')
