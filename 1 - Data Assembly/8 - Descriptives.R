library(tidyverse)
library(survey)
library(summarytools)

################
# LOADING DATA #
################

cvd_final = 
  readRDS(file = 'cvd_final.rds') %>% 
  mutate_at(vars(gender,
                 race,
                 educ_level,
                 marit_stat,
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
                 flag_infnt_brstfd,
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
                 mortstat,
                 mortstat,
                 mortstat,
                 ucod_leading,
                 flag_leading_hrt,
                 flag_leading_crbr,
                 flag_leading_diab,
                 flag_mdeath_diab,
                 flag_mdeath_htn,
                 cvd_outcome), funs(as.factor(.)))

##########################
# DEFINING SURVEY DESIGN #
##########################

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC, 
                    data=cvd_final)

####################################
# FUNCTION TO COMPUTE DESCRIPTIVES #
####################################

source('cvd_desc.R')

#####################
# Sociodemographics #
#####################

cvd_desc(cat = c('gender',
                 'race',
                 'educ_level',
                 'marit_stat',
                 'hh_income',
                 'fam_income',
                 'flag_milit_stat',
                 'birth_country',
                 'ctzn_stat',
                 'yrs_us'),
         cont = c(
           'age',
           'hh_size',
           'fam_size',
           'fmpir'),
         filename = 'demo')

#############
# Pregnancy #
#############

cvd_desc(cat = c('flag_cur_preg',
                 'flag_hst_preg',
                 'flag_preg_eli',
                 'flag_inft_wght_9lb',
                 'flag_age_diab_30'),
         cont = c(
           'n_preg',
           'n_preg_live',
           'n_vgnl_dlvry',
           'age_fst_live_brth',
           'age_lst_live_brth',
           'age_inft_wght_9lb',
           'age_diab'),
         filename = 'preg')


#########################
# Pregnancy Risk Factor #
#########################

cvd_desc(cat = c('flag_pretrm_dlvry',
                 'flag_infnt_sga',
                 'flag_preg_comp_9906',
                 'flag_2preg_comp_9906',
                 'flag_gdm',
                 'flag_hst_brstfd',
                 'flag_cur_brstfd',
                 'flag_any_brstfd',
                 'flag_infnt_brstfd',
                 'flag_any_preg_comp'),
         cont = c('n_pretrm_dlvry',
                  'n_infnt_sga',
                  'age_gest_diab',
                  'n_infnt_brstfd'),
         filename = 'pregrisk')



##############
# Gynecology #
##############

cvd_desc(cat = c('flag_reg_prd',
                 'flag_regprd_preg',
                 'flag_both_ovry_remov',
                 'flag_bth_cntrl_hst',
                 'flag_bth_cntrl_now',
                 'flag_estprog_ptch_hst'),
         cont = c(
           'age_frst_prd',
           'age_lst_prd',
           'age_both_ovry_remov'),
         filename = 'gyn')

###############
# Traditional #
###############

cvd_desc(cat = c('flag_cons_30m',
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
                 'flag_hst_chf',
                 'flag_hst_chd',
                 'flag_hst_angn',
                 'flag_hst_hattck',
                 'flag_hst_strk',
                 'flag_hst_cvd',
                 'flag_rhmtd_arth'),
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
           'ogtt'),
         filename = 'trad')

############
# Followup #
############

cvd_desc(cat = c('mortstat',
                 'ucod_leading',
                 'flag_mdeath_diab',
                 'flag_mdeath_htn',
                 'cvd_outcome'),
         cont = c(
           'time_int',
           'time_exm'),
         filename = 'follow')