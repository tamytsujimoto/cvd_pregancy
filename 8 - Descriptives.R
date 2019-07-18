library(tidyverse)
library(survey)
library(summarytools)

################
# LOADING DATA #
################

cvd_final = 
  readRDS(file = 'cvd_final.rds') %>% 
  mutate(cohort = ifelse(cycle %in% c('1999-2000', '2001-2002', '2003-2004', '2005-2006'), 1, 2),
         aux = 1) %>% 
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

cvd_desc = function(cat, cont, filename) {
  
  all <- vector(length = length(cat)+length(cont))
  all <- c(cat,cont)
  
  # Count NA
  cvd_final[,c(all,'flag_subpop')] %>%
    filter(flag_subpop == 1) %>%
    summarise_all(funs(sum(is.na(.)))) %>%
    gather(key = var, value = n_missing) %>%
    mutate(freq = n_missing/sum(cvd_final$flag_subpop)) %>%
    filter(var!='flag_subpop') %>%
    write.csv(file = paste0('Output/', filename, '_na.csv'), row.names = FALSE)
  
  # unweighted frequency: categorical
  cvd_final[,c(cat,'flag_subpop')] %>%
    filter(flag_subpop == 1) %>%
    summarytools::freq(report.nas = FALSE) %>%
    lapply(tb) %>%
    bind_rows() %>%
    write.csv(file = paste0('Output/', filename, '_unfreq.csv'), row.names = FALSE)
  
  # weighted mean: all
  svymean(as.formula(paste0('~',paste(all, collapse = '+'))),
          subset(nhanes, flag_subpop == 1), na.rm = TRUE) %>%
    write.csv(file = paste0('Output/', filename, '_mean.csv'))
  
  # weighted quantile: cont
  svyquantile(as.formula(paste0('~',paste(cont, collapse = '+'))), 
              subset(nhanes, flag_subpop == 1), c(.05,.25,.5,.75,.95), na.rm = TRUE) %>% 
    write.csv(file = paste0('Output/', filename, '_quant.csv'))
  
}

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
         filename = 'soc')

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

cvd_final %>% 
  filter(flag_subpop == 1) %>% 
  group_by(flag_cur_preg) %>% 
  summarise(n=n())

#########################
# Pregnancy Risk Factor #
#########################

cvd_desc(cat = c('flag_cur_brstfd',
                 'flag_any_preg_comp'),
         cont = c('age'),
         filename = 'pregrisk_9916')

cvd_desc(cat = c('flag_infnt_sga',
                 'flag_pretrm_dlvry',
                 'flag_preg_comp_9906',
                 'flag_2preg_comp_9906',
                 'flag_hst_brstfd',
                 'flag_any_brstfd',
                 'flag_infnt_brstfd'),
         cont = c('n_infnt_sga',
                  'n_pretrm_dlvry',
                  'n_infnt_brstfd'),
         filename = 'pregrisk_9906')

cvd_desc(cat = c('flag_gdm'),
         cont = c('age_gest_diab'),
         filename = 'pregrisk_0716')

##############
# Gynecology #
##############

cvd_desc(cat = c('flag_reg_prd',
                 'flag_regprd_preg',
                 'flag_both_ovry_remov',
                 'flag_bth_cntrl_hst',
                 #'flag_bth_cntrl_now',
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
           #'bpxsy_avg_untrt',
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