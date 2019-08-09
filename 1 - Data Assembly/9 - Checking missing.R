library(tidyverse)
library(survey)
library(summarytools)

cvd_partial = 
  readRDS(file = 'cvd_partial.rds') %>% 
  filter(flag_subpop == 1)

#####################
# SOCIODEMOGRAPHICS #
#####################

# fam_size - Total number of people in the Family #

cvd_partial %>% 
  filter(!(cycle %in% c("1999-2000", "2001-2002", "2003-2004"))) %>% 
  select(fam_size) %>% 
  summarytools::freq()
  
# birth_country - Country of Birth #

cvd_partial %>% 
  select(DMDBORN, birth_country) %>% 
  summarytools::freq()

# birth_country - Country of Birth #

cvd_partial %>% 
  select(DMDYRSUS, yrs_us) %>% 
  summarytools::freq()

#############
# PREGNANCY #
#############

# n_vgnl_dlvry - Number of vaginal deliveries #

cvd_partial %>% 
  filter(!(cycle %in% c("1999-2000", "2001-2002", "2003-2004"))) %>% 
  select(n_vgnl_dlvry) %>% 
  summarytools::freq()

# age_fst_live_brth - Age at first live birth #

cvd_partial %>% 
  select(RHQ180, RHD180, age_fst_live_brth) %>% 
  filter(is.na(age_fst_live_brth)) %>% 
  group_by(RHQ180, RHD180) %>% 
  summarise(n = n())

# age_inft_wght_9lb - Age at birth of an infant > 9 pounds #

cvd_partial %>% 
  filter(cohort == 2) %>% 
  select(RHD173) %>% 
  summarytools::freq()

# age_diab - Age at diagnosis of diabetes #

cvd_partial %>% 
  filter(is.na(age_diab)) %>%
  group_by(DIQ040Q, DID040Q, DID040) %>% 
  summarise(n = n())
  
# flag_cur_preg - Current pregnancy #

cvd_partial %>% 
  group_by(flag_cur_preg, RIDEXPRG) %>% 
  summarise(n = n())

#########################
# PREGNANCY RISK FACTOR #
#########################

# n_infnt_sga - Number of SGA infants #

cvd_partial %>% 
  filter(cohort == 1) %>% 
  group_by(flag_infnt_sga, n_infnt_sga) %>% 
  summarise(n = n())

# n_pretrm_dlvry - Number of pre-term deliveries #

cvd_partial %>% 
  filter(cohort == 1) %>% 
  group_by(flag_infnt_sga, n_pretrm_dlvry) %>% 
  summarise(n = n())
  
# age_gest_diab - Age of diagnosis of gestational diabetes #

cvd_partial %>% 
  filter(cohort == 2, is.na(age_gest_diab)) %>% 
  select(flag_gdm) %>% 
  summarytools::freq()
  
# n_infnt_brstfd - Number of infants breastfed >=1 month #

cvd_partial %>% 
  filter(cohort == 1) %>% 
  group_by(flag_hst_brstfd, flag_cur_brstfd, RHD230, n_infnt_brstfd) %>% 
  summarise(n = n())

# flag_any_brstfd - Any breastfeeding (current or history) #

cvd_partial %>% 
  group_by(cohort, flag_hst_brstfd, flag_cur_brstfd, flag_any_brstfd) %>% 
  summarise(n = n())

# flag_infnt_brstfd - Any breastfeeding > 1 month #

cvd_partial %>% 
  filter(cohort == 1) %>% 
  group_by(n_infnt_brstfd, flag_infnt_brstfd) %>% 
  summarise(n = n())

##############
# GYNECOLOGY #
##############

# age_frst_prd - Age of first menstrual period # 

cvd_partial %>% 
  filter(is.na(age_lst_prd)) %>% 
  group_by(RHQ060) %>% 
  summarise(n = n())

# age_both_ovry_remov - Age when both ovaries removed

cvd_partial %>% 
  filter(is.na(age_both_ovry_remov)) %>% 
  select(ovry_remov) %>% 
  summarytools::freq()

# flag_regprd_preg - Reason for not having regular period = pregnancy

cvd_partial %>% 
  filter(is.na(flag_regprd_preg)) %>% 
  select(flag_reg_prd) %>% 
  summarytools::freq()

# flag_both_ovry_remov - Number of women with both ovaries removed

cvd_partial %>% 
  group_by(RHQ300, RHQ310, RHQ305, ovry_remov, flag_both_ovry_remov) %>% 
  summarise(n = n())
  
cvd_partial %>% 
  select(flag_both_ovry_remov) %>% 
  summarytools::freq()

# flag_bth_cntrl_now - Taking birth control pills now #

cvd_partial %>% 
  group_by(RHD440, RHD442, flag_bth_cntrl_now) %>% 
  summarise(n = n())

# flag_estprog_ptch_hst - Used estrogen/progestin combination patches?

cvd_partial %>% 
  group_by(RHQ540, RHQ596, flag_estprog_ptch_hst) %>% 
  summarise(n = n())

cvd_partial %>% 
  select(flag_estprog_ptch_hst) %>% 
  summarytools::freq()

###############
# TRADITIONAL #
###############

# flag_htn_trt - Hypertension treatment #

cvd_partial %>% 
  group_by(flag_hst_htn, BPQ040A, BPQ050A, flag_htn_trt) %>% 
  summarise(n = n())

cvd_partial %>% 
  filter(is.na(flag_htn_trt)) %>% 
  group_by(flag_hst_htn, BPQ040A) %>% 
  summarise(n = n())

# bpxsy_avg_trt/bpxsy_avg_untrt - Treated/Untreated SYBP #

cvd_partial %>% 
  filter(is.na(bpxsy_avg_trt)) %>% 
  select(flag_htn_trt) %>% 
  summarytools::freq()

cvd_partial %>% 
  filter(is.na(bpxsy_avg_untrt)) %>% 
  select(flag_htn_trt) %>% 
  summarytools::freq()

# LBXGLU - Fasting glucose #

cvd_partial %>%
  select(LBXGLU) %>% 
  summarytools::descr()

# ogtt - Oral glucose tolerance test #

cvd_partial %>% 
  filter(!(cycle %in% c("1999-2000", "2001-2002", "2003-2004"))) %>% 
  select(ogtt) %>% 
  summarytools::descr()

# flag_cons_30m - Oral glucose tolerance test #

cvd_partial %>% 
  filter(!(cycle %in% c("2013-2014", "2015-2016"))) %>% 
  select(flag_cons_30m) %>% 
  summarytools::freq()

# flag_stg1_htn - Stage 1 HTN #

cvd_partial %>% 
  select(flag_stg1_htn) %>% 
  summarytools::freq()

# flag_smkng_nvr - Smoking self-report: Never #

cvd_partial %>% 
  filter(is.na(flag_smkng_nvr)) %>% 
  group_by(SMD030) %>% 
  summarise(n = n())

# flag_diab_trt_pill - Treatment of diabetes- oral medications #

cvd_partial %>% 
  filter(is.na(flag_diab_trt_pill)) %>% 
  select(flag_diab_hst) %>% 
  summarytools::freq()

cvd_partial %>% 
  group_by(flag_diab_hst,flag_diab_trt_pill) %>% 
  summarise(n = n())

# flag_rhmtd_arth - Rheumatoid arthritis #

cvd_partial %>% 
  filter(is.na(flag_rhmtd_arth)) %>% 
  select(MCQ160A) %>% 
    summarytools::freq()
