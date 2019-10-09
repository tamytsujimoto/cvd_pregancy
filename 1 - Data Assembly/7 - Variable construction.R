library(tidyverse)

nhanes = readRDS(file = 'nhanes_mort_complete.rds')

cvd_partial =
  nhanes %>% 
  rowwise %>% 
  mutate_at(vars(BPXSY1,
                 BPXSY2,
                 BPXSY3,
                 BPXSY4), funs(flag = ifelse(is.na(.) | . == 0, 0, 1))) %>% 
  mutate_at(vars(BPXDI1,
                 BPXDI2,
                 BPXDI3,
                 BPXDI4), funs(flag = ifelse(is.na(.) | . == 0, 0, 1))) %>% 
  mutate(bpx_flag_tot = sum(BPXSY1_flag, BPXSY2_flag, BPXSY3_flag, BPXSY4_flag),
         bpxsy_avg = ifelse(bpx_flag_tot == 1,  mean(c(BPXSY1,BPXSY2,BPXSY3,BPXSY4), na.rm = TRUE),
                            ifelse(bpx_flag_tot == 2 & BPXSY1_flag == 1 & BPXSY2_flag == 1, BPXSY2,
                                   ifelse(bpx_flag_tot == 2 & BPXSY1_flag == 1 & BPXSY3_flag == 1, BPXSY3,
                                          ifelse(bpx_flag_tot == 2 & BPXSY1_flag == 1 & BPXSY4_flag == 1, BPXSY4,
                                                 ifelse(bpx_flag_tot == 2 & BPXSY2_flag == 1 & BPXSY3_flag == 1, BPXSY3,
                                                        ifelse(bpx_flag_tot == 2 & BPXSY2_flag == 1 & BPXSY4_flag == 1, BPXSY4,
                                                               ifelse(bpx_flag_tot == 2 & BPXSY3_flag == 1 & BPXSY4_flag == 1, BPXSY4,
                                                                      ifelse(bpx_flag_tot == 3 & BPXSY1_flag == 1 & BPXSY2_flag == 1 & BPXSY3_flag == 1, (BPXSY2 + BPXSY3)/2,
                                                                             ifelse(bpx_flag_tot == 3 & BPXSY1_flag == 1 & BPXSY2_flag == 1 & BPXSY4_flag == 1, (BPXSY2 + BPXSY4)/2,
                                                                                    ifelse(bpx_flag_tot == 3 & BPXSY1_flag == 1 & BPXSY3_flag == 1 & BPXSY4_flag == 1, (BPXSY3 + BPXSY4)/2,
                                                                                           ifelse(bpx_flag_tot == 3 & BPXSY2_flag == 1 & BPXSY3_flag == 1 & BPXSY4_flag == 1, (BPXSY3 + BPXSY4)/2, NA))))))))))),
         bpx_flag_tot = sum(BPXDI1_flag, BPXDI2_flag, BPXDI3_flag, BPXDI4_flag),
         bpxdi_avg = ifelse(bpx_flag_tot == 1, mean(c(BPXDI1,BPXDI2,BPXDI3,BPXDI4), na.rm = TRUE),
                            ifelse(bpx_flag_tot == 2 & BPXDI1_flag == 1 & BPXDI2_flag == 1, BPXDI2,
                                   ifelse(bpx_flag_tot == 2 & BPXDI1_flag == 1 & BPXDI3_flag == 1, BPXDI3,
                                          ifelse(bpx_flag_tot == 2 & BPXDI1_flag == 1 & BPXDI4_flag == 1, BPXDI4,
                                                 ifelse(bpx_flag_tot == 2 & BPXDI2_flag == 1 & BPXDI3_flag == 1, BPXDI3,
                                                        ifelse(bpx_flag_tot == 2 & BPXDI2_flag == 1 & BPXDI4_flag == 1, BPXDI4,
                                                               ifelse(bpx_flag_tot == 2 & BPXDI3_flag == 1 & BPXDI4_flag == 1, BPXDI4,
                                                                      ifelse(bpx_flag_tot == 3 & BPXDI1_flag == 1 & BPXDI2_flag == 1 & BPXDI3_flag == 1, (BPXDI2 + BPXDI3)/2,
                                                                             ifelse(bpx_flag_tot == 3 & BPXDI1_flag == 1 & BPXDI2_flag == 1 & BPXDI4_flag == 1, (BPXDI2 + BPXDI4)/2,
                                                                                    ifelse(bpx_flag_tot == 3 & BPXDI1_flag == 1 & BPXDI3_flag == 1 & BPXDI4_flag == 1, (BPXDI3 + BPXDI4)/2,
                                                                                           ifelse(bpx_flag_tot == 3 & BPXDI2_flag == 1 & BPXDI3_flag == 1 & BPXDI4_flag == 1, (BPXDI3 + BPXDI4)/2, NA)))))))))))) %>% 
  ungroup %>% 
  mutate(age = RIDAGEYR,
         gender = RIAGENDR,
         race = RIDRETH1,
         educ_level = ifelse(DMDEDUC2 %in% c(7,9), NA, DMDEDUC2),
         marit_stat = ifelse(DMDMARTL %in% c(77,99), NA, DMDMARTL),
         hh_size = DMDHHSIZ,
         fam_size = DMDFMSIZ,
         hh_income = ifelse(cycle %in% c("1999-2000", "2001-2002", "2003-2004", "2005-2006"),
                            INDHHINC,
                            INDHHIN2),
         hh_income = ifelse(hh_income %in% c(77, 99), NA, hh_income),
         fam_income = ifelse(cycle %in% c("1999-2000", "2001-2002", "2003-2004", "2005-2006"),
                             INDFMINC,
                             INDFMIN2),
         fam_income = ifelse(fam_income %in% c(77, 99), NA, fam_income),
         fmpir = INDFMPIR,
         flag_milit_stat = ifelse(cycle %in% c("2011-2012", "2013-2014"),
                                  DMQMILIZ,
                                  DMQMILIT),
         flag_milit_stat = ifelse(flag_milit_stat %in% c(7,9), NA,
                                  ifelse(flag_milit_stat == 1, 1, 0)),
         birth_country = ifelse(DMDBORN %in% c(7,9), NA, DMDBORN),
         ctzn_stat = ifelse(DMDCITZN %in% c(7,9), NA, DMDCITZN),
         yrs_us = ifelse(DMDYRSUS %in% c(77, 88, 99), NA, DMDYRSUS),
         age_frst_prd = ifelse(RHQ010 %in% c(777, 999), NA, RHQ010),
         flag_reg_prd = ifelse(cycle %in% c("1999-2000", "2001-2002"),
                               RHQ030,
                               RHQ031),
         flag_reg_prd = ifelse(flag_reg_prd %in% c(7,9), NA,
                               ifelse(flag_reg_prd == 1, 1, 0)),
         RHD043 = ifelse(RHD043 == 3, 7, RHD043), # coding Hysterectomy as 7
         rsn_not_regprd = ifelse(cycle %in% c("1999-2000", "2001-2002"), RHQ040,
                                 ifelse(cycle %in% c("2013-2014", "2015-2016"), RHD043, RHD042)),
         rsn_not_regprd = ifelse(rsn_not_regprd %in% c(77,99), NA, rsn_not_regprd),
         flag_regprd_preg = ifelse(is.na(rsn_not_regprd), rsn_not_regprd, 
                                   ifelse(rsn_not_regprd %in% c(1,3), 1, 0)),
         age_lst_prd = ifelse(RHQ060 %in% c(777, 999), NA, RHQ060),
         ovry_remov = ifelse(cycle %in% c("1999-2000", "2001-2002", "2003-2004", "2005-2006"),
                             RHQ310, 
                             RHQ305),
         ovry_remov = ifelse(ovry_remov %in% c(7,9), NA, ovry_remov),
         ovry_remov = ifelse(RHQ300 %in% 2, 0, ovry_remov),
         flag_both_ovry_remov = ifelse(ovry_remov == 1, 1, 0),
         age_lst_prd2 = ifelse(flag_both_ovry_remov == 1, NA, age_lst_prd),
         age_both_ovry_remov = ifelse(cycle %in% c("1999-2000", "2001-2002", "2003-2004", "2005-2006"),
                                      RHQ330,
                                      RHQ332),
         age_both_ovry_remov = ifelse(age_both_ovry_remov <= 19, 19,
                                      ifelse(age_both_ovry_remov >= 60, 60, age_both_ovry_remov)),
         age_both_ovry_remov = ifelse(flag_both_ovry_remov == 1, 
                                      age_both_ovry_remov,
                                      NA),
         flag_bth_cntrl_hst = ifelse(RHQ420 %in% c(7,9), NA, 
                                     ifelse(RHQ420 == 1, 1, 0)),
         bth_cntrl_now = ifelse(cycle %in% c("1999-2000", "2001-2002"),
                                RHD440,
                                RHD442),
         flag_bth_cntrl_now = ifelse(bth_cntrl_now %in% c(7,9), NA, 
                                     ifelse(bth_cntrl_now == 1, 1, 0)),
         flag_estprog_ptch_hst = ifelse(RHQ540 %in% 2, 0, # No female hormone
                                        ifelse(RHQ540 %in% 1 & is.na(RHQ596), 0, # No patches
                                               ifelse(RHQ596 %in% c(7,9), NA, # Missing
                                                      ifelse(RHQ596 == 1, 1, 0)))),
         cur_preg_rhq = ifelse(cycle == "1999-2000", RHQ140, 
                               ifelse(cycle == "2001-2002", RHQ141, RHD143)),
         cur_preg_rhq = ifelse(cur_preg_rhq %in% c(7,9), NA, cur_preg_rhq),
         flag_cur_preg = ifelse(is.na(RIDEXPRG), NA,
                                ifelse(RIDEXPRG %in% 1, 1, 0)),
         hst_preg = ifelse(cycle %in% c("1999-2000", "2001-2002"), RHD130, RHQ131),
         flag_hst_preg = ifelse(hst_preg %in% c(7,9), NA, 
                                ifelse(hst_preg == 1, 1, 0)),
         flag_preg_eli = ifelse(is.na(flag_hst_preg) & is.na(flag_cur_preg), NA, 
                                ifelse(flag_hst_preg %in% 1 | flag_cur_preg %in% 1, 1, 0)),
         n_preg = ifelse(RHQ160 %in% c(77,99), NA, RHQ160),
         n_preg_live = ifelse(cycle %in% c("1999-2000", "2001-2002", "2003-2004"), RHD170, RHQ171),
         n_preg_live = ifelse(n_preg_live %in% c(77,99), NA, 
                              ifelse(n_preg_live >= 11, 11, n_preg_live)),
         n_vgnl_dlvry = ifelse(RHQ166 %in% c(77,99), NA, RHQ166),
         flag_inft_wght_9lb = ifelse(RHQ172 %in% c(7,9), NA, 
                                     ifelse(RHQ172 == 1, 1, 0)),
         age_fst_live_brth = ifelse(cycle %in% c("1999-2000", "2001-2002", "2003-2004", "2005-2006"),
                                    RHQ180, 
                                    RHD180),
         age_fst_live_brth = ifelse(age_fst_live_brth %in% c(777,999), NA, 
                                    ifelse(age_fst_live_brth <= 14, 14,
                                           ifelse(age_fst_live_brth >= 45, 45, age_fst_live_brth))),
         age_lst_live_brth = ifelse(cycle %in% c("1999-2000", "2001-2002", "2003-2004", "2005-2006"),
                                    RHQ190, 
                                    RHD190),
         age_lst_live_brth = ifelse(age_lst_live_brth %in% c(777,999), NA, 
                                    ifelse(age_lst_live_brth <= 14, 14,
                                           ifelse(age_lst_live_brth >= 45, 45, age_lst_live_brth))),
         age_inft_wght_9lb = ifelse(RHD173 %in% c(777,999), NA, RHD173),
         age_diab = ifelse(cycle == "1999-2000", DIQ040Q,
                           ifelse(cycle %in% c("2001-2002", "2003-2004"), DID040Q, DID040)),
         age_diab = ifelse(age_diab %in% c(777,999), NA, 
                           ifelse(age_diab %in% c(1, 666), 1, 
                                  ifelse(age_diab >= 80, 80, age_diab))),
         age_gest_diab = ifelse(RHQ163 %in% c(777,999), NA, RHQ163),
         flag_age_diab_30 = ifelse(age_diab < 30, 1, 0),
         flag_infnt_sga = ifelse(RHQ250 %in% c(7,9), NA, 
                                 ifelse(RHQ250 == 1, 1, 0)),
         n_infnt_sga = ifelse(flag_infnt_sga %in% 0, 0,
                              ifelse(RHQ260 %in% c(77,99), NA, RHQ260)),
         n_pretrm_dlvry = ifelse(flag_infnt_sga %in% 0, 0, 
                                 ifelse(RHD270 %in% c(77,99), NA, RHD270)),
         flag_pretrm_dlvry = ifelse(flag_infnt_sga == 0, 0, 
                                    ifelse(n_pretrm_dlvry > 0, 1, 0)),
         sga_pretrm = ifelse(flag_infnt_sga == 1 & flag_pretrm_dlvry == 1, 2, 
                             ifelse(flag_infnt_sga == 1, 1, 0)),
         flag_preg_comp_9906 = ifelse(flag_infnt_sga == 0, 0, 1),
         flag_2preg_comp_9906 = ifelse(flag_infnt_sga == 0, 0,
                                       ifelse(flag_pretrm_dlvry == 1, 1, 0)),
         flag_gdm = ifelse(RHQ162 %in% c(7,9), NA, 
                           ifelse(RHQ162 == 1, 1, 0)),
         flag_hst_brstfd = ifelse(RHQ210 %in% c(7,9), NA, 
                                  ifelse(RHQ210 == 1, 1, 0)),
         flag_cur_brstfd = ifelse(RHQ200 %in% c(7,9), NA, 
                                  ifelse(RHQ200 == 1, 1, 0)),
         flag_any_brstfd = ifelse(is.na(flag_hst_brstfd) & is.na(flag_cur_brstfd), NA,
                                  ifelse(flag_hst_brstfd %in% 1 | flag_cur_brstfd %in% 1, 1, 0)),
         n_infnt_brstfd = ifelse(flag_hst_brstfd %in% 0 & flag_cur_brstfd %in% c(0,NA), 0,
                                 ifelse(RHD230 %in% c(77,99), NA, RHD230)),
         flag_infnt_brstfd = ifelse(n_infnt_brstfd > 0, 1, 0),
         flag_any_preg_comp = ifelse(cycle %in% c("1999-2000", "2001-2002", "2003-2004", "2005-2006"), flag_infnt_sga, flag_gdm),
         flag_cons_30m = ifelse(BPQ150A == 1 | BPQ150B == 1 | BPQ150C == 1 | BPQ150D == 1, 1, 0),
         flag_hst_htn = ifelse(BPQ020 %in% c(7,9), NA, 
                               ifelse(BPQ020 == 1, 1, 0)),
         flag_htn_trt = ifelse(flag_hst_htn %in% 0, 0, # No history of htn
                               ifelse(BPQ040A %in% 2, 0, # No history of htn trt
                                      ifelse(BPQ050A %in% c(7,9), NA, 
                                             ifelse(BPQ050A == 1, 1, 0)))),
         flag_stg1_htn = ifelse(flag_htn_trt == 1, 1, 
                                ifelse(flag_htn_trt == 0 & bpxsy_avg > 130, 1, 
                                       ifelse(flag_htn_trt == 0 & bpxdi_avg > 80, 1, 0))),
         bpxsy_avg_trt = ifelse(flag_htn_trt == 1, bpxsy_avg, 0),
         bpxsy_avg_untrt = ifelse(flag_htn_trt == 0, bpxsy_avg, 0),
         cho_total = LBXTC,
         cho_hdl = ifelse(cycle %in% c('1999-2000', '2001-2002'), LBDHDL,
                          ifelse(cycle %in% c('2003-2004'), LBXHDD, LBDHDD)),
         SMQ020 = ifelse(SMQ020 %in% c(7,9), NA, SMQ020),
         SMD030 = ifelse(SMD030 %in% c(777,999), NA, SMD030),
         SMQ040 = ifelse(SMQ040 %in% c(7,9), NA, SMQ040),
         flag_smkng_cur = ifelse(is.na(SMQ020) & is.na(SMQ040), NA,
                                 ifelse(SMQ020 == 1 & SMQ040 %in% c(1,2), 1, 0)),
         flag_smkng_hst = ifelse(is.na(SMQ020) & is.na(SMQ040), NA, 
                                 ifelse(SMQ020 == 1 & SMQ040 %in% c(3), 1, 0)),
         flag_smkng_nvr = ifelse(SMD030 == 0, 1, 0),
         flag_smkng_100 = ifelse(SMQ020 == 1, 1, 0),
         cot_lvl = LBXCOT,
         flag_cot_lvl10 = ifelse(LBXCOT > 10, 1, 0),
         flag_diab_hst = ifelse(DIQ010 %in% c(7,9), NA,
                                ifelse(DIQ010 == 1, 1, 0)),
         flag_diab_trt_pill = ifelse(cycle %in% c("2005-2006", "2007-2008"), DID070, DIQ070),
         flag_diab_trt_pill = ifelse(flag_diab_hst %in% 0, 0, # No history of diabetes
                                     ifelse(flag_diab_trt_pill %in% c(7,9), NA,
                                            ifelse(flag_diab_trt_pill == 1, 1, 0))),
         flag_diab_trt_ins = ifelse(DIQ050 %in% c(7,9), NA,
                                    ifelse(DIQ050 == 1, 1, 0)),
         fglu = LBXGLU,
         flag_diab_fglu = ifelse(fglu >= 126, 1, 0),
         a1c = LBXGH,
         flag_diab_a1c = ifelse(a1c >= 6.5, 1, 0),
         ogtt = LBXGLT,
         flag_diab_ogtt = ifelse(ogtt >= 200, 1, 0),
         flag_hst_chf = ifelse(MCQ160B %in% c(7,9), NA,
                               ifelse(MCQ160B == 1, 1, 0)),
         flag_hst_chd = ifelse(MCQ160C %in% c(7,9), NA,
                               ifelse(MCQ160C == 1, 1, 0)),
         flag_hst_angn = ifelse(MCQ160D %in% c(7,9), NA,
                               ifelse(MCQ160D == 1, 1, 0)),
         flag_hst_hattck = ifelse(MCQ160E %in% c(7,9), NA,
                               ifelse(MCQ160E == 1, 1, 0)),
         flag_hst_strk = ifelse(MCQ160F %in% c(7,9), NA,
                               ifelse(MCQ160F == 1, 1, 0)),
         flag_rhmtd_arth = ifelse(MCQ160A %in% 2, 0, # No history of arthritis
                                  ifelse(is.na(MCQ190) & is.na(MCQ191) & is.na(MCQ195), NA,
                                         ifelse(MCQ190 %in% 1 | MCQ191 %in% 1 | MCQ195 %in% 2, 1, 0))) ,
         flag_hst_cvd = ifelse(is.na(flag_hst_chf) &
                               is.na(flag_hst_chd) &
                               is.na(flag_hst_angn) &
                               is.na(flag_hst_hattck) &
                               is.na(flag_hst_strk), NA,
                               ifelse(flag_hst_chf %in% 1 |
                                      flag_hst_chd %in% 1 |
                                      flag_hst_angn %in% 1 |
                                      flag_hst_hattck %in% 1 |
                                      flag_hst_strk %in% 1, 1, 0)),
         flag_leading_hrt = ifelse(ucod_leading == 1, 1, 0),
         flag_leading_crbr = ifelse(ucod_leading == 5, 1, 0),
         flag_leading_diab = ifelse(ucod_leading == 7, 1, 0),
         flag_mdeath_diab = diabetes,
         flag_mdeath_htn = hyperten,
         time_int = permth_int,
         time_exm = permth_exm,
         cvd_outcome = ifelse(ucod_leading == 1 | ucod_leading == 5, 1, 0),
         cvd_outcome2 = ifelse(cvd_outcome == 1, 1, 
                               ifelse(flag_mdeath_diab == 1, 1, 
                                      ifelse(flag_mdeath_htn == 1, 1, 0))),
         cvd_outcome = ifelse(is.na(cvd_outcome), 0, cvd_outcome),
         cvd_outcome2 = ifelse(is.na(cvd_outcome2), 0, cvd_outcome2),
         flag_hst_cancer = ifelse(MCQ220 %in% c(7,9), NA, 
                                  ifelse(MCQ220 == 1, 1, 0)),
         flag_hst_brst_cancer = ifelse(is.na(flag_hst_cancer), NA, 
                                       ifelse(MCQ230A %in% 14 | MCQ230B %in% 14 | MCQ230C %in% 14, 1, 0)),
         age_brst_cancer = ifelse(MCQ240E == 99999, NA, MCQ240E)
  )

# DIABETES CHECKING #

cvd_partial %>%
  mutate(flag_diab_fglu_ogtt = ifelse(is.na(flag_diab_hst) &
                                        is.na(flag_diab_trt_pill) &
                                        is.na(flag_diab_trt_ins) &
                                        is.na(flag_diab_fglu) &
                                        is.na(flag_diab_a1c) &
                                        is.na(flag_diab_ogtt), NA,
                                      ifelse(flag_diab_hst %in% 1 |
                                               flag_diab_trt_pill %in% 1 |
                                               flag_diab_trt_ins %in% 1 |
                                               flag_diab_fglu %in% 1 |
                                               flag_diab_a1c %in% 1 |
                                               flag_diab_ogtt %in% 1, 1, 0)),
         flag_diab_fglu = ifelse(is.na(flag_diab_hst) &
                                   is.na(flag_diab_trt_pill) &
                                   is.na(flag_diab_trt_ins) &
                                   is.na(flag_diab_fglu) &
                                   is.na(flag_diab_a1c), NA,
                                 ifelse(flag_diab_hst %in% 1 |
                                          flag_diab_trt_pill %in% 1 |
                                          flag_diab_trt_ins %in% 1 |
                                          flag_diab_fglu %in% 1 |
                                          flag_diab_a1c %in% 1, 1, 0)),
         flag_diab = ifelse(is.na(flag_diab_hst) &
                              is.na(flag_diab_trt_pill) &
                              is.na(flag_diab_trt_ins) &
                              is.na(flag_diab_a1c), NA,
                            ifelse(flag_diab_hst %in% 1 |
                                     flag_diab_trt_pill %in% 1 |
                                     flag_diab_trt_ins %in% 1 |
                                     flag_diab_a1c %in% 1, 1, 0))) %>%
  group_by(flag_diab_fglu_ogtt, flag_diab_fglu, flag_diab) %>%
  summarise(n = n()) %>%
  ungroup %>%
  mutate(freq = n/sum(n)*100)

# SUBPOP VARIABLES #

cvd_partial =
  cvd_partial %>%
  mutate(flag_diab = ifelse(is.na(flag_diab_hst) &
                              is.na(flag_diab_trt_pill) &
                              is.na(flag_diab_trt_ins) &
                              is.na(flag_diab_a1c), NA,
                            ifelse(flag_diab_hst %in% 1 |
                                     flag_diab_trt_pill %in% 1 |
                                     flag_diab_trt_ins %in% 1 |
                                     flag_diab_a1c %in% 1, 1, 0)),
         WTMEC = ifelse(cycle %in% c("1999-2000", "2001-2002"), 2/8*WTMEC4YR, 1/8*WTMEC2YR),
         cohort = ifelse(cycle %in% c('1999-2000', '2001-2002', '2003-2004', '2005-2006'), 1, 2),
         WTMEC_C1 = ifelse(cohort == 2, NA, 
                           ifelse(cycle %in% c("1999-2000", "2001-2002"), 2/4*WTMEC4YR, 1/4*WTMEC2YR)),
         WTMEC_C2 = ifelse(cohort == 1, NA, 1/4*WTMEC2YR),
         flag_subpop = ifelse(gender == 2 &
                                age >= 40 &
                                age <= 79 &
                                !(flag_hst_cvd %in% 1) &
                                flag_preg_eli %in% 1, 1, 0),
         flag_subpop_w = ifelse(gender == 2 &
                                  age >= 40 &
                                  age <= 79 &
                                  !(flag_hst_cvd %in% 1), 1, 0),
         flag_subpop_m = ifelse(gender == 1 &
                                  age >= 40 &
                                  age <= 79 &
                                  !(flag_hst_cvd %in% 1), 1, 0),
         flag_subpop_t = ifelse(age >= 40 &
                                  age <= 79 &
                                  !(flag_hst_cvd %in% 1), 1, 0)) 

# PCE RISK SCORE #

cvd_partial =
  cvd_partial %>% 
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
                                      ifelse(pce_risk < 0.2, 3, 4))))


cvd_partial %>% 
  saveRDS(file = 'cvd_partial.rds')

cvd_partial %>% 
  select(SEQN, cycle, SDMVPSU, SDMVSTRA,
         bpxsy_avg:flag_subpop_t, eligstat:ucod_leading, pce_risk, pce_risk_cat) %>% 
  saveRDS(file = 'cvd_final.rds')

cvd_partial %>% 
  select(SEQN, cycle, SDMVPSU, SDMVSTRA,
         bpxsy_avg:flag_subpop_t, eligstat:ucod_leading, pce_risk, pce_risk_cat) %>% 
  write.csv(file = 'cvd_final.csv', na = "")




