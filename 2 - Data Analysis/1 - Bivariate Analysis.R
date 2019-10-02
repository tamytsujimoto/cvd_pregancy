library(tidyverse)
library(tidyverse)
library(survey)

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  filter(cohort == 1) %>% 
  mutate(bpxsy_avg_trt = ifelse(bpxsy_avg_trt == 0, NA, bpxsy_avg_trt),
         bpxsy_avg_untrt = ifelse(bpxsy_avg_untrt == 0, NA, bpxsy_avg_untrt),
         flag_infnt_sga2 = ifelse(flag_preg_eli == 0, 2, flag_infnt_sga), # Creating non-preg category
         sga_pretrm2 = ifelse(flag_preg_eli == 0, 3, sga_pretrm), # Creating non-preg category
         flag_any_brstfd2 = ifelse(flag_preg_eli == 0, 2, flag_any_brstfd) # Creating non-preg category
         ) %>% 
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
                 flag_rhmtd_arth), funs(as.factor(.))) 

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

source('cvd_biv.R')

#######
# SGA #
#######

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
  group_by(flag_infnt_sga) %>% 
  summarise(n = n())

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop',
        by = 'flag_infnt_sga2') %>% 
  write.csv('Output/biv_sga.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_w',
        by = 'flag_infnt_sga2') %>% 
  write.csv('Output/biv_sga_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_t',
        by = 'flag_infnt_sga2') %>% 
  write.csv('Output/biv_sga_t.csv', row.names = FALSE)

###############
# SGA/PRETERM #
###############

cvd_data %>% 
  filter(flag_subpop == 1) %>% 
  group_by(sga_pretrm) %>% 
  summarise(n = n())

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop',
        by = 'sga_pretrm2') %>% 
  write.csv('Output/biv_sga_pret.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_w',
        by = 'sga_pretrm2') %>% 
  write.csv('Output/biv_sga_pret_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_t',
        by = 'sga_pretrm2') %>% 
  write.csv('Output/biv_sga_pret_t.csv', row.names = FALSE)

#################
# BREASTFEEDING #
#################

cvd_data %>% 
  filter(flag_subpop == 1) %>% 
  group_by(flag_any_brstfd) %>% 
  summarise(n = n())

cvd_biv(cat = cat, 
        cont = cont,
        subpop = 'flag_subpop',
        by = 'flag_any_brstfd2') %>% 
  write.csv('Output/biv_brstfd.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont,
        subpop = 'flag_subpop_w',
        by = 'flag_any_brstfd2') %>% 
  write.csv('Output/biv_brstfd_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont,
        subpop = 'flag_subpop_t',
        by = 'flag_any_brstfd2') %>% 
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



