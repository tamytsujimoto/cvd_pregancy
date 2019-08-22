library(tidyverse)
library(tidyverse)
library(survey)

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  filter(cohort == 1) %>% 
  mutate(sga_pretrm = ifelse(flag_infnt_sga == 1 & flag_pretrm_dlvry == 1, 2, 
                              ifelse(flag_infnt_sga == 1, 1, 0)),
         sga_pretrm = as.factor(sga_pretrm),
         bpxsy_avg_trt = ifelse(bpxsy_avg_trt == 0, NA, bpxsy_avg_trt),
         bpxsy_avg_untrt = ifelse(bpxsy_avg_untrt == 0, NA, bpxsy_avg_untrt)) %>% 
  mutate_at(vars(race,
                 educ_level,
                 marit_stat,
                 flag_hst_htn,
                 flag_htn_trt,
                 flag_smkng_cur,
                 flag_diab,
                 mortstat,
                 ucod_leading,
                 cvd_outcome,
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

###############
# SGA/PRETERM #
###############

cat = c('race',
        'educ_level',
        'marit_stat',
        'flag_hst_htn',
        'flag_htn_trt',
        'flag_smkng_cur',
        'flag_diab',
        'mortstat',
        'ucod_leading',
        'cvd_outcome')

cont = c('age',
         'bpxsy_avg',
         'bpxsy_avg_trt',
         'bpxsy_avg_untrt',
         'cho_total',
         'cho_hdl',
         'time_int',
         'time_exm')

cvd_biv(cat = cat, cont = cont, by = 'sga_pretrm') %>% 
  write.csv('Output/biv_sga.csv', row.names = FALSE)

#################
# BREASTFEEDING #
#################

cvd_biv(cat = cat, cont = cont, by = 'flag_any_brstfd') %>% 
  write.csv('Output/biv_brstfd.csv', row.names = FALSE)

######
# RA #
######

cvd_biv(cat = cat, cont = cont, by = 'flag_rhmtd_arth') %>% 
  write.csv('Output/biv_ra.csv', row.names = FALSE)



