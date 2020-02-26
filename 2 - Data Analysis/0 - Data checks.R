library(tidyverse)
library(tidyverse)
library(survey)

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  filter(cohort == 1) %>% 
  mutate(dm_htn = cvd_outcome+cvd_outcome2,
         dm_htn = ifelse(dm_htn == 0, NA, dm_htn)) %>% 
  mutate_at(vars(race, flag_diab,
                 mortstat,
                 ucod_leading,
                 #flag_mdeath_diab,
                 #flag_mdeath_htn,
                 cvd_outcome,
                 cvd_outcome2), funs(as.factor(.)))

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC_C1, 
                    data=cvd_data)

source('cvd_biv.R')

cat = c('race','ucod_leading')

cont = c('age')

# flag_mdeath_diab #

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop',
        by = 'flag_mdeath_diab') %>% 
  write.csv('Output/flag_mdeath_diab.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_w',
        by = 'flag_mdeath_diab') %>% 
  write.csv('Output/flag_mdeath_diab_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_m',
        by = 'flag_mdeath_diab') %>% 
  write.csv('Output/flag_mdeath_diab_m.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_t',
        by = 'flag_mdeath_diab') %>% 
  write.csv('Output/flag_mdeath_diab_t.csv', row.names = FALSE)

# flag_mdeath_htn #

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop',
        by = 'flag_mdeath_htn') %>% 
  write.csv('Output/flag_mdeath_htn.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_w',
        by = 'flag_mdeath_htn') %>% 
  write.csv('Output/flag_mdeath_htn_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_m',
        by = 'flag_mdeath_htn') %>% 
  write.csv('Output/flag_mdeath_htn_m.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_t',
        by = 'flag_mdeath_htn') %>% 
  write.csv('Output/flag_mdeath_htn_t.csv', row.names = FALSE)

# dm_htn #

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop',
        by = 'dm_htn') %>% 
  write.csv('Output/dm_htn.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_w',
        by = 'dm_htn') %>% 
  write.csv('Output/dm_htn_w.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_m',
        by = 'dm_htn') %>% 
  write.csv('Output/dm_htn_m.csv', row.names = FALSE)

cvd_biv(cat = cat, 
        cont = cont, 
        subpop = 'flag_subpop_t',
        by = 'dm_htn') %>% 
  write.csv('Output/dm_htn_t.csv', row.names = FALSE)


cvd_data %>% 
  filter(flag_subpop == 1) %>% 
  group_by(cvd_outcome, cvd_outcome2, flag_mdeath_diab, flag_mdeath_htn) %>% 
  summarise(n=n())

# CROSSTAB DM AND HTN WITH CAUSE OF DEATH #
# DISTRIBUTION OF CAUSE OF DEATH FOR EXTRA CVD_OUTCOME EVENTS #