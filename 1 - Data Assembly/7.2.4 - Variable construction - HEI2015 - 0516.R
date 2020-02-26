library(tidyverse)

###################################
# SELECTING VARIABLES OF INTEREST #
###################################

# Path for the downloaded datasets
nhanes_path = '../../3 - Databases/nhanes_data'
fped_path = '../../3 - Databases/fped'
source('Functions/compute_hei2015.R')
source('Functions/stack_data.R')

# Stacking FPED datasets

fped1 = 
  stack_data(path = fped_path, pattern = 'fped_dr1tot', type = 'sas') %>% 
  mutate(DAYREC=1) %>% 
  rename(F_CITMLB=DR1T_F_CITMLB,
         F_OTHER=DR1T_F_OTHER,
         PF_MPS_TOTAL=DR1T_PF_MPS_TOTAL,
         PF_EGGS=DR1T_PF_EGGS,
         PF_NUTSDS=DR1T_PF_NUTSDS,
         PF_SOY=DR1T_PF_SOY,
         PF_SEAFD_HI=DR1T_PF_SEAFD_HI,
         PF_SEAFD_LOW=DR1T_PF_SEAFD_LOW,
         ADD_SUGARS=DR1T_ADD_SUGARS,
         SOLID_FATS=DR1T_SOLID_FATS,
         V_TOTAL=DR1T_V_TOTAL,
         V_DRKGR=DR1T_V_DRKGR,
         V_LEGUMES=DR1T_V_LEGUMES,
         PF_LEGUMES=DR1T_PF_LEGUMES,
         F_TOTAL=DR1T_F_TOTAL,
         G_WHOLE=DR1T_G_WHOLE,
         D_TOTAL=DR1T_D_TOTAL,
         G_REFINED=DR1T_G_REFINED)

fped2= 
  stack_data(path = fped_path, pattern = 'fped_dr2tot', type = 'sas') %>% 
  mutate(DAYREC=2) %>% 
  rename(F_CITMLB=DR2T_F_CITMLB,
         F_OTHER=DR2T_F_OTHER,
         PF_MPS_TOTAL=DR2T_PF_MPS_TOTAL,
         PF_EGGS=DR2T_PF_EGGS,
         PF_NUTSDS=DR2T_PF_NUTSDS,
         PF_SOY=DR2T_PF_SOY,
         PF_SEAFD_HI=DR2T_PF_SEAFD_HI,
         PF_SEAFD_LOW=DR2T_PF_SEAFD_LOW,
         ADD_SUGARS=DR2T_ADD_SUGARS,
         SOLID_FATS=DR2T_SOLID_FATS,
         V_TOTAL=DR2T_V_TOTAL,
         V_DRKGR=DR2T_V_DRKGR,
         V_LEGUMES=DR2T_V_LEGUMES,
         PF_LEGUMES=DR2T_PF_LEGUMES,
         F_TOTAL=DR2T_F_TOTAL,
         G_WHOLE=DR2T_G_WHOLE,
         D_TOTAL=DR2T_D_TOTAL,
         G_REFINED=DR2T_G_REFINED)

fped = 
  bind_rows(fped1, fped2) %>% 
  select(SEQN,DAYREC,F_CITMLB,F_OTHER,PF_MPS_TOTAL,PF_EGGS,
         PF_NUTSDS,PF_SOY,PF_SEAFD_HI,PF_SEAFD_LOW,ADD_SUGARS,SOLID_FATS,
         V_TOTAL,V_DRKGR,V_LEGUMES,PF_LEGUMES,F_TOTAL,G_WHOLE,D_TOTAL,G_REFINED)

# Stacking NHANES datasets

# 2003-2014 nutrient intake during the previous 2 days - day 1 #
dr1tot = 
  stack_data(nhanes_path, pattern = 'DR1TOT') %>% 
  filter(cycle != "2003-2004") %>% # Filtering cycle 2003-2004
  filter(DR1DRSTZ == 1) %>% # Reliable dietary recall only #
  mutate(DAYREC = 1) %>% 
  rename(KCAL=DR1TKCAL,
         MFAT=DR1TMFAT,
         PFAT=DR1TPFAT,
         SFAT=DR1TSFAT,
         SODI=DR1TSODI) %>% 
  select(SEQN, cycle, DAYREC, WTDRD1, KCAL, SFAT, SODI, DR1DRSTZ, MFAT, PFAT)

# 2003-2014 nutrient intake during the previous 2 days - day 2 #
dr2tot = 
  stack_data(path = nhanes_path, pattern = 'DR2TOT') %>% 
  filter(cycle != "2003-2004") %>% # Filtering cycle 2003-2004
  filter(DR2DRSTZ == 1) %>% # Reliable dietary recall only #
  mutate(DAYREC = 2) %>% 
  rename(KCAL=DR2TKCAL,
         MFAT=DR2TMFAT,
         PFAT=DR2TPFAT,
         SFAT=DR2TSFAT,
         SODI=DR2TSODI) %>% 
  select(SEQN, cycle, DAYREC, WTDR2D, KCAL, SFAT, SODI, DR2DRSTZ, MFAT, PFAT)

nutrient =
  bind_rows(dr1tot, dr2tot)

# MERGING

byid = 
  fped %>% 
  inner_join(nutrient, by = c('SEQN', 'DAYREC')) %>% 
  mutate(FWHOLEFRT = F_CITMLB + F_OTHER,
         MONOPOLY = MFAT + PFAT,
         VTOTALLEG = V_TOTAL + V_LEGUMES,
         VDRKGRLEG = V_DRKGR + V_LEGUMES,
         PFALLPROTLEG = PF_MPS_TOTAL + PF_EGGS + PF_NUTSDS + PF_SOY + PF_LEGUMES,
         PFSEAPLANTLEG = PF_SEAFD_HI + PF_SEAFD_LOW + PF_NUTSDS + PF_SOY + PF_LEGUMES) %>% 
  group_by(SEQN) %>% 
  summarise_at(vars(KCAL,VTOTALLEG,VDRKGRLEG,F_TOTAL,FWHOLEFRT,G_WHOLE,D_TOTAL,
                    PFALLPROTLEG,PFSEAPLANTLEG,MONOPOLY,SFAT,SODI,G_REFINED,ADD_SUGARS),
               sum,
               na.rm = TRUE)
  

# COMPUTING HEI2015 SCORE

compute_hei2015(byid) %>% 
  saveRDS(file = 'Datasets/hei2015_0516.rds')






