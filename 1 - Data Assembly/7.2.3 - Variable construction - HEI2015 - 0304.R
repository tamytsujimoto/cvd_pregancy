library(tidyverse)

###################################
# SELECTING VARIABLES OF INTEREST #
###################################

# Path for the downloaded datasets
nhanes_path = '../../3 - Databases/nhanes_data'
fped_path = '../../3 - Databases/fped'
cnpp_path = '../../3 - Databases/cnpp'
source('Functions/compute_hei2015.R')
source('Functions/stack_data.R')

# Step 1: locate the required datasets and variables and make necessary edits to the datasets
# part a: get MPED per 100 grams of food and perform edits and corrections;
mped = 
  stack_data(path = fped_path, pattern = 'equiv0304', type = 'sas') %>% 
  mutate(FOODCODE=1*DRDIFDCD,
         MODCODE=1*DRDIMC,
         # Replace some food codes with values to correct for previously identified errors in the database #
         # PIZZA VALUES FROM FPED 11/12 [ONLY FOR 2003-2004]
         G_TOTAL = ifelse(FOODCODE %in% c(58106210, 58106230), 1.88, 
                          ifelse(FOODCODE %in% 58106220, 1.75, G_TOTAL)),
         G_WHL = ifelse(FOODCODE %in% c(58106210, 58106220, 58106230), 0, G_WHL),
         G_NWHL = ifelse(FOODCODE %in% c(58106210, 58106230), 1.88, 
                          ifelse(FOODCODE %in% 58106220, 1.75, G_NWHL)),
         V_TOTAL = ifelse(FOODCODE %in% c(58106210, 58106220, 58106230), 0.12, V_TOTAL),
         V_TOMATO = ifelse(FOODCODE %in% c(58106210, 58106220, 58106230), 0.12, V_TOMATO),
         D_TOTAL = ifelse(FOODCODE %in% c(58106220, 58106230), 0.66, 
                          ifelse(FOODCODE %in% 58106210, 0.70, D_TOTAL)),
         D_CHEESE = ifelse(FOODCODE %in% c(58106220, 58106230), 0.66, 
                          ifelse(FOODCODE %in% 58106210, 0.70, D_CHEESE)),
         DISCFAT_OIL = ifelse(FOODCODE %in% c(58106210, 58106220, 58106230), 0.44, DISCFAT_OIL),
         DISCFAT_SOL = ifelse(FOODCODE %in% 58106210, 8,
                              ifelse(FOODCODE %in% 58106220, 10.62, 
                                     ifelse(FOODCODE %in% 58106230, 8.82, DISCFAT_SOL))),
         ADD_SUG = ifelse(FOODCODE %in% c(58106210, 58106220, 58106230), 0.19, ADD_SUG),
         # Move soy beverages out of soybean products into dairy; */
         # In the HEI-2010, soy beverages are counted as part of the Milk component. 
         # Convert the four soy beverage codes in the MPED from M_SOY oz equivalents to D_TOTAL cup equivalents using the following conversion process*/
         #   
         #  FOODCODE=11310000, MILK, IMITATION, FLUID, SOY BASED (1 cup=244 grams)
         #  FOODCODE=11320000, MILK, SOY, READY-TO-DRINK, NOT BABY (1 cup=245 grams)
         #  FOODCODE=11321000, MILK, SOY, READY-TO-DRINK, NOT BABY'S, CHOCOLATE (1 cup=240 grams)
         #  FOODCODE=11330000, MILK, SOY, DRY, RECONSTITUTED, NOT BABY (1 cup=245 grams)
         M_SOY = ifelse(FOODCODE %in% c(11310000, 11320000, 11321000, 11330000), 0, M_SOY),
         D_TOTAL = ifelse(FOODCODE %in% 11310000, round(100*(1/244),3),
                          ifelse(FOODCODE %in% 11320000, round(100*(1/245),3),
                                 ifelse(FOODCODE %in% 11321000, round(100*(1/240),3),
                                        ifelse(FOODCODE %in% 11330000, round(100*(1/245), 3), D_TOTAL)))),
         # The number of cup equivalents of legumes is multiplied by 4 to convert to ounce equivalents of meat
         M_LEGUMES = LEGUMES*4
         )

# part b: get whole fruit data per 100 grams of food # 
cnpp = 
  stack_data(path = cnpp_path, pattern = 'cnppmyp_v1nhanes0304', type = 'sas') 

# part c: get individual food intake data for people with satisfactory recall status;
food1 =
  stack_data(path = nhanes_path, pattern = 'DR1IFF_C') %>% 
  filter(DR1DRSTZ == 1) %>% # reliable dietary recall status #
  mutate(FOODCODE = 1*DR1IFDCD,
         MODCODE = 1*DR1MC,
         GRMS = DR1IGRMS,
         DAYREC = 1)
  
food2 =
    stack_data(path = nhanes_path, pattern = 'DR2IFF_C') %>% 
    filter(DR2DRSTZ == 1) %>% # reliable dietary recall status #
  mutate(FOODCODE = 1*DR2IFDCD,
         MODCODE = 1*DR2MC,
         GRMS = DR2IGRMS,
         DAYREC = 2)

food = 
  food1 %>% 
  bind_rows(food2) %>% 
  select(SEQN, FOODCODE, MODCODE, GRMS, DAYREC, cycle)
  
# part d: get individual total food intake if reliable recall status
nutri1 = 
  stack_data(path = nhanes_path, pattern = 'DR1TOT_C') %>%
  filter(DR1DRSTZ == 1) %>% # reliable dietary recall status #
  mutate(KCAL = DR1TKCAL,
         MFAT = DR1TMFAT,
         PFAT = DR1TPFAT,
         SFAT = DR1TSFAT,
         SODI = DR1TSODI,
         ALCO = DR1TALCO,
         DAYREC = 1) 
  
nutri2 = 
  stack_data(path = nhanes_path, pattern = 'DR2TOT_C') %>%
  filter(DR2DRSTZ == 1) %>% # reliable dietary recall status #
  mutate(KCAL = DR2TKCAL,
         MFAT = DR2TMFAT,
         PFAT = DR2TPFAT,
         SFAT = DR2TSFAT,
         SODI = DR2TSODI,
         ALCO = DR2TALCO,
         DAYREC = 2) 

nutri =
  nutri1 %>% 
  bind_rows(nutri2) %>% 
  select(SEQN, WTDRD1, DR1DRSTZ, DR2DRSTZ, KCAL, SFAT, ALCO, SODI, MFAT, PFAT, DAYREC, cycle)

# Step 2: Combine the required datasets
# part a: combine MPED and WHOLE FRUIT data on a food level;
newmped = 
  mped %>% 
  left_join(cnpp, by = c('FOODCODE' = 'FoodCode', 'MODCODE' = 'ModCode'))

# part b: combine food intake and MPED plus WHOLE FRUIT data on a food level
fdpyr = 
  food %>% 
  inner_join(newmped, by = c('FOODCODE', 'MODCODE')) %>% 
  # part c: convert individuals' food intake amounts from grams to MyPyramid equivalents
  mutate_at(vars(G_TOTAL:A_BEV, M_LEGUMES, WHOLEFRT,FRTJUICE), list(~.*(GRMS/100)))

# part d: calculate individual food intake amounts for MyPyramid food groups for each day
pyrcalc = 
  fdpyr %>% 
  group_by(SEQN, DAYREC) %>% 
  summarise_at(vars(G_TOTAL:A_BEV, M_LEGUMES, WHOLEFRT,FRTJUICE), sum)

# part e: combine nutrient and mped data on a person day level
nutfdpyr =
  nutri %>% 
  left_join(pyrcalc, by = c('SEQN','DAYREC'))
  
# part g: set pryvar to zero for people with zero cal who do not appear in iff data
byid =
  nutfdpyr %>% 
  mutate_at(vars(G_TOTAL:A_BEV, WHOLEFRT,FRTJUICE), list(~ifelse(. < 0, 0, .))) %>% 
  # Step 3: Creates additional required variables: MONOPOLY, ALLMEAT, SEAPLANT, ADDSUGC, SOLFATC
  mutate(MONOPOLY = MFAT + PFAT,
         VTOTALLEG = V_TOTAL + LEGUMES,
         VDRKGRLEG = V_DRKGR + LEGUMES,
         PFALLPROTLEG = M_MPF + M_EGG + M_NUTSD + M_SOY + M_LEGUMES,
         PFSEAPLANTLEG = M_FISH_HI + M_FISH_LO + M_SOY + M_NUTSD + M_LEGUMES
         ) %>% 
  # get sum per person of variables of interest
  group_by(SEQN) %>% 
  summarise_at(vars(KCAL, 
                    VTOTALLEG,
                    VDRKGRLEG,
                    F_TOTAL,
                    WHOLEFRT,
                    G_WHL,
                    D_TOTAL,
                    PFALLPROTLEG,
                    PFSEAPLANTLEG,
                    MONOPOLY,
                    SFAT,
                    SODI,
                    G_NWHL,
                    ADD_SUG),sum) %>% 
  rename(FWHOLEFRT = WHOLEFRT,
         G_WHOLE = G_WHL,
         G_REFINED = G_NWHL,
         ADD_SUGARS = ADD_SUG)

# SAVING DATASET
compute_hei2015(byid) %>% 
  saveRDS(file = 'Datasets/hei2015_0304.rds')




