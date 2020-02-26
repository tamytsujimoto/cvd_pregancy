library(tidyverse)

# Path for the downloaded datasets
nhanes_path = '../../3 - Databases/nhanes_data'
source('Functions/stack_data.R')

# READING HEI2015 DATASETS
hei2015 = 
  readRDS(file = 'Datasets/hei2015_9900.rds') %>% 
  bind_rows(readRDS(file = 'Datasets/hei2015_0102.rds')) %>% 
  bind_rows(readRDS(file = 'Datasets/hei2015_0304.rds')) %>% 
  bind_rows(readRDS(file = 'Datasets/hei2015_0516.rds'))

# CREATING DIET SAMPLING WEIGHT

# 1999-2002 nutrient intake previous 24h
drxtot = 
  stack_data(path = nhanes_path, pattern = 'DRXTOT') %>% 
  mutate(DAYREC = 0) %>%
  select(SEQN, cycle, DAYREC, WTDRD1, WTDR4YR, DRDDRSTS, DRDDRSTZ)

# 2003-2014 nutrient intake during the previous 2 days - day 1 #
dr1tot = 
  stack_data(path = nhanes_path, pattern = 'DR1TOT') %>% 
  mutate(DAYREC = 1) %>% 
  select(SEQN, cycle, DAYREC, WTDRD1, DR1DRSTZ)

# 2003-2014 nutrient intake during the previous 2 days - day 2 #
dr2tot = 
  stack_data(path = nhanes_path, pattern = 'DR2TOT') %>% 
  mutate(DAYREC = 2) %>% 
  select(SEQN, cycle, DAYREC, WTDR2D, DR2DRSTZ)

# sampling weight #
diet <- 
  bind_rows(drxtot, dr1tot, dr2tot) %>% 
  mutate(diet_recall = ifelse(cycle %in% "1999-2000", DRDDRSTS,
                              ifelse(cycle %in% "2001-2002", DRDDRSTZ, 
                                     ifelse(DAYREC == 1, DR1DRSTZ, DR2DRSTZ))),
         WTDR = ifelse(cycle %in% c("1999-2000", "2001-2002"), 2/8*WTDR4YR,
                       ifelse(DAYREC == 1, 1/8*WTDRD1, 1/8*WTDR2D)))

# diet byid
diet_byid = 
  diet %>% 
  group_by(SEQN) %>% 
  summarise(n=n(),
            DAYREC = max(DAYREC)) %>% 
  left_join(diet, by = c('SEQN', 'DAYREC')) %>% 
  mutate(cohort = ifelse(cycle %in% c('1999-2000', '2001-2002', '2003-2004', '2005-2006'), 1, 2),
         WTDR_C1 = ifelse(cohort == 2, NA, WTDR*2),
         WTDR_C2 = ifelse(cohort == 1, NA, WTDR*2)) %>% 
  select(SEQN, cycle, DAYREC, diet_recall, WTDR, WTDR_C1, WTDR_C2) %>% 
  left_join(hei2015, by = 'SEQN')

# Saving dataset
diet_byid %>% 
  saveRDS(file = 'Datasets/diet_final.rds')




