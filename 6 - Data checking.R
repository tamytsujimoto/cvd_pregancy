library(tidyverse)

cvd = readRDS(file = 'cvd_data.rds')

# SY BP #

bpx_check = 
  cvd %>% 
  select(BPXSY1,
         BPXSY2,
         BPXSY3,
         BPXSY4,
         BPXSAR) %>% 
  filter(!is.na(BPXSAR)) %>% 
  rowwise %>% 
  mutate_at(vars(BPXSY1,
                 BPXSY2,
                 BPXSY3,
                 BPXSY4), funs(ifelse(. == 0, NA, .))) %>% 
  mutate_at(vars(BPXSY1,
                 BPXSY2,
                 BPXSY3,
                 BPXSY4), funs(flag = ifelse(is.na(.), 0, 1))) %>% 
  mutate(bpx_flag_tot = sum(BPXSY1_flag, BPXSY2_flag, BPXSY3_flag, BPXSY4_flag),
         bpx_avg1 = mean(c(BPXSY1,BPXSY2,BPXSY3,BPXSY4), na.rm = TRUE),
         bpx_avg2 = ifelse(bpx_flag_tot == 1, bpx_avg1,
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
         diff1 = (BPXSAR - bpx_avg1)^2,
         diff2 = (BPXSAR - bpx_avg2)^2) %>% 
  ungroup
                    
fivenum(bpx_check$diff1)
fivenum(bpx_check$diff2)   
         
# DI BP #

bpx_check = 
  cvd %>% 
  select(BPXDI1,
         BPXDI2,
         BPXDI3,
         BPXDI4,
         BPXDAR) %>% 
  filter(!is.na(BPXDAR)) %>% 
  rowwise %>% 
  mutate_at(vars(BPXDI1,
                 BPXDI2,
                 BPXDI3,
                 BPXDI4), funs(ifelse(. == 0, NA, .))) %>% 
  mutate_at(vars(BPXDI1,
                 BPXDI2,
                 BPXDI3,
                 BPXDI4), funs(flag = ifelse(is.na(.), 0, 1))) %>% 
  mutate(bpx_flag_tot = sum(BPXDI1_flag, BPXDI2_flag, BPXDI3_flag, BPXDI4_flag),
         bpx_avg1 = mean(c(BPXDI1,BPXDI2,BPXDI3,BPXDI4), na.rm = TRUE),
         bpx_avg2 = ifelse(bpx_flag_tot == 1, bpx_avg1,
                    ifelse(bpx_flag_tot == 2 & BPXDI1_flag == 1 & BPXDI2_flag == 1, BPXDI2,
                    ifelse(bpx_flag_tot == 2 & BPXDI1_flag == 1 & BPXDI3_flag == 1, BPXDI3,
                    ifelse(bpx_flag_tot == 2 & BPXDI1_flag == 1 & BPXDI4_flag == 1, BPXDI4,
                    ifelse(bpx_flag_tot == 2 & BPXDI2_flag == 1 & BPXDI3_flag == 1, BPXDI3,
                    ifelse(bpx_flag_tot == 2 & BPXDI2_flag == 1 & BPXDI4_flag == 1, BPXDI4,
                    ifelse(bpx_flag_tot == 2 & BPXDI3_flag == 1 & BPXDI4_flag == 1, BPXDI4,
                    ifelse(bpx_flag_tot == 3 & BPXDI1_flag == 1 & BPXDI2_flag == 1 & BPXDI3_flag == 1, (BPXDI2 + BPXDI3)/2,
                    ifelse(bpx_flag_tot == 3 & BPXDI1_flag == 1 & BPXDI2_flag == 1 & BPXDI4_flag == 1, (BPXDI2 + BPXDI4)/2,
                    ifelse(bpx_flag_tot == 3 & BPXDI1_flag == 1 & BPXDI3_flag == 1 & BPXDI4_flag == 1, (BPXDI3 + BPXDI4)/2,
                    ifelse(bpx_flag_tot == 3 & BPXDI2_flag == 1 & BPXDI3_flag == 1 & BPXDI4_flag == 1, (BPXDI3 + BPXDI4)/2, NA))))))))))),
         diff1 = (BPXDAR - bpx_avg1)^2,
         diff2 = (BPXDAR - bpx_avg2)^2) %>% 
  ungroup
         
fivenum(bpx_check$diff1)
fivenum(bpx_check$diff2)

# inconsistencies
bpx_check %>% 
  filter(diff2 > 0) %>% 
  glimpse


# SAMPLE WEIGHTS #

weights = 
  cvd %>% 
  select(cycle,
         WTINT2YR,
         WTINT4YR,
         WTMEC2YR, 
         WTMEC4YR, 
         WTSAF2YR,
         WTSAF4YR,
         WTSOG2YR) %>% 
  mutate(WTINT = ifelse(cycle %in% c("1999-2000", "2001-2002"), 2/8*WTINT4YR, 1/8*WTINT2YR),
         WTMEC = ifelse(cycle %in% c("1999-2000", "2001-2002"), 2/8*WTMEC4YR, 1/8*WTMEC2YR),
         WTSAF = ifelse(cycle %in% c("1999-2000", "2001-2002"), 2/8*WTSAF4YR, 1/8*WTSAF2YR))
  
weights %>% 
  select(WTINT, WTMEC, WTSAF) %>% 
  summarise_all(sum, na.rm = TRUE)

weights %>% 
  select(WTINT, WTMEC, WTSAF, WTSOG2YR) %>% 
  summarise_all(funs(sum(!is.na(.))))


