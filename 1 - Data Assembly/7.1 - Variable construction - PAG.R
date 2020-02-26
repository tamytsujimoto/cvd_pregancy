library(tidyverse)

###################################
# SELECTING VARIABLES OF INTEREST #
###################################

# Path for the downloaded datasets
data_path = '../../3 - Databases/nhanes_data'

# Function to stack NHANES dataset
stack_nhanes_data = function(path = data_path, pattern){
  
  data_names = list.files(path = path, pattern = pattern)
  
  db = 
    lapply(paste(path, data_names, sep = '/'), data.table::fread) %>% 
    bind_rows()
  
  return(db)
}

# Stacking datasets
paqiaf = stack_nhanes_data(pattern = 'PAQIAF') 

paq = 
  stack_nhanes_data(pattern = paste(c('PAQ.csv', 'PAQ_'), collapse = "|")) %>% 
  select(SEQN, 
         cycle,
         PAD020, 
         PAQ100,
         PAD200,
         PAD320,
         PAD120,
         PAD160,
         PAQ050U,
         PAQ050Q,
         PAD080)

###############################################################################
# CREATING VARIABLES                                                          #
#                                                                             #
# Create physical activity variables using the NHANES tutorial:               #
# https://www.cdc.gov/nchs/tutorials/PhysicalActivity/Preparing/PAQ/index.htm #
# SAS code provided in                                                        #
# https://www.cdc.gov/nchs/tutorials/PhysicalActivity/Downloads/PAQMSTR.SAS   #
###############################################################################

paq2 = 
  paq %>% 
  mutate(
    #-------------------------------------------------------------------------#
    # RECODE DATA                                                             #
    #                                                                         #
    # PAQ100: Tasks around home/yard past 30 days (1 = Yes, 2 = No)           #
    # PAD020: Walked or bicycled over past 30 days (1 = Yes, 2 = No)          # 
    # PAD320: Moderate activity over past 30 days (1 = Yes, 2 = No)           #             
    # PAD200: Vigorous activity over past 30 days (1 = Yes, 2 = No)           #
    #                                                                         #
    # Calculate min/week and MET min/week for the three physical activity     #     
    # domains: household/yard, transportation, and leisure. Begin by recoding #     
    # the physical activity data values as follows:                           #
    #  value=3 (Unable to do activity) change to 2 (no)                       #
    #  value=7 (Refused) change to . (missing)                                # 
    #  value=9 (Don't know) change to . (missing)                             #	                               
    #-------------------------------------------------------------------------#
    
    RPAD020 = ifelse(PAD020 %in% c(7,9), NA, 
                     ifelse(PAD020 %in% 3, 2, PAD020)),
    RPAQ100 = ifelse(PAQ100 %in% c(7,9), NA, 
                     ifelse(PAQ100 %in% 3, 2, PAQ100)),
    RPAD200 = ifelse(PAD200 %in% c(7,9), NA, 
                     ifelse(PAD200 %in% 3, 2, PAD200)),
    RPAD320 = ifelse(PAD320 %in% c(7,9), NA, 
                     ifelse(PAD320 %in% 3, 2, PAD320)),
    
    # ------------------------------------------------------------------------#
    # CREATE NEW VARIABLES                                                    #
    #                                                                         #
    # Create Household/Yard MET minutes per week - use MET score 4.5          #
    # Also create Household/Yard min/wk (HHMINW)                              #
    # HHMINW = 'Household and Yard Work min/wk'                               #
    # HHMMPW = 'Household and Yard Work MET min/wk';                          #    
    #                                                                         #
    # RPAQ100: Tasks around home/yard past 30 days (1 = Yes, 2 = No)          #
    # PAD120: # times past 30 days (range of values: 1 to 300)                # 
    # PAD160: How long each time (minutes) (range of values: 1 to 600)	      #	  
    # Use "4.33" as the average number of weeks in a month.        		        #
    # ------------------------------------------------------------------------# 
    
    HHMINW = ifelse(RPAQ100 == 2, 0, 
                    ifelse(RPAQ100 == 1 & PAD120 %in% c(99999, 77777), NA,
                           ifelse(RPAQ100 == 1 & PAD120 <= 300 & PAD160 <= 600, (PAD120*PAD160)/4.33, NA))),
    HHMMPW = ifelse(RPAQ100 == 2, 0, 
                    ifelse(RPAQ100 == 1 & PAD120 %in% c(99999, 77777), NA,
                           ifelse(RPAQ100 == 1 & PAD120 <= 300 & PAD160 <= 600, (HHMINW)*4.5, NA))),
    
    # ------------------------------------------------------------------------#
    # Create Transportation MET minutes per week (TRMMPW) - use MET score 4.0 #
    # Cap number of times in transportation questions.  Then, calculate       #   
    # MET min/wk and min/wk:	                                                #
    # TRMMPW: 'Transportation MET min/wk'                                     #
    # TRMINW: 'Transportation min/wk'                                         #
    # TRNUMTIM: 'Recoded/capped number of times for transportation'           #
    #     							                                                      #
    # RPAD020: Walked or bicycled over past 30 days (1 = Yes, 2 = No)         #
    # PAQ050Q: # of times walked or bicycled (range of values: 1 to 124)      #
    # PAQ050U: Unit of measure (day/week/month)(1 = Day, 2 = Week, 3 = Month) #
    # PAD080: How long per day (minutes)(range of values: 1 to 600)           #
    # ------------------------------------------------------------------------# 
    
    TRNUMTIM = ifelse(PAQ050U == 2 & PAQ050Q > 7, 7, 
                      ifelse(PAQ050U == 3 & PAQ050Q > 30, 30, 
                             ifelse(PAQ050U == 1, 1, PAQ050Q))),
    TRMINW = ifelse(PAQ050U == 1, (PAD080*TRNUMTIM)*7, 
                    ifelse(PAQ050U == 2, PAD080*TRNUMTIM, PAD080*TRNUMTIM/4.33)),
    TRMINW = ifelse(RPAD020 == 2, 0, 
                    ifelse(RPAD020 == 1 & PAQ050Q %in% c(99999, 77777), NA,
                           ifelse(RPAD020 == 1 & PAD080 %in% c(99999, 77777), NA, TRMINW))),
    TRMMPW = TRMINW*4,
    TRMMPW = ifelse(RPAD020 == 2, 0, 
                    ifelse(RPAD020 == 1 & PAQ050Q %in% c(99999, 77777), NA,
                           ifelse(RPAD020 == 1 & PAD080 %in% c(99999, 77777), NA, TRMMPW)))
  )

#-------------------------------------------------------------------------;
# Create Leisure-time MET minutes per week (LTMMPW) - use MET values in   ;
# PAQIAF data (PADMETS).  Also create leisure-time min/wk (LTMINW)        ;
#  PADMETS: MET score for activity                                        ;
#  PADTIMES: # of times did activity in past 30 days                      ;
#  PADDURAT: Average duration of activity in minutes                      ;
#-------------------------------------------------------------------------; 

paqiaf2 =
  paqiaf %>% 
  # CREATING VARIABLES #
  # MOD_LTMINM: MODerate Leisure-Time MINutes per Month
  # MOD_LTMMPM: MODerate Leisure-Time MET Minutes Per Month
  # VIG_LTMINM: VIGorous Leisure-Time MINutes per Month
  # VIG_LTMMPM: VIGorous Leisure-Time MET MINutes Per Month
  mutate(
    MOD_LTMINM = ifelse(PADLEVEL == 1, PADTIMES*PADDURAT, 0),
    MOD_LTMMPM = ifelse(PADLEVEL == 1, PADMETS*MOD_LTMINM, 0),
    VIG_LTMINM = ifelse(PADLEVEL == 2, PADTIMES*PADDURAT, 0),
    VIG_LTMMPM = ifelse(PADLEVEL == 2, PADMETS*VIG_LTMINM, 0)
  ) %>%
  # SUMMING LEISURE-TIME (MET) MINUTES PER MONTH FOR EACH STUDY PARTICIPANT 
  group_by(SEQN) %>% 
  summarise_at(vars(contains('MOD_L'), contains('VIG_L')), sum, na.rm = TRUE) %>% 
  ungroup %>% 
  mutate(
    # COMPUTING MEASURES PER WEEK
    MOD_LTMINW = MOD_LTMINM/4.33,
    MOD_LTMMPW = MOD_LTMMPM/4.33,
    VIG_LTMINW = VIG_LTMINM/4.33,
    VIG_LTMMPW = VIG_LTMMPM/4.33,
    # SUM LEISURE ACTIVITY BASED ON MODERATE+VIGOROUS
    LTMINW = MOD_LTMINW + VIG_LTMINW,
    LTMMPW = MOD_LTMMPW + VIG_LTMMPW
  )

#----------------------------------------------------------------------------------------;
# Create a variable to describe Physical Activity Guidelines Minutes per week (PAG_MINW)  
# Recall that vigorous intensity activities yield twice the benefit of moderate 
# intensity activities;                                                   
# PAG_MINW : 'Physical Activity Guidelines  - Minutes per Week'
# ADHERENCE : "Level of adherence to 2008 Physical Activity Guidelines for Americans"
# 1 = Below
# 2 = Meets
# 3 = Exceeds
#-------------------------------------------------------------------------; 

paq3 = 
  paq2 %>% 
  left_join(paqiaf2, by = 'SEQN') %>% 
  mutate(LTMINW = ifelse(LTMMPW == 0, 0, LTMINW),
         HHMINW = ifelse(HHMMPW == 0, 0, HHMINW),
         TRMINW = ifelse(TRMMPW == 0, 0, TRMINW),
         MOD_LTMINW = ifelse(RPAD320 == 2 & RPAD200 == 2, 0, MOD_LTMINW),
         MOD_LTMMPW = ifelse(RPAD320 == 2 & RPAD200 == 2, 0, MOD_LTMMPW),
         VIG_LTMINW = ifelse(RPAD320 == 2 & RPAD200 == 2, 0, VIG_LTMINW),
         VIG_LTMMPW = ifelse(RPAD320 == 2 & RPAD200 == 2, 0, VIG_LTMMPW),
         LTMMPW = ifelse(RPAD320 == 2 & RPAD200 == 2, 0, LTMMPW),
         LTMINW = ifelse(RPAD320 == 2 & RPAD200 == 2, 0, LTMINW),
  ) %>% 
  group_by(SEQN) %>% 
  mutate(TOTMMPW = ifelse(is.na(HHMMPW) & is.na(TRMMPW) & is.na(LTMMPW), NA,
                          sum(HHMMPW, TRMMPW, LTMMPW, na.rm=TRUE)),
         TOTMINW = ifelse(is.na(HHMINW) & is.na(TRMINW) & is.na(LTMINW), NA,
                          sum(HHMINW, TRMINW, LTMINW, na.rm = TRUE)),
         PAG_MINW = ifelse(is.na(VIG_LTMINW) & is.na(MOD_LTMINW) & is.na(HHMINW) & is.na(TRMINW), NA,
                           sum((VIG_LTMINW*2),MOD_LTMINW,HHMINW,TRMINW, na.rm = TRUE)),
         ADHERENCE = ifelse(is.na(PAG_MINW), NA, 
                            ifelse(PAG_MINW < 150, 1, 
                                   ifelse(PAG_MINW < 300, 2, 3)))) %>%
  ungroup %>% 
  select(SEQN, cycle, HHMINW:ADHERENCE)


##################
# SAVING DATASET #
##################

paq3 %>% saveRDS(file = 'Datasets/paq_final.rds')
paq3 %>% write.csv(file = 'Datasets/paq_final.csv', na = "", row.names = FALSE)

