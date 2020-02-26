library(tidyverse)

#-----------------------------------------------------------------------------------------#
# FUNCTION TO COMPUTE HEI 2015                                                            #
#                                                                                         #
# INPUT: dataframe (unique by SEQN) containing the following variables:                   #
#   KCAL          Calorie amount.                                                         #
#   VTOTALLEG     Intake of total veg plus legumes in cup eq.                             #
#   VDRKGRLEG     Intake of dark green veg plus legumes in cup eq.                        #
#   F_TOTAL       Intake of total fruit in cup eq                                         #
#   FWHOLEFRT     Intake of whole fruit in cup eq.                                        #
#   G_WHOLE       Intake of whole grain in oz. eq.                                        #
#   D_TOTAL       Intake of total dairy in cup eq.                                        #
#   PFALLPROTLEG  Intake of total protein (includes legumes) in oz. eq.                   #
#   PFSEAPLANTLEG Intake of seafood, fish and plant protein (includes legumes) in oz. eq. #
#   MONOPOLY      Grams of mono fat plus poly fat.                                        #
#   SFAT        Grams of saturated fat.                                                   #
#   SODI        Mg of sodium.                                                             #
#   G_REFINED     Intake of refined grain in oz. eq.                                      #
#   ADD_SUGARS    Intake of added sugars in tsp. eq.                                      #
#                                                                                         #
# OUTPUT: dataframe containing the following variables:                                   #
#   SEQN:         SEQUENCE NUMBER NHANES                                                  #
#   VEGDEN:       DENSITY OF TOTAL VEGETABLES PER 1000 KCAL                               #
#   GRBNDEN:      DENSITY OF DARK GREEN VEG AND BEANS PER 1000 KCAL                       #
#   FRTDEN:       DENSITY OF TOTAL FRUIT PER 1000 KCAL                                    #
#   WHFRDEN:      DENSITY OF WHOLE FRUIT PER 1000 KCAL                                    #
#   WGRNDEN:      DENSITY OF WHOLE GRAIN PER 1000 KCAL                                    #
#   DAIRYDEN:     DENSITY OF DAIRY PER 1000 KCAL                                          #
#   PROTDEN:      DENSITY OF TOTAL PROTEIN PER 1000 KCAL                                  #
#   SEAPLDE:      DENSITY OF SEAFOOD AND PLANT PROTEIN PER 1000 KCAL                      #
#   FARATIO:      FATTY ACID RATIO                                                        #
#   SODDEN:       DENSITY OF SODIUM PER 1000 KCAL                                         #
#   RGDEN:        DENSITY OF REFINED GRAINS PER 1000 KCAL                                 #
#   SFAT_PERC:    PERCENT OF CALORIES FROM SAT FAT                                        #
#   ADDSUG_PERC:  PERCENT OF CALORIES FROM ADDED SUGAR                                    #
#   HEI2015C1_TOTALVEG:       HEI-2015 COMPONENT 1 TOTAL VEGETABLES                       #
#   HEI2015C2_GREEN_AND_BEAN: HEI-2015 COMPONENT 2 GREENS AND BEANS                       #
#   HEI2015C3_TOTALFRUIT:     HEI-2015 COMPONENT 3 TOTAL FRUIT                            #
#   HEI2015C4_WHOLEFRUIT:     HEI-2015 COMPONENT 4 WHOLE FRUIT                            #
#   HEI2015C5_WHOLEGRAIN:     HEI-2015 COMPONENT 5 WHOLE GRAINS                           #
#   HEI2015C6_TOTALDAIRY:     HEI-2015 COMPONENT 6 DAIRY                                  #
#   HEI2015C7_TOTPROT:        HEI-2015 COMPONENT 7 TOTAL PROTEIN FOODS                    #
#   HEI2015C8_SEAPLANT_PROT:  HEI-2015 COMPONENT 8 SEAFOOD AND PLANT PROTEIN              #
#   HEI2015C9_FATTYACID:      HEI-2015 COMPONENT 9 FATTY ACID RATIO                       #
#   HEI2015C10_SODIUM:        HEI-2015 COMPONENT 10 SODIUM                                #         
#   HEI2015C11_REFINEDGRAIN:  HEI-2015 COMPONENT 11 REFINED GRAINS                        #
#   HEI2015C12_SFAT:          HEI-2015 COMPONENT 12 SAT FAT                               #
#   HEI2015C13_ADDSUG:        HEI-2015 COMPONENT 13 ADDED SUGAR                           #
#-----------------------------------------------------------------------------------------#

compute_hei2015 <- function(db){
  
  hei2015 =
    db %>% 
    mutate(VEGDEN = ifelse(KCAL > 0, VTOTALLEG/(KCAL/1000), NA),
           GRBNDEN = ifelse(KCAL > 0, VDRKGRLEG/(KCAL/1000), NA),
           FRTDEN = ifelse(KCAL > 0, F_TOTAL/(KCAL/1000), NA),
           WHFRDEN = ifelse(KCAL > 0, FWHOLEFRT/(KCAL/1000), NA),
           WGRNDEN = ifelse(KCAL > 0, G_WHOLE/(KCAL/1000), NA),
           DAIRYDEN = ifelse(KCAL > 0, D_TOTAL/(KCAL/1000), NA),
           PROTDEN = ifelse(KCAL > 0, PFALLPROTLEG/(KCAL/1000), NA),
           SEAPLDEN = ifelse(KCAL > 0, PFSEAPLANTLEG/(KCAL/1000), NA),
           FARATIO = ifelse(SFAT > 0, MONOPOLY/SFAT, NA),
           SODDEN = ifelse(KCAL > 0, SODI/KCAL, NA),
           RGDEN = ifelse(KCAL > 0, G_REFINED/(KCAL/1000), NA),
           SFAT_PERC = ifelse(KCAL > 0, 100*(SFAT*9/KCAL), NA),
           ADDSUG_PERC = ifelse(KCAL > 0, 100*(ADD_SUGARS*16/KCAL), NA),
           # Computing HEI2015 components:
           HEI2015C1_TOTALVEG=5*(VEGDEN/1.1),
           HEI2015C2_GREEN_AND_BEAN=5*(GRBNDEN/0.2),
           HEI2015C3_TOTALFRUIT=5*(FRTDEN/0.8),
           HEI2015C4_WHOLEFRUIT=5*(WHFRDEN/0.4),
           HEI2015C5_WHOLEGRAIN=10*(WGRNDEN/1.5),
           HEI2015C6_TOTALDAIRY=10*(DAIRYDEN/1.3),
           HEI2015C7_TOTPROT=5*(PROTDEN/2.5),
           HEI2015C8_SEAPLANT_PROT=5*(SEAPLDEN/0.8),
           HEI2015C9_FATTYACID = ifelse(MONOPOLY %in% 0 & SFAT %in% 0, 0,
                                        ifelse(MONOPOLY > 0 & SFAT %in% 0, 10,
                                               ifelse(FARATIO >= 2.5, 10,
                                                      ifelse(FARATIO <= 1.2, 0, 10*((FARATIO-1.2)/(2.5-1.2)))))),
           HEI2015C10_SODIUM = ifelse(SODDEN <= 1.1, 10,
                                      ifelse(SODDEN >= 2.0, 0, 10 - (10*(SODDEN-1.1)/(2.0-1.1)))),
           HEI2015C11_REFINEDGRAIN = ifelse(RGDEN <= 1.8, 10,
                                            ifelse(RGDEN >= 4.3, 0, 10 - (10*(RGDEN-1.8)/(4.3-1.8)))),
           HEI2015C12_SFAT = ifelse(SFAT_PERC <= 8, 10,
                                    ifelse(SFAT_PERC >= 16, 0, 10 - (10*(SFAT_PERC-8)/(16-8)))),
           HEI2015C13_ADDSUG = ifelse(ADDSUG_PERC <= 6.5, 10,
                                      ifelse(ADDSUG_PERC >= 26, 0, 10 - (10*(ADDSUG_PERC-6.5)/(26-6.5))))
           
    ) %>% 
    mutate_at(vars(HEI2015C1_TOTALVEG,
                   HEI2015C2_GREEN_AND_BEAN,
                   HEI2015C3_TOTALFRUIT,
                   HEI2015C4_WHOLEFRUIT,
                   HEI2015C5_WHOLEGRAIN,
                   HEI2015C6_TOTALDAIRY,
                   HEI2015C7_TOTPROT,
                   HEI2015C8_SEAPLANT_PROT), list(~ifelse(. > 5, 5, .))) %>% 
    mutate_at(vars(contains('HEI2015C')), list(~ifelse(KCAL == 0, 0, .))) %>% 
    # Computing HEI2015 total score:
    mutate(HEI2015_TOTAL_SCORE = HEI2015C1_TOTALVEG +
             HEI2015C2_GREEN_AND_BEAN +
             HEI2015C3_TOTALFRUIT +
             HEI2015C4_WHOLEFRUIT +
             HEI2015C5_WHOLEGRAIN +
             HEI2015C6_TOTALDAIRY +
             HEI2015C7_TOTPROT +
             HEI2015C8_SEAPLANT_PROT +
             HEI2015C9_FATTYACID + 
             HEI2015C10_SODIUM +
             HEI2015C11_REFINEDGRAIN + 
             HEI2015C12_SFAT +
             HEI2015C13_ADDSUG) %>% 
    select(SEQN, VEGDEN:HEI2015_TOTAL_SCORE)
  
  return(hei2015)
  
}

