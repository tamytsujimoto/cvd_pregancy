library(tidyverse)
library(survey)

#####################################
# Computing ACC-AHA PCE risk scores #
#####################################

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  mutate(flag_rhmtd_arth = factor(flag_rhmtd_arth),
         pce_risk_cat = factor(pce_risk_cat),
         pce_risk = pce_risk*100,
         flag_subpop_t2 = flag_subpop_t*(1-cvd_outcome), # excluding cvd outcome
         flag_subpop_w2 = flag_subpop_w*(1-cvd_outcome), # excluding cvd outcome
         flag_subpop_m2 = flag_subpop_m*(1-cvd_outcome) # excluding cvd outcome
         ) 

##########################
# DEFINING SURVEY DESIGN #
##########################

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC, 
                    data=cvd_data)


#######################
# LOGISTIC REGRESSION #
#######################

source('cvd_logistic.R')

# All Women #

wom <- cvd_logistic(cov = c('flag_rhmtd_arth'),
                    subpop = 'flag_subpop_w')

# All Men #

men <- cvd_logistic(cov = c('flag_rhmtd_arth'),
                    subpop = 'flag_subpop_m')

# All Sample #

all <- cvd_logistic(cov = c('flag_rhmtd_arth'),
                    subpop = 'flag_subpop_t')

bind_rows(wom, men, all) %>% 
  write.csv('Output/logistic_ra.csv', row.names = FALSE)

####################################################
# LOGISTIC REGRESSION - NON CVD+HTN/DM vs NO DEATH #
####################################################

# All Women #

wom2 <- cvd_logistic(cov = c('flag_rhmtd_arth'),
                    subpop = 'flag_subpop_w2')

# All Men #

men2 <- cvd_logistic(cov = c('flag_rhmtd_arth'),
                    subpop = 'flag_subpop_m2')

# All Sample #

all2 <- cvd_logistic(cov = c('flag_rhmtd_arth'),
                    subpop = 'flag_subpop_t')

bind_rows(wom2, men2, all2) %>% 
  write.csv('Output/logistic_ra2.csv', row.names = FALSE)
