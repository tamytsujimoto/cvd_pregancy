library(tidyverse)
library(survey)

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  mutate(pce_risk = pce_risk*100) %>% 
  filter(cohort == 1)

##########################
# DEFINING SURVEY DESIGN #
##########################

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC_C1, 
                    data=cvd_data)

################
# KAPLAN-MEIER #
################

# cvd_outcome #
plot(svykm(Surv(time_exm,cvd_outcome)~1, design=subset(nhanes, flag_subpop == 1)))

plot(svykm(Surv(time_exm,cvd_outcome)~factor(pce_risk_cat), design=subset(nhanes, flag_subpop == 1)),
     pars=list(col=1:4))

plot(svykm(Surv(time_exm,cvd_outcome)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1)),
     pars=list(col=1:2))

plot(svykm(Surv(time_exm,cvd_outcome)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1)),
     pars=list(col=1:2))

# cvd_outcome2 #
plot(svykm(Surv(time_exm,cvd_outcome2)~1, design=subset(nhanes, flag_subpop == 1)))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(pce_risk_cat), design=subset(nhanes, flag_subpop == 1)),
     pars=list(col=1:4))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_infnt_sga), design=subset(nhanes, flag_subpop == 1)),
     pars=list(col=1:2))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_any_brstfd), design=subset(nhanes, flag_subpop == 1)),
     pars=list(col=1:2))

################
# COX PH MODEL #
################

# Preg + PCE #
m1 <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_infnt_sga), 
               design=subset(nhanes, flag_subpop == 1)); summary(m1)

m1 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_infnt_sga), 
               design=subset(nhanes, flag_subpop == 1)); summary(m1)

m1 <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_any_brstfd), 
               design=subset(nhanes, flag_subpop == 1)); summary(m1)

m1 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_any_brstfd), 
               design=subset(nhanes, flag_subpop == 1)); summary(m1)

# Preg + PCE #
m1 <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_infnt_sga) + pce_risk, 
               design=subset(nhanes, flag_subpop == 1)); summary(m1)

m1 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_infnt_sga) + pce_risk, 
               design=subset(nhanes, flag_subpop == 1)); summary(m1)

m1 <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_any_brstfd) + pce_risk, 
               design=subset(nhanes, flag_subpop == 1)); summary(m1)

m1 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_any_brstfd) + pce_risk, 
               design=subset(nhanes, flag_subpop == 1)); summary(m1)





