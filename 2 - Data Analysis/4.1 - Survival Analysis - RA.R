library(tidyverse)
library(survey)

# Loading dataset and creating censoring time at the end of follow up #

cvd_data =
  readRDS(file = '../1 - Data Assembly/cvd_final.rds') %>% 
  separate(cycle, c("cycle1", "cycle2"), remove=FALSE) %>% 
  mutate(exam_date = as.Date(ifelse(RIDEXMON == 1, 
                            paste("02/01", cycle2, sep="/"),
                            paste("08/01", cycle2, sep="/")), "%m/%d/%Y"),
         fup_time = as.numeric(round((as.Date("2015-12-31") - exam_date)/(365.25/12))),
         time_exm2 = ifelse(mortstat == 1 & cvd_outcome == 0 & fup_time > time_exm, 
                            fup_time,
                            time_exm),
         pce_risk = pce_risk*100) %>% 
  filter(!is.na(flag_rhmtd_arth))

  
##########################
# DEFINING SURVEY DESIGN #
##########################

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTMEC, 
                    data=cvd_data)

################
# KAPLAN-MEIER #
################

# all sample #

plot(svykm(Surv(time_exm,cvd_outcome)~factor(pce_risk_cat), 
           design=subset(nhanes, flag_subpop_t == 1)),
     pars=list(col=1:4))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(pce_risk_cat), 
           design=subset(nhanes, flag_subpop_t == 1)),
     pars=list(col=1:4))

plot(svykm(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth), 
           design=subset(nhanes, flag_subpop_t == 1)),
     pars=list(col=1:2))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth), 
           design=subset(nhanes, flag_subpop_t == 1)),
     pars=list(col=1:2))

s<-svykm(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth), 
      design=subset(nhanes, flag_subpop_t == 1))

# all women #

plot(svykm(Surv(time_exm,cvd_outcome)~factor(pce_risk_cat), 
           design=subset(nhanes, flag_subpop_w == 1)),
     pars=list(col=1:4))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(pce_risk_cat), 
           design=subset(nhanes, flag_subpop_w == 1)),
     pars=list(col=1:4))

plot(svykm(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth), 
           design=subset(nhanes, flag_subpop_w == 1)),
     pars=list(col=1:2))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth), 
           design=subset(nhanes, flag_subpop_w == 1)),
     pars=list(col=1:2))

# all women #

plot(svykm(Surv(time_exm,cvd_outcome)~factor(pce_risk_cat), 
           design=subset(nhanes, flag_subpop_m == 1)),
     pars=list(col=1:4))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(pce_risk_cat), 
           design=subset(nhanes, flag_subpop_m == 1)),
     pars=list(col=1:4))

plot(svykm(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth), 
           design=subset(nhanes, flag_subpop_m == 1)),
     pars=list(col=1:2))

plot(svykm(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth), 
           design=subset(nhanes, flag_subpop_m == 1)),
     pars=list(col=1:2))

#################################
# COX PH MODEL - CAUSE SPECIFIC #
#################################

all <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth), rescale=TRUE,
                design=nhanes,
                subset="flag_subpop_m == 1"); summary(all)

# RA #
all <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth), rescale=TRUE,
               design=subset(nhanes, flag_subpop_t == 1)); summary(all)

wom <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth), 
               design=subset(nhanes, flag_subpop_w == 1)); summary(wom)

men <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth), 
               design=subset(nhanes, flag_subpop_m == 1)); summary(men)

all2 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth), 
                design=subset(nhanes, flag_subpop_t == 1)); summary(all2)

wom2 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth), 
                design=subset(nhanes, flag_subpop_w == 1)); summary(wom2)

men2 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth), 
                design=subset(nhanes, flag_subpop_m == 1)); summary(men2)

# RA + PCE #
all <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth) + pce_risk, 
                design=subset(nhanes, flag_subpop_t == 1)); summary(all)

wom <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth) + pce_risk, 
                design=subset(nhanes, flag_subpop_w == 1)); summary(wom)

men <- svycoxph(Surv(time_exm,cvd_outcome)~factor(flag_rhmtd_arth) + pce_risk,
                design=subset(nhanes, flag_subpop_m == 1)); summary(men)

all2 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth) + pce_risk, 
                 design=subset(nhanes, flag_subpop_t == 1)); summary(all2)

wom2 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth) + pce_risk, 
                 design=subset(nhanes, flag_subpop_w == 1)); summary(wom2)

men2 <- svycoxph(Surv(time_exm,cvd_outcome2)~factor(flag_rhmtd_arth) + pce_risk, 
                 design=subset(nhanes, flag_subpop_m == 1)); summary(men2)



