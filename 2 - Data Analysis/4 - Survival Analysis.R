library(tidyverse)
library(survey)

# Loading 
cvd_imp =
  readRDS(file = '../1 - Data Assembly/Datasets/cvd_IMP.rds')

# Loading dataset and creating censoring time at the end of follow up #

cvd_data =
  readRDS(file = '../1 - Data Assembly/Datasets/cvd_final.rds') %>% 
  left_join(cvd_imp, by = c('SEQN', 'cycle')) %>% 
  separate(cycle, c("cycle1", "cycle2"), remove=FALSE) %>% 
  mutate(exam_date = as.Date(ifelse(RIDEXMON == 1, 
                            paste("02/01", cycle2, sep="/"),
                            paste("08/01", cycle2, sep="/")), "%m/%d/%Y"),
         fup_time = as.numeric(round((as.Date("2015-12-31") - exam_date)/(365.25/12))),
         time_exm2 = ifelse(mortstat == 1 & cvd_outcome == 0 & fup_time > time_exm, 
                            fup_time,
                            time_exm),
         pce_risk_IMP = pce_risk_IMP*100,
         PAG_MINW = PAG_MINW/60) %>% 
  mutate_at(vars(flag_infnt_sga,
                 flag_any_brstfd,
                 flag_any_brstfd_1m,
                 brstfd,
                 flag_marit_1,
                 flag_educ_hs,
                 flag_parity_gt1), list(~as.factor(.))) %>% 
  filter(cohort == 1, diet_recall == 1) 

  
##########################
# DEFINING SURVEY DESIGN #
##########################

# Using sampling weight from diet data

nhanes <- svydesign(id=~SDMVPSU, 
                    strata=~SDMVSTRA, 
                    nest=TRUE, 
                    weights=~WTDR_C1, 
                    data=cvd_data)

################
# KAPLAN-MEIER #
################

source('4.3 - KM plots.R')

################
# COX PH MODEL #
################

source('cvd_surv.R')

# Covariates #
cov = c('flag_marit_1',
        'flag_educ_hs',
        'flag_parity_gt1',
        'age_fst_live_brth',
        'HEI2015_TOTAL_SCORE',
        'PAG_MINW',
        'bmi',
        'pce_risk')

# Preg #

sga <- cvd_surv(var='flag_infnt_sga', time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

sga2 <- cvd_surv(var='flag_infnt_sga', time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

bf <- cvd_surv(var='flag_any_brstfd_1m', time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

bf2 <- cvd_surv(var='flag_any_brstfd_1m', time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

# Preg + cov #
sga_cov <- cvd_surv(var='flag_infnt_sga', cov = cov, time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

sga_cov2 <- cvd_surv(var='flag_infnt_sga', cov = cov, time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

bf_cov <- cvd_surv(var='flag_any_brstfd_1m', cov = cov, time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

bf_cov2 <- cvd_surv(var='flag_any_brstfd_1m', cov = cov, time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

bind_rows(sga, sga2, bf, bf2, sga_cov, sga_cov2, bf_cov, bf_cov2) %>% 
  write.csv('Output/surv.csv', row.names = FALSE)

# Shoenfeld Residuals #

# SHOENFELD RESIDUAL

plot_resid <- function(fit, title){
  
  scho <- cox.zph(fit)
  p <- ifelse(dim(scho$table)[1] == 1, 1, dim(scho$table)[1]-1)
  
  for(i in 1:p){
    plot(scho[i], col.lab = "transparent")
    title(main=title[i], ylab="Schoenfeld Residuals")
    #legend('topleft', 
           #legend = paste("p-value", ifelse(round(scho$table[i,3],3)>=0.001, format(round(scho$table[i,3], 3), nsmall = 3), '< 0.001'), sep = ": "), box.lty = 0)
    abline(0, 0, col = 'red', lwd = 2)
  }
  
}

pdf(file = 'Output/schoenfeld_cause_spec.pdf', width = 8, height = 8)

par(mfrow = c(2,2), mar = c(2, 4, 3, 1) + 0.1)
plot_resid(fit1_sga, title="CVD death - SGA")
plot_resid(fit2_sga, title="CVD death + HTN/DM - SGA")
plot_resid(fit1_bf, title="CVD death - BF")
plot_resid(fit2_bf, title=" CVD death + HTN/DM - BF")

par(mfrow = c(4,2), mar = c(2, 4, 3, 1) + 0.1)
plot_resid(fit1_sga_pce, title=c("CVD death - SGA", "CVD death - PCE"))
plot_resid(fit2_sga_pce, title=c("CVD death + HTN/DM - SGA", "CVD death + HTN/DM - PCE"))
plot_resid(fit1_bf_pce, title=c("CVD death - BF", "CVD death - PCE"))
plot_resid(fit2_bf_pce, title=c("CVD death + HTN/DM - BF", "CVD death + HTN/DM - PCE"))
par(mfrow = c(1,1), mar = c(2, 4, 3, 1) + 0.1)

dev.off()

#####################################
# FINE-GRAY MODEL - COMPETING RISKS #
#####################################

sga <- cvd_surv(var='flag_infnt_sga', time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

sga2 <- cvd_surv(var='flag_infnt_sga', time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

bf <- cvd_surv(var='flag_any_brstfd_1m', time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

bf2 <- cvd_surv(var='flag_any_brstfd_1m', time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

# Preg + cov #
sga_cov <- cvd_surv(var='flag_infnt_sga', cov = cov, time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

sga_cov2 <- cvd_surv(var='flag_infnt_sga', cov = cov, time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

bf_cov <- cvd_surv(var='flag_any_brstfd_1m', cov = cov, time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

bf_cov2 <- cvd_surv(var='flag_any_brstfd_1m', cov = cov, time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

bind_rows(sga, sga2, bf, bf2, sga_cov, sga_cov2, bf_cov, bf_cov2) %>% 
  write.csv('Output/surv2.csv', row.names = FALSE)

#################################
# COX PH MODEL - IMPUTED VALUES #
#################################

# Covariates #
cov = c('flag_marit_1_IMP',
        'flag_educ_hs_IMP',
        'flag_parity_gt1_IMP',
        'age_fst_live_brth_IMP',
        'HEI2015_TOTAL_SCORE',
        'PAG_MINW',
        'bmi_IMP',
        'pce_risk_IMP')

# Preg #

sga <- cvd_surv(var='flag_infnt_sga', time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

sga2 <- cvd_surv(var='flag_infnt_sga', time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

bf <- cvd_surv(var='flag_any_brstfd_1m', time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

bf2 <- cvd_surv(var='flag_any_brstfd_1m', time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

# Preg + cov #
sga_cov <- cvd_surv(var='flag_infnt_sga', cov = cov, time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

sga_cov2 <- cvd_surv(var='flag_infnt_sga', cov = cov, time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

bf_cov <- cvd_surv(var='flag_any_brstfd_1m', cov = cov, time = 'time_exm', out='cvd_outcome', subpop='flag_subpop')

bf_cov2 <- cvd_surv(var='flag_any_brstfd_1m', cov = cov, time = 'time_exm', out='cvd_outcome2', subpop='flag_subpop')

bind_rows(sga, sga2, bf, bf2, sga_cov, sga_cov2, bf_cov, bf_cov2) %>% 
  write.csv('Output/surv_IMP.csv', row.names = FALSE)

######################################################
# FINE-GRAY MODEL - COMPETING RISKS - IMPUTED VALUES #
######################################################

sga <- cvd_surv(var='flag_infnt_sga', time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

sga2 <- cvd_surv(var='flag_infnt_sga', time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

bf <- cvd_surv(var='flag_any_brstfd_1m', time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

bf2 <- cvd_surv(var='flag_any_brstfd_1m', time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

# Preg + cov #
sga_cov <- cvd_surv(var='flag_infnt_sga', cov = cov, time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

sga_cov2 <- cvd_surv(var='flag_infnt_sga', cov = cov, time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

bf_cov <- cvd_surv(var='flag_any_brstfd_1m', cov = cov, time = 'time_exm2', out='cvd_outcome', subpop='flag_subpop')

bf_cov2 <- cvd_surv(var='flag_any_brstfd_1m', cov = cov, time = 'time_exm2', out='cvd_outcome2', subpop='flag_subpop')

bind_rows(sga, sga2, bf, bf2, sga_cov, sga_cov2, bf_cov, bf_cov2) %>% 
  write.csv('Output/surv2_IMP.csv', row.names = FALSE)