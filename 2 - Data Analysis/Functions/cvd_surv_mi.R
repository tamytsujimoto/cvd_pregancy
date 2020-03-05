
# Function to run survival analysis with survey weights
# imp: mids (multiply imputed data set) from mice::mice


cvd_surv_mi <- function(imp, var, cov='', time, out, subpop){
  
  # INITIALIZING 
  m <- imp$m
  coef.imp <- vector("list", m)
  cov.imp <- vector("list", m)
  aux <- vector("list", m)
  fit.imp <- vector("list", m)
  
  # FORMULA FOR COVARIATES CONSIDERED IN THE MODEL
  cov.f <- ifelse(cov != '', paste0(c(var,cov), collapse = '+'), var)
  
  ###################################
  # FITTING MODEL FOR OBSERVED DATA #
  ###################################
  
  obs <- 
    cvd_data %>% 
    mutate(pce_risk = pce_risk*100,
           PAG_MINW = PAG_MINW/60)
  
  nhanes.obs <- svydesign(id=~SDMVPSU, 
                      strata=~SDMVSTRA, 
                      nest=TRUE, 
                      weights=~WTDR_C1, 
                      data=obs)
  
  fit.obs <- svycoxph(as.formula(paste("Surv(",time, ",", out, ") ~ ",cov.f)), 
                      design=subset(nhanes.obs, get(subpop) == 1))
  
  coef.obs <- fit.obs$coefficients
  
  ##################################
  # FITTING MODEL FOR IMPUTED DATA #
  ##################################
  
  for(i in 1:m){
    
    # Creating complete dataset
    comp <- 
      cvd_data %>% 
      select(SEQN,
             SDMVPSU,
             SDMVSTRA,
             WTDR_C1,
             flag_subpop,
             flag_infnt_sga,
             flag_any_brstfd_1m,
             time_exm,
             time_exm2,
             cvd_outcome,
             cvd_outcome2) %>% 
      left_join(mice::complete(imp, action = i), by = 'SEQN') %>% 
      mutate(pce_risk = pce_risk*100,
             PAG_MINW = PAG_MINW/60)
    
    # Specifyng survey design
    
    nhanes.imp <- svydesign(id=~SDMVPSU, 
                        strata=~SDMVSTRA, 
                        nest=TRUE, 
                        weights=~WTDR_C1, 
                        data=comp)
    
    fit[[i]] <- svycoxph(as.formula(paste("Surv(",time, ",", out, ") ~ ",cov.f)), 
                         design=subset(nhanes.imp, get(subpop) == 1))
  }
  
  #################################################
  # COMPUTING ESTIMATES AFTER MULTIPLE IMPUTATION #
  #################################################
  
  for(i in 1:m) {
    
    coef.imp[[i]] <- fit[[i]]$coefficients 
    cov.imp[[i]] <- fit[[i]]$var
    aux[[i]] <- (coef.imp[[i]] - coef.obs)%*%t((coef.imp[[i]] - coef.obs))
    
  }
  
  coef <- Reduce("+", coef.imp)/5
  W <- Reduce("+", cov.imp)/5
  B <- Reduce("+", aux)/4
  cov <- W + (1 + 1/m)*B

  ####################
  # COMPUTING RESULT #
  ####################
  
  result <- 
    data.frame(outcome = out,
               variable = names(coef),
               n = fit[[1]]$n,
               n_event = fit[[1]]$nevent) %>% 
    mutate(coef = coef,
           std = sqrt(diag(cov)),
           exp_coef = exp(coef),
           exp_coef_L = exp(coef - qnorm(.975)*std),
           exp_coef_U = exp(coef + qnorm(.975)*std)
           )
  
  return(result)
  
}