var = 'flag_marit_1'
cov = cov.model[!cov.model %in% 'flag_marit_1']
family = 'binomial'
B = 5


impute_var <- function(var, cov, family, B){
  
  # Selecting dataset to be imputed
  var.mis <- 
    cvd_data %>% 
    filter(flag_subpop == 1) %>% 
    filter(is.na(get(var))) %>% 
    select(SEQN, cov)
  
  imp <- 
    var.mis %>% 
    select(SEQN)
  
  # Running regression with observed data
  
  form <- paste(var, paste(cov, collapse = '+'), sep = '~')
  
  fit <- svyglm(as.formula(form),
                family = get(family),
                design = subset(nhanes, flag_subpop == 1))
  
  # Generating B coefficients from MVN
  
  coef_boot <- mvtnorm::rmvnorm(n = B,
                                mean = fit$coefficients,
                                sigma = vcov(fit))
  
  # Imputing missing values for variable var
  
  for(i in 1:B){
    
    var_name = paste(var, "imp", i, sep = "_")
    
    imp <- 
      imp %>% 
      cbind(var.mis %>% 
              mutate(lin_pred = as.vector(coef_boot[i,1] + as.matrix(var.mis)[,-1]%*%as.matrix(coef_boot[i,-1])),
                     prob_pred = exp(lin_pred)/(1+exp(lin_pred)),
                     !!var_name := rbinom(n(), 1, prob = prob_pred)) %>% 
              select(!!var_name))

  }
  
  imp2 <-
    cvd_data %>% 
    select(SEQN, flag_subpop, var) %>% 
    left_join(imp, by = 'SEQN') %>% 
    mutate_at(vars(contains('imp')), funs(ifelse(is.na(.), get(var), .)))
    
  
  return(imp)
  
}


