


impute_var <- function(var, cov, family){
  
  form <- paste(var, paste(cov, collapse = '+'), sep = '~')
  
  fit <- svyglm(as.formula(form),
                family = get(family),
                design = subset(nhanes, flag_subpop == 1))
  imp <- 
    cvd_data %>% 
    select(SEQN,
           cov) %>% 
    filter(complete.cases(.)) %>% 
    mutate(var_IMP = predict(fit, ., type = "response")) %>% 
    select(SEQN, contains('IMP')) %>% 
    rename(!!paste0(var, '_IMP') := var_IMP)
  
  # result <- list(fit = fit,
  #                data = imp)
  
  return(imp)
  
}


