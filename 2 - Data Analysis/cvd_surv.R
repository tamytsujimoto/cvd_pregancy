

cvd_surv<- function(var, cov='', time, out, subpop){
  
  cov.f <- ifelse(cov != '', paste0(c(var,cov), collapse = '+'), var)
  
  fit <- svycoxph(as.formula(paste("Surv(",time, ",", out, ") ~ ",cov.f)), 
               design=subset(nhanes, get(subpop) == 1))
  
  coef <- summary(fit)$coeff %>% data.frame()
  confint <- summary(fit)$conf.int
  result <- data.frame()
  
  result <- 
    result %>% 
    bind_rows(coef[,c(1,3,2,5)]) %>% 
    mutate(exp_coef_L = confint[,3],
           exp_coef_U = confint[,4],
           variable = rownames(confint),
           outcome = out,
           n = fit$n,
           n_event = fit$nevent)
  
  colnames(result) <- c('coef', 'coef_se', 'exp_coef', 'pval','exp_coef_L', 'exp_coef_U', 'var', 'outcome', 'n', 'n_event')
  
  return(result %>% select(outcome, var, n, n_event, coef, contains('coef'), pval))
}


