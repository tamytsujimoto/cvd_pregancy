


cvd_logistic <- function(cov, subpop){
  
  n.cov = length(cov)
  outcome = c('cvd_outcome', 'cvd_outcome2')
  risk = c('pce_risk', 'pce_risk_cat')
  
  result <- data.frame()
  
  for(i in 1:n.cov){
    for(j in 1:2){
      for(k in 1:2){
        
        fit <- summary(svyglm(as.formula(paste(outcome[k], paste(cov[i], risk[j], sep = '+'), sep = '~')),
                              family = binomial(link="logit"),
                              design = subset(nhanes, get(subpop) == 1)))$coeff
        
        result <- 
          result %>% 
          bind_rows(data.frame(var = cov[i],
                               risk = risk[j],
                               outcome = outcome[k],
                               parameter = row.names(fit),
                               estimate = fit[,1],
                               sd = fit[,2],
                               pval = fit[,4]))
        
      }
    }  
  }
  
  result <-
    result %>% 
    mutate(or = exp(estimate),
           or.ci.l = exp(estimate - qnorm(0.975)*sd),
           or.ci.u = exp(estimate + qnorm(0.975)*sd),
           subpop = subpop) %>% 
    select(subpop, var:sd, or:or.ci.u, pval)
  
  return(result)
}





