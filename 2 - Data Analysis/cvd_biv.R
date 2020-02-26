
cvd_biv = function(cat, cont, subpop, by) {
  
  result <- data.frame()
  
  # CONTINUOUS #
  
  for(i in 1:length(cont)){
    
    var = cont[i]
    
    mean = svyby(as.formula(paste0('~',var)), 
              as.formula(paste0('~',by)), 
              subset(nhanes, get(subpop) == 1), svymean, vartype = 'ci', na.rm = TRUE)
    p = regTermTest(svyglm(as.formula(paste(var, by, sep = '~')), 
                           subset(nhanes, get(subpop) == 1)), 
                as.formula(paste0('~',by)))$p
    
    n = cvd_data %>% 
      filter(get(subpop) == 1) %>% 
      group_by(get(by)) %>% 
      summarise(n = sum(!is.na(get(var))))
    
    colnames(n) = c('by', 'n')
    colnames(mean) <- c('by', 'avg', 'ci_l', 'ci_u')
    
    aux =
      mean %>% 
      left_join(n, by = 'by') %>% 
      gather("stat", "value", -by) %>% 
      mutate(aux = paste0(by, stat)) %>% 
      select(value, aux) %>% 
      spread(aux, value) %>% 
      mutate(var = var,
             p = p)
    
    result <- bind_rows(result, aux)
    
    }
 
  # CATEGORICAL #
  
  for(i in 1:length(cat)){
    
    var = cat[i]
    l = with(cvd_data, levels(get(var)))
    
    mean = svyby(as.formula(paste0('~',var)), 
                 as.formula(paste0('~',by)), 
                 subset(nhanes, get(subpop) == 1), svymean, vartype = 'ci', na.rm = TRUE)
    p = svychisq(as.formula(paste0('~',paste(var, by, sep = '+'))), 
                 subset(nhanes, get(subpop) == 1), statistic = "Chisq")$p.value
    
    n = cvd_data %>% 
      filter(get(subpop) == 1, !is.na(get(var))) %>% 
      group_by(get(by), get(var)) %>% 
      summarise(n = n()) %>% 
      rowwise() %>% 
      mutate(`get(var)` = paste0('n', `get(var)`)) %>% 
      spread(`get(var)`, 'n')
    
    colnames(n)[1] <- 'by'
    
    colnames(mean) <- c('by', paste0('avg', l), paste0('ci_l', l), paste0('ci_u', l))
    
    aux =
      mean %>% 
      left_join(n, by = 'by') %>% 
      gather("stat", "value", -by) %>% 
      extract(stat, "level", "([0-9]+)", remove = FALSE) %>% 
      separate(stat, into = c("stat", "aux"), sep = "([0-9]+)") %>% 
      mutate(aux = paste0(by,stat)) %>% 
      select(value, level, aux) %>% 
      spread(aux, value) %>% 
      mutate(var = var,
             p = p)
    
    result <- bind_rows(result, aux)
  } 
  
  return(result)
  
}


