


cvd_desc = function(cat, cont, filename) {

#######
# NAs #
#######

all <- vector(length = length(cat)+length(cont))
all <- c(cat,cont)

# Count NA
na = 
  cvd_final[,c(all,'flag_subpop')] %>%
  filter(flag_subpop == 1) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  gather(key = var, value = n.na) %>%
  filter(var!='flag_subpop')

# Count Valid
valid = 
  cvd_final[,c(all,'flag_subpop')] %>%
  filter(flag_subpop == 1) %>%
  summarise_all(funs(sum(!is.na(.)))) %>%
  gather(key = var, value = n.valid) %>%
  filter(var!='flag_subpop')

perc.na = 
  na %>% 
  left_join(valid, by = 'var') %>% 
  mutate(perc.na = n.na/(n.valid+n.na))
  
##############
# STATISTICS #
##############

result.cat <- result.cont <- data.frame(var = NA, 
                    level = NA, 
                    freq = NA,
                    freq.perc = NA,
                    mean = NA, 
                    se = NA,
                    q0 = NA,
                    q25 = NA,
                    q50 = NA,
                    q75 = NA,
                    q100 = NA)

#cat#

for(i in 1:length(cat)){
  
  var = cat[i]
  
  db = as.data.frame(cvd_final[,c(var,'flag_subpop')]) %>% filter(flag_subpop == 1)
  freq = data.frame(table(db[,1]))
  freq.perc = data.frame(prop.table(table(db[,1])))
  mean = data.frame(svymean(as.formula(paste0('~',paste(cat[i]))), 
                 subset(nhanes, flag_subpop == 1), na.rm = TRUE))
  
  aux = data.frame(var = var,
                   level = freq$Var1,
                   freq = freq$Freq,
                   freq.perc = freq.perc$Freq,
                   mean = mean$mean,
                   se = mean$SE)
  
  result.cat <- 
    result.cat %>% 
    bind_rows(data.frame(var = var, 
              level = NA, 
              freq = NA,
              freq.perc = NA,
              mean = NA, 
              se = NA,
              q0 = NA,
              q25 = NA,
              q50 = NA,
              q75 = NA,
              q100 = NA)) %>% 
    bind_rows(aux)
  
}

#cont#

for(i in 1:length(cont)){
  
  var = cont[i]
  mean = data.frame(svymean(as.formula(paste0('~',paste(cont[i]))), 
                            subset(nhanes, flag_subpop == 1), na.rm = TRUE))
  quant = data.frame(svyquantile(as.formula(paste0('~',paste(cont[i]))), 
                     subset(nhanes, flag_subpop == 1), c(0,.25,.5,.75,1), na.rm = TRUE))
  
  aux = data.frame(var = var,
                   mean = mean[,1],
                   se = mean[,2],
                   q0 = quant[,1],
                   q25 = quant[,2],
                   q50 = quant[,3],
                   q75 = quant[,4],
                   q100 = quant[,5])
  
  result.cont <- bind_rows(result.cont, aux)
  
}

####################
# COMBINING RESULT #
####################

result.cont %>% 
  bind_rows(result.cat) %>% 
  left_join(perc.na, by = 'var') %>% 
  filter(!is.na(var)) %>% 
  select(var, level, n.na, perc.na, freq, freq.perc, mean, se, q0, q25, q50, q75, q100) %>% 
  write.csv(file = paste0('Output/', filename, '.csv'), row.names = FALSE, na="")

}


