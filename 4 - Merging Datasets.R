library(tidyverse)

# Using cycle also as key to avoid dropping the variable

nhanes = 
  demo %>% 
  left_join(bpq, by = c('SEQN', 'cycle')) %>% 
  left_join(bpx, by = c('SEQN', 'cycle')) %>% 
  left_join(chol, by = c('SEQN', 'cycle')) %>% 
  left_join(cot, by = c('SEQN', 'cycle')) %>% 
  left_join(diq, by = c('SEQN', 'cycle')) %>% 
  left_join(glu, by = c('SEQN', 'cycle')) %>% 
  left_join(glyc, by = c('SEQN', 'cycle')) %>% 
  left_join(hdl, by = c('SEQN', 'cycle')) %>% 
  left_join(mcq, by = c('SEQN', 'cycle')) %>% 
  left_join(ogt, by = c('SEQN', 'cycle')) %>% 
  left_join(rhq, by = c('SEQN', 'cycle')) %>% 
  left_join(smq, by = c('SEQN', 'cycle')) %>% 
  left_join(mortality, by = c('SEQN', 'cycle'))
  
saveRDS(nhanes, file = 'nhanes_mort_complete.rds')
