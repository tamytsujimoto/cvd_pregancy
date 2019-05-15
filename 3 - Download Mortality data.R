library(tidyverse)

# Downloaded the files and saved in a local repo:
data_path = '../3 - Databases/mortality_data'

# Function to read mortality dataset based on code from 
# (ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/R_ReadInProgramAllSurveys.R)

read_mortality_data = function(data_name){
  
  cycle = substr(data_name,40,48)
    
  dsn <- 
    read_fwf(file=data_name,
                  col_types = "ciiiiiiiddii",
                  fwf_cols(publicid = c(1,14),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           dodqtr = c(22,22),
                           dodyear = c(23,26),
                           wgt_new = c(27,34),
                           sa_wgt_new = c(35,42),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = ".") %>% 
    mutate(SEQN = as.numeric(substr(publicid,1,5)), # create the ID (SEQN) for the NHANES surveys
           cycle = sub("_", "-", cycle)) %>% 
    select(-c(publicid, dodqtr, dodyear, wgt_new, sa_wgt_new)) # Drop NHIS variables
  
  return(dsn)

}

# Function to stack mortality datasets

stack_mort_data = function(path){
  
  data_names = list.files(path = path)
  
  db = 
    lapply(paste(path, data_names, sep = '/'), read_mortality_data) %>% 
    bind_rows()
  
  return(db)
}


mortality = stack_mort_data(path = data_path)
