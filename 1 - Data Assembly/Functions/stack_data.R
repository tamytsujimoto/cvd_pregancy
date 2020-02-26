stack_data = function(path, pattern, type = 'csv'){
  
  data_names = list.files(path = path, pattern = pattern)
  
  if(type == 'csv') db = lapply(paste(path, data_names, sep = '/'), data.table::fread) %>% bind_rows()
  else if(type == 'sas') db = lapply(paste(path, data_names, sep = '/'), haven::read_sas) %>% bind_rows()
  else print('Error: only csv or sas datasets allowed')

  return(db)
}