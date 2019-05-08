library(tidyverse)

###################################
# SELECTING VARIABLES OF INTEREST #
###################################

# Path for the downloaded datasets
data_path = '../3 - Databases/nhanes_data'

# Function to stack NHANES dataset
stack_nhanes_data = function(path = data_path, pattern){
  
  data_names = list.files(path = path, pattern = pattern)
  
  db = 
    lapply(paste(path, data_names, sep = '/'), data.table::fread) %>% 
    bind_rows()
  
  return(db)
}

# Stacking nhanes datasets and selecting variables of interest:

# Demographic Variables & Sample Weights
demo = 
  stack_nhanes_data(pattern = 'DEMO') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         RIAGENDR,
         RIDRETH1,
         RIDRETH2,
         RIDRETH3,
         RIDAGEYR,
         RIDAGEMN,
         RIDAGEEX,
         RIDEXPRG,
         WTINT2YR,
         WTMEC2YR)

# Blood Pressure
bpx = 
  stack_nhanes_data(pattern = 'BPX') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         BPXSY1,
         BPXSY2,
         BPXSY3,
         BPXSY4,
         BPXSAR,
         BPXDI1,
         BPXDI2,
         BPXDI3,
         BPXDI4,
         BPAEN1,
         BPAEN2,
         BPAEN3,
         BPAEN4)

# Blood Pressure & Cholesterol
bpq = 
  stack_nhanes_data(pattern = 'BPQ') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         BPQ080,
         BPQ020,
         BPQ030,
         BPQ040A,
         BPQ050A,
         BPQ100D)

# Smoking - Cigarette/Tobacco Use - Adult 
smq = 
  stack_nhanes_data(pattern = 'SMQ') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         SMQ020,
         SMQ040,
         SMQ050Q,
         SMQ050U)

# Diabetes  
diq = 
  stack_nhanes_data(pattern = 'DIQ') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         DIQ010,
         DIQ050,
         DIQ070,
         DID070,
         DIQ280)

# Reproductive Health          
rhq = 
  stack_nhanes_data(pattern = 'RHQ') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         RHQ031,
         RHD042,
         #RHQ135C,
         RHD152,
         #RHQ165,
         RHQ166,
         RHQ169,
         RHQ171,
         #RHQ175,
         RHQ180,
         RHQ190,
         #RHQ195,
         RHD130,
         RHQ131,
         RHQ140,
         RHQ141,
         RHD143,
         RHQ160,
         RHD170,
         RHQ171,
         RHD270,
         RHQ250,
         RHQ260,
         RHQ210,
         RHD230,
         RHQ162)

# Cholesterol 
chol = 
  stack_nhanes_data(pattern = paste(c('Lab13', 'l13_b', 'l13_c', 'TCHOL'), collapse = "|")) %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         LBXTC,
         LBDHDL,
         LBXHDD)

# HDL 
hdl = 
  stack_nhanes_data(pattern ='HDL') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         LBDHDD)

# Cotinine 
cot = 
  stack_nhanes_data(pattern = paste(c('LAB06', 'L06_B', 'L06COT_C', 'COT_D', 'COTNAL_E', 'COTNAL_F', 'COTNAL_G', 'COT_H'), collapse = "|")) %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         LBXCOT)

# Glycohemoglobin 
glyc = 
  stack_nhanes_data(pattern = paste(c('LAB10', 'L10_B', 'L10_C', 'GHB'), collapse = "|")) %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         LBXGH)

# Medical Conditions 
mcq = 
  stack_nhanes_data(pattern = 'MCQ') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         MCQ160A,
         MCQ180A,
         MCQ190)

# Oral Glucose Tolerance Test 
ogt = 
  stack_nhanes_data(pattern = 'OGTT') %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         LBXGLT)

# Plasma Fasting Glucose
glu = 
  stack_nhanes_data(pattern = paste(c('LAB10AM', 'L10AM_B', 'L10AM_C', 'GLU'), collapse = "|")) %>% 
  select(SEQN,
         file_name,
         cycle,
         begin_year,
         end_year,
         LBXGLU)



