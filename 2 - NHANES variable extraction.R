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
         cycle,
         RIAGENDR,
         RIDRETH1,
         RIDRETH2,
         RIDRETH3,
         RIDAGEYR,
         RIDAGEMN,
         RIDAGEEX,
         DMQMILIT,
         DMQMILIZ,
         DMDBORN,
         DMDCITZN,
         DMDYRSUS,
         DMDEDUC2,
         DMDMARTL,
         DMDHHSIZ,
         DMDFMSIZ,
         INDHHINC,
         INDHHIN2,
         INDFMINC,
         INDFMIN2,
         INDFMPIR,
         RIDEXPRG,
         WTINT2YR,
         WTINT4YR,
         WTMEC2YR,
         WTMEC4YR)

# Blood Pressure
bpx = 
  stack_nhanes_data(pattern = 'BPX') %>% 
  select(SEQN,
         cycle,
         BPXSY1,
         BPXSY2,
         BPXSY3,
         BPXSY4,
         BPXSAR,
         BPXDI1,
         BPXDI2,
         BPXDI3,
         BPXDI4,
         BPXDAR,
         BPAEN1,
         BPAEN2,
         BPAEN3,
         BPAEN4,
         BPQ150A,
         BPQ150B,
         BPQ150C,
         BPQ150D)

# Blood Pressure & Cholesterol
bpq = 
  stack_nhanes_data(pattern = 'BPQ') %>% 
  select(SEQN,
         cycle,
         BPQ020,
         BPQ030,
         BPQ040A,
         BPQ050A,
         BPQ080,
         BPQ090D,
         BPQ100D)

# Smoking - Cigarette/Tobacco Use - Adult 
smq = 
  stack_nhanes_data(pattern = 'SMQ') %>% 
  select(SEQN,
         cycle,
         SMQ020,
         SMD030,
         SMQ040,
         SMQ050Q,
         SMQ050U)

# Diabetes  
diq = 
  stack_nhanes_data(pattern = 'DIQ') %>% 
  select(SEQN,
         cycle,
         DIQ010,
         DIQ040Q,
         DID040Q,
         DID040,
         DIQ050,
         DIQ070,
         DID070,
         DIQ280)

# Reproductive Health          
rhq = 
  stack_nhanes_data(pattern = 'RHQ') %>% 
  select(SEQN,
         cycle,
         RHQ010,
         RHQ030,
         RHQ031,
         RHQ031,
         RHQ040,
         RHD042,
         RHD043,
         RHQ060,
         RHD130,
         RHQ131,
         RHQ140,
         RHQ141,
         RHD143,
         RHD152,
         RHQ160,
         RHQ166,
         RHD170,
         RHQ171,
         RHQ172,
         RHD173,
         RHQ180,
         RHD180,
         RHQ190,
         RHD190,
         RHQ420,
         RHQ540,
         RHQ570,
         RHQ574,
         RHQ596,
         RHD440,
         RHD442,
         RHQ300,
         RHQ310,
         RHQ305,
         RHQ330,
         RHQ332,
         RHD270,
         RHQ250,
         RHQ260,
         RHQ205,
         RHQ200,
         RHQ210,
         RHD230,
         RHQ162,
         RHQ163)

# Cholesterol 
chol = 
  stack_nhanes_data(pattern = paste(c('Lab13', 'l13_b', 'l13_c', 'TCHOL'), collapse = "|")) %>% 
  select(SEQN,
         cycle,
         LBXTC,
         LBDHDL,
         LBXHDD)

# HDL 
hdl = 
  stack_nhanes_data(pattern ='HDL') %>% 
  select(SEQN,
         cycle,
         LBDHDD)

# Cotinine 
cot = 
  stack_nhanes_data(pattern = paste(c('LAB06', 'L06_B', 'L06COT_C', 'COT_D', 'COTNAL_E', 'COTNAL_F', 'COTNAL_G', 'COT_H'), collapse = "|")) %>% 
  select(SEQN,
         cycle,
         LBXCOT)

# Glycohemoglobin 
glyc = 
  stack_nhanes_data(pattern = paste(c('LAB10.csv', 'L10_B', 'L10_C', 'GHB'), collapse = "|")) %>% 
  select(SEQN,
         cycle,
         LBXGH)

# Medical Conditions 
mcq = 
  stack_nhanes_data(pattern = 'MCQ') %>% 
  select(SEQN,
         cycle,
         MCQ160A,
         MCQ160B,
         MCQ160C,
         MCQ160D,
         MCQ160E,
         MCQ160F,
         MCQ180A,
         MCQ190,
         MCQ191,
         MCQ195)

# Oral Glucose Tolerance Test 
ogt = 
  stack_nhanes_data(pattern = 'OGTT') %>% 
  select(SEQN,
         cycle,
         LBXGLT,
         WTSOG2YR)

# Plasma Fasting Glucose
glu = 
  stack_nhanes_data(pattern = paste(c('LAB10AM', 'L10AM_B', 'L10AM_C', 'GLU'), collapse = "|")) %>% 
  select(SEQN,
         cycle,
         WTSAF2YR,
         WTSAF4YR,
         LBXGLU)



