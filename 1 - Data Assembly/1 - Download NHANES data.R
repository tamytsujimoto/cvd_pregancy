library(RNHANES)
library(tidyverse)

##################################
# SAME NAME DATASETS (1999-2014) #
##################################

yr = paste(seq(1999, 2013, by = 2), seq(2000, 2014, by = 2), sep = "-")
dest_path = "../../3 - Databases/nhanes_data"

# Demographic Variables & Sample Weights
nhanes_load_data(file = "DEMO", 
                 year = yr, 
                 destination = dest_path)

# Blood Pressure
nhanes_load_data(file = "BPX", 
                 year = yr, 
                 destination = dest_path)

# Blood Pressure & Cholesterol
nhanes_load_data(file = "BPQ", 
                 year = yr, 
                 destination = dest_path)

# Smoking - Cigarette/Tobacco Use - Adult 
nhanes_load_data(file = "SMQ", 
                 year = yr, 
                 destination = dest_path)

# Diabetes
nhanes_load_data(file = "DIQ", 
                 year = yr, 
                 destination = dest_path)

# Reproductive Health 
nhanes_load_data(file = "RHQ", 
                 year = yr, 
                 destination = dest_path)

# Medical Conditions 
nhanes_load_data(file = "MCQ", 
                 year = yr, 
                 destination = dest_path)

# Body Measure
nhanes_load_data(file = "BMX", 
                 year = yr, 
                 destination = dest_path)

# MET Score
nhanes_load_data(file = "PAQIAF", 
                 year = yr[1:4], 
                 destination = dest_path)

########################################
# DIFFERENT NAMES DATASETS (1999-2014) #
########################################

# Cholesterol 
nhanes_load_data(file = c('Lab13', 'l13_B', 'l13_C', rep('TCHOL', 5)), 
                 year = yr, 
                 destination = dest_path)

nhanes_load_data(file = 'HDL', 
                 year = yr[-c(1:3)], 
                 destination = dest_path)

# Cotinine 
nhanes_load_data(file = c('LAB06', 'L06_B', 'L06COT_C', 'COT_D', 'COTNAL_E', 'COTNAL_F', 'COTNAL_G', 'COT_H'), 
                 year = yr, 
                 destination = dest_path)

# Glycohemoglobin 
nhanes_load_data(file = c('LAB10', 'L10_B', 'L10_C', rep('GHB', 5)), 
                 year = yr, 
                 destination = dest_path)

# Oral Glucose Tolerance Test 
nhanes_load_data(file = 'OGTT', 
                 year = yr[-c(1:3)], 
                 destination = dest_path)

# Plasma Fasting Glucose
nhanes_load_data(file = c('LAB10AM', 'L10AM_B', 'L10AM_C', rep('GLU', 5)), 
                 year = yr, 
                 destination = dest_path)

# MET Score
nhanes_load_data(file = c('LAB11', 'L11_B', 'L11_C', rep('CRP', 3)), 
                 year = yr[-c(7:8)], 
                 destination = dest_path)

