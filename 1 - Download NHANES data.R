library(RNHANES)
library(tidyverse)

##################################
# SAME NAME DATASETS (1999-2014) #
##################################

yr = paste(seq(1999, 2013, by = 2), seq(2000, 2014, by = 2), sep = "-")

# Demographic Variables & Sample Weights
nhanes_load_data(file = "DEMO", 
                 year = yr, 
                 destination = "nhanes_data")

# Blood Pressure
nhanes_load_data(file = "BPX", 
                 year = yr, 
                 destination = "nhanes_data")

# Blood Pressure & Cholesterol
nhanes_load_data(file = "BPQ", 
                 year = yr, 
                 destination = "nhanes_data")

# Smoking - Cigarette/Tobacco Use - Adult 
nhanes_load_data(file = "SMQ", 
                 year = yr, 
                 destination = "nhanes_data")

# Diabetes
nhanes_load_data(file = "DIQ", 
                 year = yr, 
                 destination = "nhanes_data")

# Reproductive Health 
nhanes_load_data(file = "RHQ", 
                 year = yr, 
                 destination = "nhanes_data")

########################################
# DIFFERENT NAMES DATASETS (1999-2014) #
########################################

# Cholesterol 
nhanes_load_data(file = c('Lab13', 'l13_B', 'l13_C', rep('TCHOL', 5)), 
                 year = yr, 
                 destination = "nhanes_data")

nhanes_load_data(file = 'HDL', 
                 year = yr[-c(1:3)], 
                 destination = "nhanes_data")

# Cotinine 
nhanes_load_data(file = c('LAB06', 'L06_B', 'L06COT_C', 'COT_D', 'COTNAL_E', 'COTNAL_F', 'COTNAL_G', 'COT_H'), 
                 year = yr, 
                 destination = "nhanes_data")

# Glycohemoglobin 
nhanes_load_data(file = c('LAB10', 'L10_B', 'L10_C', rep('GHB', 5)), 
                 year = yr, 
                 destination = "nhanes_data")


