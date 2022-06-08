
## HR_201_Employee_Attrition_Project

# DATA UNDERSTANDING ----

# Libraries
library(pacman)
p_load(tidyverse, tidyquant, readxl, skimr, GGally)


# Load Data
# train
path_train            <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'
path_data_definitions <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_data_definitions.xlsx'

train_raw_tbl         <- read_excel(path_train, sheet = 1)
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

definitions_raw_tbl
