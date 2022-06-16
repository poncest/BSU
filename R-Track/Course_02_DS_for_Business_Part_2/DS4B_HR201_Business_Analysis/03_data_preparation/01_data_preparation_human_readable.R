## HR_201_Employee_Attrition_Project
## Steven Ponce 2022

# DATA PREPARATION ----
# Human Readable ----

# Libraries
library(pacman)
p_load(tidyverse, tidyquant, readxl)


# Load Data
# train
path_train            <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'

path_data_definitions <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_data_definitions.xlsx'

train_raw_tbl         <- read_excel(path_train, sheet = 1)

definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)


# Processing Pipeline - for people readability
source(here::here('R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/data_processing_pipeline_rev1.R'))

train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)


# For example, lets examine readability before/after
# Before (coded)
train_raw_tbl %>% 
    ggplot(aes(Education)) +
    geom_bar()
    
# After (decoded)
train_readable_tbl %>% 
    ggplot(aes(Education)) +
    geom_bar() 
    

# Tidy data ----    
definitions_tbl <- definitions_raw_tbl %>% 
    rename(x = ...1, y = ...2) %>% 
    fill(x, .direction = 'down') %>% 
    drop_na() %>% 
    separate(col = y, into = c('key', 'value'), sep = " '") %>% 
    mutate(value = str_remove(string = value, pattern = "'") %>% str_trim()) %>% 
    rename(name = x)
    
    



