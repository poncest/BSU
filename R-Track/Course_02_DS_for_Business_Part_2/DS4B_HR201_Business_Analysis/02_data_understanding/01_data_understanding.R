
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


# Examining the data
glimpse(train_raw_tbl)
glimpse(definitions_raw_tbl)


# Exploratory Data Analysis (EDA) ----

# Step 1: Data Summarization ----
skim(train_raw_tbl)

# character data type
train_raw_tbl %>% 
    # select only character columns
    select_if(is.character) %>% 
    glimpse()
 
train_raw_tbl %>% 
    # select only character columns
    select_if(is.character) %>% 
    # to see the levels for each chr column
    map(unique)

train_raw_tbl %>% 
    # select only character columns
    select_if(is.character) %>% 
    # proportion for each level for each chr column
    map(~ table(.) %>% prop.table())
 
# numeric data type
train_raw_tbl %>% 
    # select only numeric columns
    select_if(is.numeric) %>% 
    # proportion for each level for each num column
    map(~ unique(.) %>% length())

train_raw_tbl %>% 
    # select only numeric columns
    select_if(is.numeric) %>% 
    map_df(~ unique(.) %>% length()) %>% 
    gather() %>% 
    arrange(desc(value)) %>% 
    filter(value >= 10) 


# Step s: Data Visualization ----
train_raw_tbl %>% 
    select(Attrition, Age, Gender, MaritalStatus,NumCompaniesWorked,
           Over18, DistanceFromHome) %>% 
    
    ggpairs()


# customize 
train_raw_tbl %>% 
    select(Attrition, Age, Gender, MaritalStatus,NumCompaniesWorked,
           Over18, DistanceFromHome) %>% 
    
    ggpairs(aes(color = Attrition), lower = 'blank', legend = 1,
            diag = list(continous = wrap('densityDiag', alpha = 0.5))) + 
    
    theme(legend.position = 'bottom')
   



