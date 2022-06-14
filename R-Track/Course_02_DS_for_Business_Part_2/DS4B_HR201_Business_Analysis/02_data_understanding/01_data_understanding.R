
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
   

# Custom Function: plot_ggpairs()

# Arguments: data, color, and, density_alpha
plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5){
    
    color_expr <- enquo(color)
    
    if (rlang::quo_is_null(color_expr)) {
        
        g <- data %>% 
            ggpairs(lower = 'blank')
        
    } else {
        
        color_name <- quo_name(color_expr)
        
        g <- data %>% 
            ggpairs(mapping = aes_string(color = color_name),
                    lower = 'blank', legend = 1,
                    diag = list(continous = wrap('densityDiag',
                                                 alpha = density_alpha))) +
            
            theme(legend.position = 'bottom')
    }
    
    return(g)
}

# testing function
train_raw_tbl %>% 
    select(Attrition, Age, Gender, MaritalStatus,NumCompaniesWorked,
           Over18, DistanceFromHome) %>% 
    plot_ggpairs(color = Attrition)
    

# Explore Features by Category

# 1. Descriptive features: age, gender, marital status
train_raw_tbl %>% 
    select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
    plot_ggpairs(Attrition)

# 2. Employment features: department, job role, job level
train_raw_tbl %>% 
    select(Attrition, contains('employee'), contains('department'), contains('job')) %>% 
    plot_ggpairs(Attrition)

# 3. Compensation features: hourly rate, monthly income, stock option level
train_raw_tbl %>% 
    select(Attrition, contains('income'), contains('rate'), contains('salary'), contains('stock')) %>% 
    plot_ggpairs(Attrition)

# 4. Survey results: satisfaction level, work life balance
train_raw_tbl %>% 
    select(Attrition, contains('satisfaction'),  contains('life')) %>% 
    plot_ggpairs(Attrition)

# 5. Performance data: job involvement, performance rating
train_raw_tbl %>% 
    select(Attrition, contains('performance'),  contains('involvement')) %>% 
    plot_ggpairs(Attrition)

# 6. work life features
train_raw_tbl %>% 
    select(Attrition, contains('overtime'),  contains('travel')) %>% 
    plot_ggpairs(Attrition)

# 7. training and education
train_raw_tbl %>% 
    select(Attrition, contains('training'),  contains('education')) %>% 
    plot_ggpairs(Attrition)

# 8. time based features: year at company and year in current role
train_raw_tbl %>% 
    select(Attrition, contains('years')) %>% 
    plot_ggpairs(Attrition)






