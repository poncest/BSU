## HR_201_Employee_Attrition_Project
## Steven Ponce 2022

# DATA PREPARATION ----
# Machine Readable ----


# Setup ----

# Libraries
library(pacman)
p_load(tidyverse, tidyquant, readxl, recipes)


# Data
path_train            <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'))
path_test             <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_test.xlsx'))
path_data_definitions <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_data_definitions.xlsx'))

 
train_raw_tbl         <- read_excel(path_train, sheet = 1)
test_raw_tbl          <- read_excel(path_test, sheet = 1)
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)


# Processing Pipeline - for machine readability
source(here::here('R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/data_processing_pipeline_rev1.R'))
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# Custom Function plot_hist_facet() ----

data <- train_raw_tbl
    
plot_hist_facet <- function(data, bins = 10, ncol = 5,
                            fct_reorder = FALSE, fct_rev = FALSE,
                            fill = palette_light()[[3]],
                            color = 'white', scale = 'free') {
    
    data_factored <- data %>% 
        mutate_if(is.character, as.factor) %>% 
        mutate_if(is.factor, as.numeric) %>% 
        gather(key = key, value = value, factor_key = TRUE)
    
    if(fct_reorder) {
        data_factored <- data_factored %>% 
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if(fct_rev) {
        data_factored <- data_factored %>% 
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>% 
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = bins, fill = fill, color = color) + 
        facet_wrap(~ key, ncol = ncol, scale = scale) + 
        theme_tq()
    
    return(g)
    
}

#Testing plot_hist_facet function
train_raw_tbl %>% 
    plot_hist_facet(bins = 10, ncol = 5)
                            
train_raw_tbl %>% 
    select(Attrition, everything()) %>% 
    plot_hist_facet(bins = 10, ncol = 5, fct_rev = FALSE)


# Data Processing with Recipes ----

# Plan: Correlation Analysis 
# 1. Impute / Zero Variance Features ----
    # If we don't have missing values, and we don't need to remove outliers (Impute)
    # then we could focus on removing 'zero variance' features (ZV)
    # e.g. `EmployeeCount`, `Over18`, etc.

# setting up the template
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
    # set up attrition as the outcome
    # then select all other variables as predictor (~ .)
    
    # zero variance step
    step_zv(all_predictors())

# prep the recipe for calculations
recipe_obj %>% 
    prep() %>%                            # does not transform the data
    bake(new_data = train_readable_tbl)   # transform the data

# 3-part-process:
# a. create the instructions with recipes and steps 
# b. prepare the recipe
# c. bake new data

# 2. Transformation ---
    # Changes the data to remove skew, stabilize variance, etc.
    # e.g. `DistanceFromHome`, `MonthlyIncome`, etc.

# filtering out skewness
skewed_features_names <- train_readable_tbl %>% 
    select_if(is.numeric) %>% 
    # high skewness features have either high positive or high negative values
    map_df(skewness) %>% 
    # transpose the data
    gather(factor_key = TRUE) %>% 
    arrange(desc(value)) %>% 
    # selecting the features - pick a cutoff
    filter(value > 0.8) %>% 
    pull(key) %>% 
    as.character()

# skewed features only
# separating non-continuous features
# e.g. `JobLevel`, `StockOptionLevel` are a factors
train_readable_tbl %>% 
    select(skewed_features_names) %>% 
    plot_hist_facet()

# remove non-continuous features
!skewed_features_names %in% c('JobLevel', 'StockOptionLevel')
    
skewed_features_names <- train_readable_tbl %>% 
    select_if(is.numeric) %>% 
    # high skewness features have either high positive or high negative values
    map_df(skewness) %>% 
    # transpose the data
    gather(factor_key = TRUE) %>% 
    arrange(desc(value)) %>% 
    # selecting the features - pick a cutoff
    filter(value > 0.8) %>% 
    filter(!skewed_features_names %in% c('JobLevel', 'StockOptionLevel')) %>% 
    pull(key) %>% 
    as.character()

# verifying the removal of factors
train_readable_tbl %>% 
    select(skewed_features_names) %>% 
    plot_hist_facet()

factor_names <- c('JobLevel', 'StockOptionLevel')


# Transforming skewed features
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
    step_zv(all_predictors()) %>% 
    # transformation - remove skewness
    step_YeoJohnson(skewed_features_names) %>% 
    # convert numbers to factors
    step_mutate_at(factor_names, fn = as.factor)
    
# verifying that we have successfully remove the skweness
recipe_obj %>% 
    prep() %>% 
    bake(train_readable_tbl) %>% 
    select(skewed_features_names) %>% 
    plot_hist_facet()

# 3. Discretize ----
    # Making a continuous variable discrete. Sometimes it can hurt correlations.
    # It's often best no to discretize
    # we're going to SKIP this step

# 4. Center/Scaling ----
    # Getting the data onto a consistent scale   

 

 

# 5. Dummy Variables ----
    # Turning categorical data into separate columns of zeros and ones.
    # This is important for ML algorithms to detect patterns in unordered data

# 6. Interaction Variables / Engineered Features ---
    # When two features have a relationship to each other they are said to interact
    # An examples is the ratio of height and weight of a person
    # we're going to SKIP this step

# 7. Multivariate Transformation ----
    # Examples includes (PCA) for dimensionality reduction. Useful in cases where
    # the data is very wide and can be susceptible to overfitting.
    # we're going to SKIP this step












