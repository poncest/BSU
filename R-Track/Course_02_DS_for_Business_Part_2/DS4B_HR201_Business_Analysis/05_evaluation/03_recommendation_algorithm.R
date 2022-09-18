# RECOMMENDATION ALGORITHM ----

# 1.0 Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)    # Make sure v0.1.3 or later is installed. If not restart & install.packages("recipes") to update.


# Load Data
path_train            <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'))
path_test             <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_test.xlsx'))
path_data_definitions <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_data_definitions.xlsx'))


train_raw_tbl         <- read_excel(path_train, sheet = 1)
test_raw_tbl          <- read_excel(path_test, sheet = 1)
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)



# Processing Pipeline
source(here::here("R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/data_processing_pipeline_rev1.R"))
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)



# 2.0 Correlation Analysis - Machine Readable ----
source(here::here("R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/plot_cor.R"))


# 2.1 Recipes ----
train_readable_tbl

# factor names
factor_names <- c("JobLevel", "StockOptionLevel")

# Recipe 
# modified the final recipe from `02_data_preparation_machine_readable.R`

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
    step_zv(all_predictors()) %>% 
    step_mutate_at(factor_names, fn = as.factor) %>% 
    step_discretize(all_numeric(), min_unique = 1) %>%             # bins
    step_dummy(all_nominal(), one_hot = TRUE) %>% 
    prep()

recipe_obj

train_corr_tbl <- bake(object = recipe_obj, new_data = train_readable_tbl)
train_corr_tbl %>% glimpse()

tidy(recipe_obj)
tidy(recipe_obj, number = 3)
 

# 2.2 Correlation Visualization ----

# Manipulate Data

correlation_results_tbl <- train_corr_tbl %>% 
    select(-Attrition_No) %>% 
    get_cor(target = Attrition_Yes, fct_reorder = TRUE, fct_rev = TRUE) %>% 
    filter(abs(Attrition_Yes) >= 0.02) %>% 
    mutate(
        relationship = case_when(
            Attrition_Yes > 0 ~ "Supports",
            TRUE              ~ "Contradicts"
        )
    ) %>% 
    
    mutate(feature_text = as.character(feature)) %>% 
    separate(feature_text, into = "feature_base", sep = "_", extra = "drop") %>% 
    mutate(feature_base = as_factor(feature_base) %>%  fct_rev())
    

correlation_results_tbl %>% 
    select(feature_base) %>% 
    mutate(level = as.numeric(feature_base))


# Create Visualization 



# 3.0 Recommendation Strategy Development Worksheet ----




# 4.0 Recommendation Algorithm Development ----

# 4.1 Personal Development (Mentorship, Education) ----


# 4.2 Professional Development (Promotion Readiness) ----


# 4.3 Work Life Balance ----




