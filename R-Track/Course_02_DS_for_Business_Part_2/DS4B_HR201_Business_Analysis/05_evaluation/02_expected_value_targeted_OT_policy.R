# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)


# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline_rev1.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>%
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_0_AutoML_20180503_035824")

automl_leader

# * OPTIONAL (rerunning h2o.automl() ) ----

train_h2o <- as.h2o(train_tbl)

y <- "Attrition"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
    x = x,
    y = y,
    training_frame   = train_h2o,
    max_runtime_secs = 30,
    nfolds           = 5
)

automl_leader <- automl_models_h2o@leader

# 3. Primer: Working With Threshold & Rates ----




# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

# 4.2 Calculating Expected Value With Targeted OT ----

# 4.3 Savings Calculation ----



# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----

# 5.2 Optimization ----



# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

# 6.2 Sensitivity Analysis ----
