# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# NO OVERTIME POLICY ----

# 1. Setup ----
 
# Load Libraries 
library(pacman)
p_load(h2o, recipes, readxl, tidyverse, tidyquant)


# Load Data
path_train            <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'))
path_test             <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_test.xlsx'))
path_data_definitions <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_data_definitions.xlsx'))

 
train_raw_tbl         <- read_excel(path_train, sheet = 1)
test_raw_tbl          <- read_excel(path_test, sheet = 1)
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

 

# Processing Pipeline
source(here::here('R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/data_processing_pipeline_rev1.R'))
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    # remove zero variance
    step_zv(all_predictors()) %>%
    # factor variables
    step_mutate_at(JobLevel, StockOptionLevel, fn = factor) %>%
    prep()
 
recipe_obj  

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)
 

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/04_modeling/h2o_models/StackedEnsemble_BestOfFamily_1_AutoML_3_20220715_102232")

automl_leader

# * FIX 2: OPTIONAL (rerunning h2o.automl() ) ----

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

# 3. Expected Value ----

# 3.1 Calculating Expected Value With OT (Baseline) ----

source(here::here('R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/assess_attrition.R'))

predictions_with_OT_tbl <- automl_leader %>% 
    h2o.predict(newdata = as.h2o(test_tbl)) %>% 
    as_tibble() %>% 
    bind_cols(
        test_tbl %>% 
            select(EmployeeNumber, MonthlyIncome, OverTime)
    )

    

ev_with_OT_tbl <- predictions_with_OT_tbl %>% 
    mutate( 
        # attrition cost 
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12, 
            net_revenue_per_employee = 250000)
        ) %>% 
    
    mutate(
        cost_of_policy_change =  0  # baseline case
    ) %>% 
    
    mutate(
        # expected value
        expected_attrition_cost =  
            Yes * (attrition_cost + cost_of_policy_change) +
            No  * (cost_of_policy_change)
    ) 

## the cost of attrition (EV) with overtime for employee # 5 was $59,586.    

# total EV with OT
total_ev_with_OT_tbl <- ev_with_OT_tbl %>% 
    summarise(total_expected_attrition_cost_0 = sum(expected_attrition_cost))

 
# 3.2 Calculating Expected Value With Targeted OT ----

# remove OT
test_without_OT_tbl <- test_tbl %>% 
    mutate(OverTime = fct_recode(OverTime, 'No' = 'Yes'))

# predictions
preditions_without_OT_tbl <- automl_leader %>% 
    ## with OT (employee #5 prob = 0.685353505)
    #h2o.predict(newdata = as.h2o(test_tbl))

    ## without OT (employee #5 prob = 0.317736051)
    h2o.predict(newdata = as.h2o(test_without_OT_tbl)) %>% 
   
     as_tibble() %>% 
    bind_cols(
        test_tbl %>% 
            select(EmployeeNumber, MonthlyIncome, OverTime),
        
        test_without_OT_tbl %>% 
            select(OverTime)
        ) %>% 
    
    rename(
        OverTime_0 = `OverTime...6`,  # initial state
        OverTime_1 = `OverTime...7`   # new state
    )


avg_overtime_pct <- 0.10
    
ev_without_EV_tbl <- preditions_without_OT_tbl %>% 
    # attrition cost 
    mutate( 
        # attrition cost 
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12, 
            net_revenue_per_employee = 250000)
    ) %>% 
    
    mutate(
        cost_of_policy_change =  case_when(
            OverTime_0 == 'Yes' & OverTime_1 == 'No' ~ avg_overtime_pct * attrition_cost,
            TRUE ~ 0
            )
    ) %>% 
    
    mutate(
        # expected value
        expected_attrition_cost =  
            Yes * (attrition_cost + cost_of_policy_change) +
            No  * (cost_of_policy_change)
    ) 

ev_with_OT_tbl
## the cost of attrition (EV) with overtime for employee # 5 was $59,586.  

ev_without_EV_tbl
## now, the prediction of EV without overtime or employee # 5 is $36,319


total_ev_without_OT_tbl <- ev_without_EV_tbl %>% 
    summarise(total_expected_attrition_cost_1 = sum(expected_attrition_cost))



# 3.3 Savings Calculation ----

