# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

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



# 3. Primer: Working With Threshold & Rates ----
performance_h2o <- automl_leader %>% 
    h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>% 
    h2o.confusionMatrix()


rates_by_treshold_tbl <- performance_h2o %>% 
    h2o.metric() %>% 
    as_tibble() 

rates_by_treshold_tbl %>% glimpse()  # focus  from tns to tpr

# tns - tps = confusion matrix
# tnr - tpr = confusion matrix values converted to probabilities

# tnr & fpr are related. They total to 1 (100%)
# tpr & fnr also related.


rates_by_treshold_tbl %>%
    select(threshold, f1, tnr:tpr)

rates_by_treshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>% 
    filter(f1 == max(f1)) %>% 
    slice(1)


rates_by_treshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>% 
    pivot_longer(
        cols         = tnr:tpr,
        names_to     = "key",
        values_to    = "value",
        names_ptypes = list(key = factor())
    ) %>% 
   # gather(key = key, value = value, tnr:tpr, factor_key = TRUE) %>% 
    mutate(key = fct_reorder2(key, threshold, value)) %>% 
    
    ggplot(aes(threshold, value, color = key)) +
    geom_point() +
    geom_smooth() +
    scale_color_tq() +
    labs(
        x = "Threshold", y = "Value",
        title = "Expected Rates",
    ) +
    theme_tq() + 
    theme(legend.position = 'right')

    

# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

source(here::here("R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/assess_attrition.R"))


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

## the cost of attrition (EV) with overtime for employee # 5 was $65,605.     

# total EV with OT
total_ev_with_OT_tbl <- ev_with_OT_tbl %>% 
    summarise(
        total_expected_attrition_cost_0 = sum(expected_attrition_cost)
        )


# 4.2 Calculating Expected Value With Targeted OT ----

max_f1_tbl <- rates_by_treshold_tbl |> 
    select(threshold, f1, tnr:tpr) |> 
    filter(f1 == max(f1)) |> 
    slice(1)


tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr 

threshold <- max_f1_tbl$threshold


## What is happening?
## Anyone with Yes >= 0.31 (threshold) and OT = Yes, had their OT toggled to NO
test_targeted_OT_tbl <- test_tbl |> 
    add_column(Yes = predictions_with_OT_tbl$Yes) |> 
    mutate(
        OverTime = case_when(
            Yes >= threshold ~ factor('No', levels = levels(test_tbl$OverTime)),
            TRUE ~ OverTime
        )
    ) |> 
    select(-Yes)

## Making some predictions
## running h2o.predict() on the modified data will give us the class probability of
## attrition = Yes implementing the policy

# BEFORE 
automl_leader |> 
    h2o.predict(newdata = as.h2o(test_tbl))                 # yes = 0.69

# AFTER - OT policy
automl_leader |> 
    h2o.predict(newdata = as.h2o(test_targeted_OT_tbl))     # yes = 0.32

# employee 5 when from 0.69 down to 0.32 percent after implementing the policy change

predictions_targeted_OT_tbl <- automl_leader |> 
    h2o.predict(newdata = as.h2o(test_targeted_OT_tbl)) |> 
    as_tibble() |> 
    bind_cols(
        test_tbl |> 
            select(EmployeeNumber, MonthlyIncome, OverTime),
        test_targeted_OT_tbl |> 
            select(OverTime)
    ) |> 
    rename(
        OverTime_0 = OverTime...6,
        OverTime_1 = OverTime...7
    )  


avg_overtime_pct <- 0.10

ev_targted_OT_tbl <- predictions_targeted_OT_tbl |> 
    
    # attrition
    mutate( 
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12, 
            net_revenue_per_employee = 250000)
    ) |> 
    
    # cost of policy change
    mutate(
    cost_of_policy_change =  case_when(
        OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
        TRUE ~ 0
        )
    ) %>% 
    
    # cost benefits (cb)   
    mutate(
        # cost of an employee staying
        cb_tn = cost_of_policy_change,
        
        # cost of predicting leave when the employee stays
        cb_fp = cost_of_policy_change,
        
        # cost of an employee leaving
        cb_tp = cost_of_policy_change + attrition_cost,
        
        # cost of predicting leaving when we predict to stays
        cb_fn = cost_of_policy_change + attrition_cost,
        
        expected_attrition_cost = 
            Yes * (tpr*cb_tp + fnr*cb_fn) +
            No  * (tnr*cb_tn + fpr*cb_fp)
    ) 

# employee 5, is targeted by the policy change.


total_ev_targeted_OT_tbl <- ev_targted_OT_tbl |> 
    summarise(
        total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )



# 4.3 Savings Calculation ----  

# BEFORE policy change - $3,136,943
total_ev_with_OT_tbl

# AFTER policy change - $2,802,246 (10.7%)
total_ev_targeted_OT_tbl

# total savings - $334,697
total_ev_with_OT_tbl - total_ev_targeted_OT_tbl


savings_tbl <- bind_cols(
    total_ev_with_OT_tbl,
    total_ev_targeted_OT_tbl
) |> 
    mutate(
        savings     = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
        pct_savings = savings / total_expected_attrition_cost_0
    )


# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----

# Inputs
data <- test_tbl
h2o_model <- automl_leader
threshold = 0
tnr = 0
fpr = 1
fnr = 0
tpr = 1


# function
calculate_savings_by_threshold <- function(data, h2o_model, threshold = 0,
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1) {
    
    
    data_0_tbl <- as_tibble(data)
    
    # 4. Expected Value 
    
    # 4.1 Calculating Expected Value With OT 
    
    pred_0_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
        as_tibble() %>%
        bind_cols(
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime)
        )
    
    ev_0_tbl <- pred_0_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                net_revenue_per_employee = 250000)
        ) %>%
        mutate(
            cost_of_policy_change = 0
        ) %>%
        mutate(
            expected_attrition_cost = 
                Yes * (attrition_cost + cost_of_policy_change) +
                No  *  (cost_of_policy_change)
        )
    
    
    total_ev_0_tbl <- ev_0_tbl %>%
        summarise(
            total_expected_attrition_cost_0 = sum(expected_attrition_cost)
        )
    
    # 4.2 Calculating Expected Value With Targeted OT
    
    data_1_tbl <- data_0_tbl %>%
        add_column(Yes = pred_0_tbl$Yes) %>%
        mutate(
            OverTime = case_when(
                Yes  >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
                TRUE ~ OverTime
            )
        ) %>%
        select(-Yes) 
    
    pred_1_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
        as_tibble() %>%
        bind_cols(
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime),
            data_1_tbl %>%
                select(OverTime)
        ) %>%
        # * FIX 3 ----
    rename(
        OverTime_0 = OverTime...6,
        OverTime_1 = OverTime...7
    )
    
    
    avg_overtime_pct <- 0.10
    
    ev_1_tbl <- pred_1_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                net_revenue_per_employee = 250000)
        ) %>%
        mutate(
            cost_of_policy_change = case_when(
                OverTime_1 == "No" & OverTime_0 == "Yes" 
                ~ attrition_cost * avg_overtime_pct,
                TRUE ~ 0
            ))%>%
        mutate(
            cb_tn = cost_of_policy_change,
            cb_fp = cost_of_policy_change,
            cb_fn = attrition_cost + cost_of_policy_change,
            cb_tp = attrition_cost + cost_of_policy_change,
            expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
                No * (tnr*cb_tn + fpr*cb_fp)
        )
    
    
    total_ev_1_tbl <- ev_1_tbl %>%
        summarise(
            total_expected_attrition_cost_1 = sum(expected_attrition_cost)
        )
    
    
    # 4.3 Savings Calculation
    
    savings_tbl <- bind_cols(
        total_ev_0_tbl,
        total_ev_1_tbl
    ) %>%
        mutate(
            savings     = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
            pct_savings = savings / total_expected_attrition_cost_0
        )
    
    return(savings_tbl$savings)
    
}

# threshold @ max F1
rates_by_treshold_tbl %>% 
    select(threshold, f1, tnr:tpr) %>% 
    filter(f1 == max(f1))


# BEFORE optimization - savings $359,198
calculate_savings_by_threshold(data = test_tbl, h2o_model = automl_leader, 
                               threshold = max_f1_tbl$threshold, 
                               tnr = max_f1_tbl$tnr,
                               fnr = max_f1_tbl$fnr,
                               tpr = max_f1_tbl$tpr,
                               fpr = max_f1_tbl$fpr
                               )


# No OT Policy - savings $388,211
test_tbl %>% 
    calculate_savings_by_threshold(h2o_model = automl_leader,   
                                   threshold = 0,
                                   tnr = 0,
                                   fnr = 0,
                                   tpr = 1,
                                   fpr = 1
                                   )

# Do Nothing Policy - savings $0
test_tbl %>% 
    calculate_savings_by_threshold(h2o_model = automl_leader,
                                   threshold = 1,
                                   tnr = 1,
                                   fnr = 1,
                                   tpr = 0,
                                   fpr = 0
    )

# 5.2 Optimization ----
# threshold optimization with purr


# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

# 6.2 Sensitivity Analysis ----
