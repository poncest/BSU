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
    # * FIX 1 ----
    # step_num2factor(JobLevel, StockOptionLevel) %>%
    step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>%
    # * END FIX 1 ----
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_0_AutoML_20180503_035824")

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

# * END FIX 2 ----


# 3. Primer: Working With Threshold & Rates ----

performance_h2o <- automl_leader %>%
    h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>%
    h2o.confusionMatrix()

rates_by_threshold_tbl <- performance_h2o %>%
    h2o.metric() %>%
    as_tibble()

rates_by_threshold_tbl %>% glimpse()

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr)

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    gather(key = "key", value = "value", tnr:tpr, factor_key = TRUE) %>%
    mutate(key = fct_reorder2(key, threshold, value)) %>%
    ggplot(aes(threshold, value, color = key)) +
    geom_point() +
    geom_smooth() +
    theme_tq() +
    scale_color_tq() +
    theme(legend.position = "right") +
    labs(
        title = "Expected Rates",
        y = "Value", x = "Threshold"
    )


# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

source("00_Scripts/assess_attrition.R")

predictions_with_OT_tbl <- automl_leader %>%
    h2o.predict(newdata = as.h2o(test_tbl)) %>%
    as.tibble() %>%
    bind_cols(
        test_tbl %>%
            select(EmployeeNumber, MonthlyIncome, OverTime)
    )

predictions_with_OT_tbl


ev_with_OT_tbl <- predictions_with_OT_tbl %>%
    mutate(
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12,
            net_revenue_per_employee = 250000
        )
    ) %>%
    mutate(
        cost_of_policy_change = 0 
    ) %>%
    mutate(
        expected_attrition_cost = 
            Yes * (attrition_cost + cost_of_policy_change) +
            No *  (cost_of_policy_change)
    )

ev_with_OT_tbl

total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
    summarise(
        total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )

total_ev_with_OT_tbl



# 4.2 Calculating Expected Value With Targeted OT ----


max_f1_tbl <- rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1)) %>%
    slice(1)

max_f1_tbl

tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr

threshold <- max_f1_tbl$threshold

test_targeted_OT_tbl <- test_tbl %>%
    add_column(Yes = predictions_with_OT_tbl$Yes) %>%
    mutate(
        OverTime = case_when(
            Yes >= threshold ~ factor("No", levels = levels(test_tbl$OverTime)),
            TRUE ~ OverTime
        )
    ) %>%
    select(-Yes)

test_targeted_OT_tbl

predictions_targeted_OT_tbl <- automl_leader %>%
    h2o.predict(newdata = as.h2o(test_targeted_OT_tbl)) %>%
    as.tibble() %>%
    bind_cols(
        test_tbl %>%
            select(EmployeeNumber, MonthlyIncome, OverTime),
        test_targeted_OT_tbl %>%
            select(OverTime)
    ) %>%
    # * FIX 3 ----
    rename(
        OverTime_0 = `OverTime...6`,
        OverTime_1 = `OverTime...7`
    )
    # * END FIX 3 ----

predictions_targeted_OT_tbl


avg_overtime_pct <- 0.10

ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>%
    mutate(
        attrition_cost = calculate_attrition_cost(
            n = 1,
            salary = MonthlyIncome * 12,
            net_revenue_per_employee = 250000
        )
    ) %>%
    mutate(
        cost_of_policy_change = case_when(
            OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
            TRUE ~ 0
        )
    ) %>%
    mutate(
        cb_tn = cost_of_policy_change,
        cb_fp = cost_of_policy_change,
        cb_tp = cost_of_policy_change + attrition_cost,
        cb_fn = cost_of_policy_change + attrition_cost,
        expected_attrition_cost = 
            Yes * (tpr*cb_tp + fnr*cb_fn) +
            No *  (tnr*cb_tn + fpr*cb_fp)
    ) 

ev_targeted_OT_tbl

total_ev_targeted_OT_tbl <- ev_targeted_OT_tbl %>%
    summarize(
        total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )

total_ev_targeted_OT_tbl 

# 4.3 Savings Calculation ----

savings_tbl <- bind_cols(
    total_ev_with_OT_tbl,
    total_ev_targeted_OT_tbl
) %>%
    mutate(
        savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
        pct_savings = savings / total_expected_attrition_cost_0
    )

savings_tbl



# 5. Optimizing By Threshold ----

# 5.1 Create calculate_savings_by_threshold() ----


data <- test_tbl
h2o_model <- automl_leader


calculate_savings_by_threshold <- function(data, h2o_model, threshold = 0,
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1) {
    
    
    data_0_tbl <- as.tibble(data)
    
    # 4. Expected Value 
    
    # 4.1 Calculating Expected Value With OT 
    
    pred_0_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
        as.tibble() %>%
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
                No *  (cost_of_policy_change)
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
                Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
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
        # * END FIX 3 ----
    
    
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
            savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
            pct_savings = savings / total_expected_attrition_cost_0
        )
    
    return(savings_tbl$savings)
    
}

# No OT Policy

test_tbl %>%
    calculate_savings_by_threshold(automl_leader, threshold = 0,
                                   tnr = 0, fnr = 0, tpr = 1, fpr = 1)

# Do Nothing Policy

test_tbl %>%
    calculate_savings_by_threshold(automl_leader, threshold = 1,
                                   tnr = 1, fnr = 1, tpr = 0, fpr = 0)

# Threshold @ Max F1

rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1))

max_f1_savings <- calculate_savings_by_threshold(test_tbl, automl_leader,
                               threshold = max_f1_tbl$threshold,
                               tnr = max_f1_tbl$tnr,
                               fpr = max_f1_tbl$fpr,
                               fnr = max_f1_tbl$fnr,
                               tpr = max_f1_tbl$tpr)

max_f1_savings


# 5.2 Optimization ----

smpl <- seq(1, 220, length.out = 20) %>% round(digits = 0)

partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)

rates_by_threshold_optimized_tbl <- rates_by_threshold_tbl %>%
    select(threshold, tnr:tpr) %>%
    slice(smpl) %>%
    mutate(
        savings = pmap_dbl(
            .l = list(
                threshold = threshold,
                tnr = tnr,
                fnr = fnr,
                fpr = fpr,
                tpr = tpr
            ),
            .f = partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)
        )
    )

rates_by_threshold_optimized_tbl


rates_by_threshold_optimized_tbl %>%
    ggplot(aes(threshold, savings)) +
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]]) +
    
    # Optimal Point
    geom_point(shape = 21, size = 5, color = palette_light()[[3]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(savings == max(savings))) +
    geom_label(aes(label = scales::dollar(savings)), 
               vjust = -1, color = palette_light()[[3]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(savings == max(savings))) +
    
    # F1 Max
    geom_vline(xintercept = max_f1_tbl$threshold, 
               color = palette_light()[[5]], size = 2) +
    annotate(geom = "label", label = scales::dollar(max_f1_savings),
             x = max_f1_tbl$threshold, y = max_f1_savings, vjust = -1,
             color = palette_light()[[1]]) + 
    
    # No OT Policy
    geom_point(shape = 21, size = 5, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == min(threshold))) +
    geom_label(aes(label = scales::dollar(savings)), 
               vjust = -1, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == min(threshold))) +
    
    # Do Nothing Policy
    geom_point(shape = 21, size = 5, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == max(threshold))) +
    geom_label(aes(label = scales::dollar(round(savings, 0))), 
               vjust = -1, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl %>%
                   filter(threshold == max(threshold))) +
    
    # Aesthestics
    theme_tq() +
    expand_limits(x = c(-.1, 1.1), y = 8e5) +
    scale_x_continuous(labels = scales::percent, 
                       breaks = seq(0, 1, by = 0.2)) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Optimization Results: Expected Savings Maximized At 14.9%",
        x = "Threshold (%)", y = "Savings"
    )




# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----

data <- test_tbl

h2o_model <- automl_leader

calculate_savings_by_threshold_2 <- function(data, h2o_model, threshold = 0,
                                             tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                             avg_overtime_pct = 0.10,
                                             net_revenue_per_employee = 250000) {
    
    data_0_tbl <- as.tibble(data)
    
    
    # 4. Expected Value 
    
    # 4.1 Calculating Expected Value With OT 
    
    pred_0_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
        as.tibble() %>%
        bind_cols(
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime)
        )
    
    ev_0_tbl <- pred_0_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                # Changed in _2 ----
                net_revenue_per_employee = net_revenue_per_employee) 
        ) %>%
        mutate(
            cost_of_policy_change = 0
        ) %>%
        mutate(
            expected_attrition_cost = 
                Yes * (attrition_cost + cost_of_policy_change) +
                No *  (cost_of_policy_change)
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
                Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
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
        # *END FIX 3 ----
    
    
    avg_overtime_pct <- avg_overtime_pct # Changed in _2 ----
    
    ev_1_tbl <- pred_1_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                # Changed in _2 ----
                net_revenue_per_employee = net_revenue_per_employee)
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
            savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
            pct_savings = savings / total_expected_attrition_cost_0
        )
    
    return(savings_tbl$savings)
    
}

test_tbl %>%
    calculate_savings_by_threshold_2(automl_leader, 
                                     avg_overtime_pct = 0.10, 
                                     net_revenue_per_employee = 250000)

# 6.2 Sensitivity Analysis ----

max_savings_rates_tbl <- rates_by_threshold_optimized_tbl %>%
    filter(savings == max(savings))

max_savings_rates_tbl

calculate_savings_by_threshold_2(
    data = test_tbl,
    h2o_model = automl_leader,
    threshold = max_savings_rates_tbl$threshold,
    tnr = max_savings_rates_tbl$tnr,
    fnr = max_savings_rates_tbl$fnr,
    fpr = max_savings_rates_tbl$fpr,
    tpr = max_savings_rates_tbl$tpr
)

calculate_savings_by_threshold_2_preloaded <- partial(
    calculate_savings_by_threshold_2,
    # Function Arguments
    data = test_tbl,
    h2o_model = automl_leader,
    threshold = max_savings_rates_tbl$threshold,
    tnr = max_savings_rates_tbl$tnr,
    fnr = max_savings_rates_tbl$fnr,
    fpr = max_savings_rates_tbl$fpr,
    tpr = max_savings_rates_tbl$tpr
)

calculate_savings_by_threshold_2_preloaded(
    avg_overtime_pct = 0.10, 
    net_revenue_per_employee = 250000)

sensitivity_tbl <- list(
    avg_overtime_pct = seq(0.05, 0.30, by = 0.05),
    net_revenue_per_employee = seq(200000, 400000, by = 50000)
) %>%
    cross_df() %>%
    mutate(
        savings = pmap_dbl(
            .l = list(
                avg_overtime_pct = avg_overtime_pct,
                net_revenue_per_employee = net_revenue_per_employee
            ),
            .f = calculate_savings_by_threshold_2_preloaded
        )
    )

sensitivity_tbl

sensitivity_tbl %>%
    ggplot(aes(avg_overtime_pct, net_revenue_per_employee)) +
    geom_tile(aes(fill = savings)) +
    geom_label(aes(label = savings %>% round(0) %>% scales::dollar())) +
    theme_tq() +
    theme(legend.position = "none") +
    scale_fill_gradient2(
        low = palette_light()[[2]],
        mid = "white",
        high = palette_light()[[1]],
        midpoint = 0
    ) +
    scale_x_continuous(
        labels = scales::percent,
        breaks = seq(0.05, 0.30, by = 0.05)
    ) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Profitability Heatmap: Expected Savings Sensitivity Analysis",
        subtitle = "How sensitive is savings to net revenue per emploee and average overtime percentage?",
        x = "Average Overtime Percentage",
        y = "Net Revenue Per Employee"
    )




# Challenge: People With No Stock Options Are Leaving ----

# Part 1: Find optimal threshold ----

avg_overtime_pct   <- 0.10 
net_revenue_per_employee <- 250000
stock_option_cost <- 5000




# Part 1: Solution ----

data <- test_tbl
h2o_model <- automl_leader

threshold <- 0
tnr <- 0
fpr <- 1
fnr <- 0
tpr <- 1

avg_overtime_pct <- 0.10
net_revenue_per_employee <- 250000
stock_option_cost <- 5000

calculate_savings_by_threshold_3 <- function(data, h2o_model, threshold = 0,
                                             tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                             avg_overtime_pct = 0.10,
                                             net_revenue_per_employee = 250000,
                                             stock_option_cost = 5000) {
    
    data_0_tbl <- as.tibble(data)
    
    
    # 4. Expected Value 
    
    # 4.1 Calculating Expected Value With OT 
    
    pred_0_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
        as_tibble() %>%
        bind_cols(
            # Changed in _3 ----
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime, StockOptionLevel)
        )
    
    ev_0_tbl <- pred_0_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                # Changed in _2 ----
                net_revenue_per_employee = net_revenue_per_employee) 
        ) %>%
        mutate(
            cost_of_policy_change = 0
        ) %>%
        mutate(
            expected_attrition_cost = 
                Yes * (attrition_cost + cost_of_policy_change) +
                No *  (cost_of_policy_change)
        )
    
    
    total_ev_0_tbl <- ev_0_tbl %>%
        summarise(
            total_expected_attrition_cost_0 = sum(expected_attrition_cost)
        )
    
    # 4.2 Calculating Expected Value With Targeted OT & Stock Option Policy
    
    data_1_tbl <- data_0_tbl %>%
        add_column(Yes = pred_0_tbl$Yes) %>%
        mutate(
            OverTime = case_when(
                Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
                TRUE ~ OverTime
            )
        ) %>%
        # Changed in _3 ----
    mutate(
        StockOptionLevel = case_when(
            Yes >= threshold & StockOptionLevel == 0 
            ~ factor("1", levels = levels(data_0_tbl$StockOptionLevel)),
            TRUE ~ StockOptionLevel
        )
    ) %>%
        select(-Yes) 
    
    pred_1_tbl <- h2o_model %>%
        h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
        as.tibble() %>%
        # Changed in _3 ----
        bind_cols(
            data_0_tbl %>%
                select(EmployeeNumber, MonthlyIncome, OverTime, StockOptionLevel),
            data_1_tbl %>%
                select(OverTime, StockOptionLevel)
        ) %>%
        # * FIX 3 ----
        rename(
            OverTime_0 = OverTime...6,
            OverTime_1 = OverTime...8,
            # Changed in _3 ----
            StockOptionLevel_0 = StockOptionLevel...7,
            StockOptionLevel_1 = StockOptionLevel...9
        )
        # * END FIX 3 ----
    
    
    avg_overtime_pct <- avg_overtime_pct # Changed in _2 ----
    stock_option_cost <- stock_option_cost # Changed in _3 ----
    
    ev_1_tbl <- pred_1_tbl %>%
        mutate(
            attrition_cost = calculate_attrition_cost(
                n = 1,
                salary = MonthlyIncome * 12,
                # Changed in _2 ----
                net_revenue_per_employee = net_revenue_per_employee)
        ) %>%
        # Changed in _3 ----
        # cost_OT
        mutate(
            cost_OT = case_when(
                OverTime_1 == "No" & OverTime_0 == "Yes" 
                ~ avg_overtime_pct * MonthlyIncome * 12,
                TRUE ~ 0
            )
        ) %>%
        # cost Stock Options
        mutate(
            cost_SO = case_when(
                StockOptionLevel_1 == "1" & StockOptionLevel_0 == "0"
                ~ stock_option_cost,
                TRUE ~ 0
            )
        ) %>%
        mutate(cost_of_policy_change = cost_OT + cost_SO) %>%
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
            savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
            pct_savings = savings / total_expected_attrition_cost_0
        )
    
    return(savings_tbl$savings)
    
}

max_f1_tbl <- rates_by_threshold_tbl %>%
    select(threshold, f1, tnr:tpr) %>%
    filter(f1 == max(f1))

max_f1_savings <- calculate_savings_by_threshold_3(
    test_tbl, automl_leader,
    threshold = max_f1_tbl$threshold,
    tnr = max_f1_tbl$tnr,
    fpr = max_f1_tbl$fpr,
    fnr = max_f1_tbl$fnr,
    tpr = max_f1_tbl$tpr,
    avg_overtime_pct = 0.10,
    net_revenue_per_employee = 250000,
    stock_option_cost = 5000
)

# Optimization

smpl <- seq(1, 220, length.out = 20) %>% round(digits = 0)

calculate_savings_by_threshold_3_preloded <- 
    partial(calculate_savings_by_threshold_3, 
            data = test_tbl, 
            h2o_model = automl_leader,
            avg_overtime_pct = 0.10,
            net_revenue_per_employee = 250000,
            stock_option_cost = 5000)

rates_by_threshold_optimized_tbl_3 <- rates_by_threshold_tbl %>%
    select(threshold, tnr:tpr) %>%
    slice(smpl) %>%
    mutate(
        savings = pmap_dbl(
            .l = list(
                threshold = threshold,
                tnr = tnr,
                fnr = fnr,
                fpr = fpr,
                tpr = tpr
            ),
            .f = calculate_savings_by_threshold_3_preloded
        )
    )

rates_by_threshold_optimized_tbl_3

rates_by_threshold_optimized_tbl_3 %>%
    filter(savings == max(savings))


rates_by_threshold_optimized_tbl_3 %>%
    ggplot(aes(threshold, savings)) +
    
    # Vlines
    geom_vline(xintercept = max_f1_tbl$threshold, 
               color = palette_light()[[5]], size = 2) +
    geom_vline(aes(xintercept = threshold), 
               color = palette_light()[[3]], size = 2,
               data = rates_by_threshold_optimized_tbl_3 %>%
                   filter(savings == max(savings))
    ) +
    
    # Points
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]]) +
    
    # F1 Max
    annotate(geom = "label", label = scales::dollar(max_f1_savings),
             x = max_f1_tbl$threshold, y = max_f1_savings, vjust = -1,
             color = palette_light()[[1]]) + 
    
    # Optimal Point
    geom_point(shape = 21, size = 5, color = palette_light()[[3]],
               data = rates_by_threshold_optimized_tbl_3 %>%
                   filter(savings == max(savings))) +
    geom_label(aes(label = scales::dollar(savings)), 
               vjust = -2, color = palette_light()[[3]],
               data = rates_by_threshold_optimized_tbl_3 %>%
                   filter(savings == max(savings))) +
    
    # No OT Policy
    geom_point(shape = 21, size = 5, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl_3 %>%
                   filter(threshold == min(threshold))) +
    geom_label(aes(label = scales::dollar(savings)), 
               vjust = -1, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl_3 %>%
                   filter(threshold == min(threshold))) +
    
    # Do Nothing Policy
    geom_point(shape = 21, size = 5, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl_3 %>%
                   filter(threshold == max(threshold))) +
    geom_label(aes(label = scales::dollar(round(savings, 0))), 
               vjust = -1, color = palette_light()[[2]],
               data = rates_by_threshold_optimized_tbl_3 %>%
                   filter(threshold == max(threshold))) +
    
    # Aesthestics
    theme_tq() +
    expand_limits(x = c(-.1, 1.1), y = 12e5) +
    scale_x_continuous(labels = scales::percent, 
                       breaks = seq(0, 1, by = 0.2)) +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Optimization Results: Expected Savings Maximized At 18.9%",
        x = "Threshold (%)", y = "Savings"
    )



# Part 2: Perform sensitivity analysis at optimal threshold ----

net_revenue_per_employee <- 250000
avg_overtime_pct   <- seq(0.05, 0.30, by = 0.05)
stock_option_cost <- seq(5000, 25000, by = 5000)


# Part 2: Solution -----

max_savings_rates_tbl_3 <- rates_by_threshold_optimized_tbl_3 %>%
    filter(savings == max(savings))

calculate_savings_by_threshold_3_preloaded <- partial(
    calculate_savings_by_threshold_3,
    # Function Arguments
    data = test_tbl,
    h2o_model = automl_leader,
    threshold = max_savings_rates_tbl_3$threshold,
    tnr = max_savings_rates_tbl_3$tnr,
    fnr = max_savings_rates_tbl_3$fnr,
    fpr = max_savings_rates_tbl_3$fpr,
    tpr = max_savings_rates_tbl_3$tpr
)

calculate_savings_by_threshold_3_preloaded(
    avg_overtime_pct = 0.10, 
    net_revenue_per_employee = 250000,
    stock_option_cost = 5000
)

sensitivity_tbl_3 <- list(
    avg_overtime_pct = seq(0.05, 0.30, by = 0.05),
    net_revenue_per_employee = 250000,
    stock_option_cost = seq(5000, 25000, by = 5000)
) %>%
    cross_df() %>%
    mutate(
        savings = pmap_dbl(
            .l = list(
                avg_overtime_pct = avg_overtime_pct,
                net_revenue_per_employee = net_revenue_per_employee,
                stock_option_cost = stock_option_cost
            ),
            .f = calculate_savings_by_threshold_3_preloaded
        )
    )

sensitivity_tbl_3



sensitivity_tbl_3 %>%
    ggplot(aes(avg_overtime_pct, stock_option_cost)) +
    geom_tile(aes(fill = savings)) +
    geom_label(aes(label = savings %>% round(0) %>% scales::dollar())) +
    theme_tq() +
    theme(legend.position = "none") +
    scale_fill_gradient2(
        low = palette_light()[[2]],
        mid = "white",
        high = palette_light()[[1]],
        midpoint = 0
    ) +
    scale_x_continuous(
        labels = scales::percent,
        breaks = seq(0.05, 0.30, by = 0.05)
    ) +
    scale_y_continuous(
        labels = scales::dollar,
        breaks = seq(5000, 25000, by = 5000)
    ) +
    labs(
        title = "Profitability Heatmap: Expected Savings Sensitivity Analysis",
        subtitle = "How sensitive is savings to stock option cost and average overtime percentage?",
        x = "Average Overtime Percentage",
        y = "Average Stock Option Cost"
    )




