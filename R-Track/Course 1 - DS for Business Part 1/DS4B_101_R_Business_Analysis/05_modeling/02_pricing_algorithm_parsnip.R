# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# REGRESSION MODELS ----

# GOAL: BUILD PREDICTION MODEL FOR PRICING ALGORITHM

 
# LIBRARIES & DATA ----
model_04_tree_deci
pkgs <- c("parsnip", "glmnet", "rpart", "rpart.plot", "ranger", "randomForest", "xgboost", "kernlab")
# If any of these packages are not installed, run this: install.packages(pkgs)

# Standard
library(readxl)
library(tidyverse)
library(tidyquant)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)

# Source Scripts
source('R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_scripts/separete_bikes_and_outlier_detection.R')

# Read Data
bike_orderlines_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PROBLEM DEFINITION ----
# - Which Bike Categories are in high demand?
# - Which Bike Categories are under represented?
# - GOAL: Use a pricing algorithm to determine a new product price in a category gap

model_sales_tbl <- bike_orderlines_tbl %>%
    select(total_price, model, category_2, frame_material) %>%
    
    group_by(model, category_2, frame_material) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    arrange(desc(total_sales))

model_sales_tbl %>% 
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(total_sales, .fun = max) %>% 
               fct_rev()) %>%
    
    ggplot(aes(frame_material, total_sales)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    #coord_flip() +
    facet_wrap(~ category_2) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
    theme_tq() +
    labs(
        title = "Total Sales for Each Model",
        x = "Frame Material", y = "Revenue"
    )
 

# 2.0 TRAINING & TEST SETS ----

bike_features_tbl <- bike_orderlines_tbl %>%
    
    select(price, model, category_2, frame_material) %>%
    
    # unique instances
    distinct() %>%
    
    mutate(id = row_number()) %>%
    
    # rearrange
    select(id, everything()) %>%
    
    separate_bike_model(keep_model_column = TRUE, append = TRUE)

bike_features_tbl


# splitting the data training 80%/ test 20%
# Note: run set.seed() and `split_obj` together
set.seed(seed = 1113)
split_obj <- rsample::initial_split(data = bike_features_tbl, 
                                    prop = 0.80, 
                                    # make sure all 18 levels are included in the training set
                                    strata = "model_base")

# there are 18 levels
bike_features_tbl %>% distinct(model_base)

# training - we have all levels in the training set
split_obj %>% training() %>% distinct(model_base)

# testing
split_obj %>% testing() %>% distinct(model_base)


train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)  

# *** FIX 1 *** ----
# Error: factor model_base has new levels Fat CAAD2
# - Need to move Fat CAAD2 from test to training set because model doesn't know how to handle
#   a new category that is unseen in the training data

train_tbl <- train_tbl %>%
    bind_rows(
        test_tbl %>% filter(model_base %>% str_detect("Fat CAAD2"))
    )

test_tbl <- test_tbl %>%
    filter(!model_base %>% str_detect("Fat CAAD2"))

# *** END FIX 1 *** ---- 

# 3.0 LINEAR METHODS ---- 
?linear_reg  
?set_engine  
?fit  
?predict.model_fit  
?metrics  


# 3.1 LINEAR REGRESSION - NO ENGINEERED FEATURES ----

# 3.1.1 Model ----
?lm 

# simple linear regression model
model_01_linear_lm_simple <- linear_reg(mode = "regression") %>%
    # engine
    set_engine("lm") %>% 
    
    # fit(target ~ as a function of cols)
    fit(price ~ category_2 + frame_material, data = train_tbl)

    # manual calculations
    # residuals
    # mutate(residuals = price - .pred) %>%
    # 
    # summarize(
          # mean abs error
    #     mae = abs(residuals) %>% mean(),
          # root mean sqrt error
    #     rmse = mean(residuals^2)^0.5
    # )

# the same calcs but using yardstick
model_01_linear_lm_simple %>% 
    # prediction, data Testing Dataset
    predict(new_data = test_tbl) %>%
    
    # 
    bind_cols(test_tbl %>% select(price)) %>%
    yardstick::metrics(truth = price, estimate = .pred)

# 3.1.2 Feature Importance ---- 
model_01_linear_lm_simple 

model_01_linear_lm_simple$fit %>% class()

model_01_linear_lm_simple$fit %>%
    broom::tidy() %>%
    arrange(p.value) %>%
    mutate(term = as_factor(term) %>% fct_rev()) %>%
    
    # visualization
    ggplot(aes(x = estimate, y = term)) +
    geom_point() + 
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 01: Simple lm Model") +
    theme_tq()

# 3.1.3 Function to Calculate Metrics ----

# computes metrics (rmse, rsq, mae)
model_01_linear_lm_simple %>% 
    predict(new_data = test_tbl) %>%
    
    bind_cols(test_tbl %>% select(price)) %>%
    yardstick::metrics(truth = price, estimate = .pred)

# use a function to compute metrics
calc_metrics <- function(model, new_data = test_tbl) {
    
    model %>%
        predict(new_data = new_data) %>%
        
        bind_cols(new_data %>% select(price)) %>%
        yardstick::metrics(truth = price, estimate = .pred)
    
}

# test function 
model_01_linear_lm_simple %>% calc_metrics(test_tbl)

calc_metrics(model_01_linear_lm_simple, test_tbl)


# 3.2 LINEAR REGRESSION - WITH ENGINEERED FEATURES ----

# 3.2.1 Model ----
train_tbl

model_02_linear_lm_complex <- linear_reg("regression") %>%
    set_engine("lm") %>% 
    
    # TARGET = price, ~. = as a function of everything
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))


# using calc_metrics function
# BEFORE - SIMPLE (mae = 1997)
model_01_linear_lm_simple %>% calc_metrics(new_data = test_tbl)

# AFTER - COMPLEX (mae = 904)
model_02_linear_lm_complex %>% calc_metrics(new_data = test_tbl)


# 3.2.2 Feature importance ---- 
model_02_linear_lm_complex$fit %>%
    broom::tidy() %>%
    arrange(p.value) %>% drop_na() %>% 
    mutate(term = as_factor(term) %>% fct_rev()) %>%
    
    ggplot(aes(x = estimate, y = term)) +
    geom_point() +
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 02: Complex lm Model") +
    theme_tq()

  
# 3.3 PENALIZED REGRESSION ---- 

# 3.3.1 Model ----
?linear_reg
?glmnet::glmnet

# BEFORE - GLMNET (mae = 898)
model_03_linear_glmnet <- linear_reg(mode = "regression", penalty = 10, mixture = 0.5) %>%
    set_engine("glmnet") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_03_linear_glmnet %>% calc_metrics(test_tbl)

# BEFORE - GLMNET (mae = 881)
model_03_linear_glmnet <- linear_reg(mode = "regression", penalty = 50, mixture = 0) %>%
    set_engine("glmnet") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_03_linear_glmnet %>% calc_metrics(test_tbl)


# 3.3.2 Feature Importance ---- 
model_03_linear_glmnet$fit %>%
    broom::tidy() %>%  
    
    # *** FIX 2 *** ----
    # Problem: glmnet returns all lambda (penalty) values assessed
    # Solution: Filter to the max dev.ratio, which is the "optimal" lambda according to glmnet
    filter(dev.ratio == max(dev.ratio)) %>%
    # *** END FIX 2 *** ----
    
    arrange(desc(abs(estimate))) %>%
    mutate(term = as_factor(term) %>% fct_rev()) %>%
    
    ggplot(aes(x = estimate, y = term)) +
    geom_point() +
    ggrepel::geom_label_repel(aes(label = scales::dollar(estimate, accuracy = 1)),
                              size = 3) +
    scale_x_continuous(labels = scales::dollar_format()) +
    labs(title = "Linear Regression: Feature Importance",
         subtitle = "Model 03: GLMNET Model") + 
    theme_tq()
 


# 4.0 TREE-BASED METHODS ----

# 4.1 DECISION TREES ----

# 4.1.1 Model ----
?decision_tree 
?rpart::rpart 

# BEFORE - (mae 1205)  
model_04_tree_decision_tree <- decision_tree(mode = "regression", 
              cost_complexity = 0.01, 
              tree_depth      = 5, 
              min_n           = 10) %>%
    set_engine("rpart") %>% 
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_04_tree_decision_tree %>% calc_metrics(test_tbl)


# AFTER - (mae 1463) - Worst
model_04_tree_decision_tree <- decision_tree(mode = "regression", 
                                             cost_complexity = 0.1, 
                                             tree_depth      = 5, 
                                             min_n           = 10) %>%
    set_engine("rpart") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_04_tree_decision_tree %>% calc_metrics(test_tbl)

# AFTER - (mae 1088) 
model_04_tree_decision_tree <- decision_tree(mode = "regression", 
                                             cost_complexity = 0.001, 
                                             tree_depth      = 7, 
                                             min_n           = 7) %>%
    set_engine("rpart") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_04_tree_decision_tree %>% calc_metrics(test_tbl)


# 4.1.2 Decision Tree Plot ----
?rpart.plot()

# base plot
model_04_tree_decision_tree$fit %>%
    rpart.plot(roundint = FALSE)

# modify plot
model_04_tree_decision_tree$fit %>%
    rpart.plot(
        roundint = FALSE,
        type = 1, 
        extra = 101,                              # add n and %
        fallen.leaves = FALSE,                    # change the angles
        cex = 0.8,                                # font size
        main = "Model 04: Decision Tree",         # title
        box.palette = "Grays"                     # color
        )

show.prp.palettes()


# 4.2 RANDOM FOREST ----

# 4.2.1 Model: ranger ----
?rand_forest()  
?ranger::ranger  

# BEFORE (mae 1335)
model_05_rand_forest_ranger <- rand_forest(mode = "regression") %>% 
    
    set_engine("ranger") %>%
    
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_05_rand_forest_ranger %>% calc_metrics(test_tbl)


# AFTER - tuning parameters (mae 1156)
model_05_rand_forest_ranger <- rand_forest(
    mode = "regression", 
    mtry = 8, 
    trees = 1000, 
    min_n = 10
    ) %>%
    
    set_engine("ranger", 
               replace = TRUE, 
               splitrule = "extratrees", 
               importance = "impurity") %>%
    
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_05_rand_forest_ranger %>% calc_metrics(test_tbl)


# AFTER - tuning parameters (mae 1149) about the same
set.seed(1234)
model_05_rand_forest_ranger <- rand_forest(
    mode = "regression", 
    mtry = 8, 
    trees = 1000, 
    min_n = 10
) %>%
    
    set_engine("ranger", 
               replace = TRUE, 
               splitrule = "extratrees", 
               importance = "impurity") %>%
    
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_05_rand_forest_ranger %>% calc_metrics(test_tbl)


# AFTER - tuning parameters (mae 1149) about the same
set.seed(1234)
model_05_rand_forest_ranger <- rand_forest(
    mode = "regression", 
    mtry = 8, 
    trees = 1000, 
    min_n = 10
) %>%
    
    # other parameters to tune
    set_engine("ranger", 
               replace = TRUE, 
               splitrule = "extratrees", 
               importance = "impurity") %>%
    
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_05_rand_forest_ranger %>% calc_metrics(test_tbl)


# 4.2.2 ranger: Feature Importance ---- 

model_05_rand_forest_ranger$fit %>% 
    ranger::importance() %>%
    enframe() %>%                        # list -> dataframe
    arrange(desc(value)) %>%
    mutate(name = as_factor(name) %>% fct_rev()) %>%
    
    ggplot(aes(value, name)) +
    geom_point() +
    labs(title = "ranger: Variable Importance",
         subtitle = "Model 05: Ranger Random Forest Model")+
    theme_tq()

# we know that the `black` feature is important but we don not know about how much 
# it is important ($) because RF is an ensemble method

 

# 4.2.3 Model randomForest ----
?rand_forest() 
?randomForest::randomForest

# *** FIX 3 *** ----
# - Encodings have changed for randomForest. Must use factors for character data

# - Error: New factor levels not present in the training data
set.seed(1234)
model_06_rand_forest_randomForest <- rand_forest("regression") %>%
    set_engine("randomForest") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_06_rand_forest_randomForest %>% calc_metrics(test_tbl)

## Solution: ##  (mae 1429)
set.seed(1234)
model_06_rand_forest_randomForest <- rand_forest("regression") %>%
    set_engine("randomForest") %>%
    fit(price ~ ., 
        data = train_tbl %>% 
            select(-id, -model, -model_tier) %>%
            mutate_if(is.character, factor) # <- This internally tells randomForest to convert characters to factors
        )

model_06_rand_forest_randomForest %>% calc_metrics(test_tbl)

# *** END FIX 3 *** ----


# 4.2.4 randomForest: Feature Importance ---- 

model_06_rand_forest_randomForest$fit %>%
    # returns the variable of importance
    randomForest::importance() %>%
    as_tibble(rownames = "name") %>%
    arrange(desc(IncNodePurity)) %>%
    mutate(name = as_factor(name) %>% fct_rev()) %>%
    
    ggplot(aes(IncNodePurity, name)) +
    geom_point() +
    labs(
        title = "randomForest: Variable Importance",
        subtitle = "Model 06: randomForest Model"
    ) 
    
 
# 4.3 XGBOOST ---- 
 
# 4.3.1 Model ----  
?boost_tree  
?xgboost::xgboost  

# BEFORE (mae 988)
set.seed(1234)
model_07_boost_tree_xgboost <- boost_tree(
    mode = "regression", 
) %>%
    set_engine("xgboost") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_07_boost_tree_xgboost %>% calc_metrics(test_tbl)

# AFTER (mae 927)
set.seed(1234)
model_07_boost_tree_xgboost <- boost_tree(
    mode = "regression", 
    mtry = 30,
    learn_rate = 0.25,
    tree_depth = 7
    ) %>%
    set_engine("xgboost") %>%
    fit(price ~ ., data = train_tbl %>% select(-id, -model, -model_tier))

model_07_boost_tree_xgboost %>% calc_metrics(test_tbl)


# 4.3.2 Feature Importance ----
?xgboost::xgb.importance


model_07_boost_tree_xgboost$fit %>%
    xgboost::xgb.importance(model = .) %>%
    as_tibble() %>%
    arrange(desc(Gain)) %>%
    mutate(Feature = as_factor(Feature) %>% fct_rev()) %>%
    
    ggplot(aes(Gain, Feature)) +
    geom_point() +
    labs(
        title    = "XGBoost: Variable Importance",
        subtitle = "Model 07: XGBoost Model"
    ) +
    theme_tq()


# 5.0 TESTING THE ALGORITHMS OUT ---- 

g1 <- bike_features_tbl %>%
    mutate(category_2 = as_factor(category_2) %>% 
               fct_reorder(price)) %>%
    
    ggplot(aes(category_2, y = price)) +
    geom_violin() +
    geom_jitter(width = 0.1, alpha = 0.5, color = "#2c3e50") +
    coord_flip() +
    facet_wrap(~ frame_material) +
    scale_y_continuous(labels = scales::dollar_format()) +
    theme_tq() +
    labs(
        title = "Unit Price for Each Model",
        y = "", x = "Category 2"
    )

# 5.1 NEW JEKYLL MODEL ----  

new_over_mountain_jekyll <- tibble(
    model = "Jekyll Al 1",
    frame_material = as.factor("Aluminum"),
    category_2 = as.factor("Over Mountain"),
    model_base = as.factor("Jekyll"),
    model_tier = as.factor("Aluminum 1"),
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 

new_over_mountain_jekyll 

# Linear Methods ----

predict(model_03_linear_glmnet, new_data = new_over_mountain_jekyll)
# price = 2433

# Tree-Based Methods ---- 

predict(model_07_boost_tree_xgboost, new_data = new_over_mountain_jekyll)
# price = 2315

# Iteration for all models
models_tbl <- tibble(
    model_id = str_c("Model 0", 1:7),
    model = list(
        model_01_linear_lm_simple,
        model_02_linear_lm_complex,
        model_03_linear_glmnet,
        model_04_tree_decision_tree,
        model_05_rand_forest_ranger,
        model_06_rand_forest_randomForest,
        model_07_boost_tree_xgboost
    )
)

models_tbl

# Add Predictions 

predictions_new_over_mountain_tbl <- models_tbl %>%
    mutate(predictions = map(model, predict, new_data = new_over_mountain_jekyll)) %>%
    unnest(predictions) %>%
    mutate(category_2 = "Over Mountain") %>%
    left_join(new_over_mountain_jekyll, by = "category_2")
    
predictions_new_over_mountain_tbl 

# Update plot

g2 <- g1 +
    geom_point(aes(y = .pred), color = "red", alpha = 0.5,
               data = predictions_new_over_mountain_tbl) +
    ggrepel::geom_text_repel(aes(label = model_id, y = .pred),
                             size = 3,
                             data = predictions_new_over_mountain_tbl)

g2

# 5.2 NEW TRIATHALON MODEL ----

new_triathalon_slice_tbl <- tibble(
    model = "Slice Al 1",
    frame_material = as.factor("Aluminum"),
    category_2 = as.factor("Triathalon"),
    model_base = as.factor("Slice"),
    model_tier = as.factor("Ultegra"),
    black      = 0,
    hi_mod     = 0,
    team       = 0,
    red        = 0,
    ultegra    = 0,
    dura_ace   = 0,
    disc       = 0
) 


# Linear Methods ----
predict(model_03_linear_glmnet, new_data = new_triathalon_slice_tbl)
# price = -241

# Tree-Based Methods ----
predict(model_07_boost_tree_xgboost, new_data = new_triathalon_slice_tbl)
# price = 2315

# Iteration for all models
predictions_new_triathalon_tbl <- models_tbl %>%
    mutate(predictions = map(model, predict, new_data = new_triathalon_slice_tbl)) %>%
    unnest(predictions) %>%
    mutate(category_2 = "Triathalon", frame_material = "Aluminum")


predictions_new_triathalon_tbl

# Update Plot
g2 +
    geom_point(aes(y = .pred), color = "red", alpha = 0.5, 
               data = predictions_new_triathalon_tbl) +
    ggrepel::geom_text_repel(aes(y = .pred, label = model_id), 
                             size = 3,
                             data = predictions_new_triathalon_tbl) 

# ALTERNATIVE - highlights in red model y-prediction < 0 
g2 +
    geom_point(data = predictions_new_triathalon_tbl, 
               aes(y = .pred, color = .pred < 0)) +
    
    scale_color_manual(values = c("#79b321", "red")) +
    
    ggrepel::geom_text_repel(data = predictions_new_triathalon_tbl, 
                              aes(y = .pred, label = model_id),
                              size = 3)


# 6.0 ADDITIONAL ADVANCED CONCEPTS ---- 

# - CLASSIFICATION - Binary & Multi-Class
# - ADVANCED ALGORITHMS
#   - SVMs - svm_poly() and svm_rbf() - Must be normalized
#   - Neural Networks - keras - Must be normalized
#   - Stacking Models 
# - PREPROCESSING - recipes 
# - HYPERPARAMETER TUNING - purrr
# - SAMPLING & CROSS VALIDATION - rsample 
# - AUTOMATIC MACHINE LEARNING - H2O


# 7.0 BONUS - PREPROCESSING & SVM-Regression ----

library(recipes)

?recipe
?step_dummy
?prep
?bake

train_tbl 

# BEFORE
recipe_obj <- recipe(price ~ ., data = train_tbl) %>%
    # remove variables
    step_rm(id, model, model_tier) %>% 
    # preform initial calculations
    prep()

# applies the prepared recipe to the data set
bake(recipe_obj, train_tbl) %>% glimpse()


# AFTER
recipe_obj <- recipe(price ~ ., data = train_tbl) %>%
    # remove variables
    step_rm(id, model, model_tier) %>%
    # encode categorical variables
    # all_nominal() select all categorical vars
    # one_hot = get all categories as columns
    step_dummy(all_nominal(), one_hot = TRUE) %>%    
    
    # *** FIX 4 *** ----
    # Error: Assigned data `log(new_data[[col_names[i]]] + object$offset, base = object$base)` must be compatible with existing data.
    # - Recipe interface has changed when applying recipes to the target
    # - Need to use skip = TRUE

    # step_log(price) %>%
    # step_center(price) %>%
    # step_scale(price) %>%
    
    # price transformations for SVM
    # logarithmic
    step_log(price, skip = TRUE) %>%
    # substract the mean of the numeric feature
    step_center(price, skip = TRUE) %>%
    # shift the fange to (-1, 1)
    step_scale(price, skip = TRUE) %>%
    
    # *** END FIX 4 *** ----
    
    # preform initial calculations
    prep()

bake(recipe_obj, train_tbl) %>% glimpse() 

# *** FIX *** ----
# - Recipe interface has changed when applying recipes to the target (when skip = TRUE)
# - Need to juice() instead of bake()

train_transformed_tbl <- bake(recipe_obj, train_tbl)
train_transformed_tbl <- juice(recipe_obj)

# *** END FIX *** ----

# tibble with encoded categorical variables
test_transformed_tbl  <- bake(recipe_obj, test_tbl)

# tidying a recipe returns information
tidy(recipe_obj)

# reverse order ????=
scale  <- tidy(recipe_obj, 5) # the num = step_number; 5 = scale
center <- tidy(recipe_obj, 4) # 4 = center


# SVM: Radial Basis
?svm_rbf 
?kernlab::ksvm

train_transformed_tbl %>% glimpse()

# BEFORE (Training error : 0.091836, mae = 1704)
model_08_svm_rbf <- svm_rbf("regression") %>%
    set_engine("kernlab") %>%
    # the data is now all numeric (0,1)
    fit(price ~ ., data = train_transformed_tbl) # . = everything

# we need to retransform the data
model_08_svm_rbf %>%
    predict(new_data = test_transformed_tbl) %>%
    # reversing the data (scale, center, and log)
    mutate(
        .pred = .pred * scale$value,
        .pred = .pred + center$value,
        .pred = exp(.pred)
    ) %>%
    bind_cols(test_tbl %>% select(price)) %>%
    yardstick::metrics(truth = price, estimate = .pred)


# AFTER (Training error : 0.075172, mae = 992)
model_08_svm_rbf <- svm_rbf("regression", 
                            cost = 10, 
                            rbf_sigma = 0.1, 
                            margin = 0.25) %>%
    
    set_engine("kernlab", scaled = FALSE) %>%
    fit(price ~ ., data = train_transformed_tbl)

model_08_svm_rbf %>% 
    predict(new_data = test_transformed_tbl) %>%
    mutate(
        .pred = .pred * scale$value,
        .pred = .pred + center$value,
        .pred = exp(.pred)
        ) %>%
    bind_cols(test_tbl %>% select(price)) %>%
    yardstick::metrics(truth = price, estimate = .pred)


# Predictions (price = 2044)
bake(recipe_obj, new_data = new_over_mountain_jekyll) %>%
    predict(object = model_08_svm_rbf, new_data = .) %>%
    # re-transform the data back
    mutate(
        .pred = .pred * scale$value,
        .pred = .pred + center$value,
        .pred = exp(.pred)
    )

predictions_new_over_mountain_tbl

g2

# g3

bake(recipe_obj, new_data = new_triathalon_slice_tbl) %>%
    predict(object = model_08_svm_rbf, new_data = .) %>%
    mutate(
        .pred = .pred * scale$value,
        .pred = .pred + center$value,
        .pred = exp(.pred)
    )

predictions_new_triathalon_tbl

# comparing 'Endurance Road' price
bike_features_tbl %>% 
    filter(category_2 == "Endurance Road") %>%
    arrange(price)

## svm seems to be the best model so far

# 8.0 SAVING & LOADING MODELS ----

#fs::dir_create("00_models")
fs::dir_create("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_models")

models_tbl <- list(
    "MODEL_01__LM_SIMPLE"       = model_01_linear_lm_simple,
    "MODEL_02__LM_COMPLEX"      = model_02_linear_lm_complex,
    "MODEL_03__GLMNET"          = model_03_linear_glmnet,
    "MODEL_04__DECISION_TREE"   = model_04_tree_decision_tree,
    "MODEL_05__RF_RANGER"       = model_05_rand_forest_ranger,
    "MODEL_06__RF_RANDOMFOREST" = model_06_rand_forest_randomForest,
    "MODEL_07__XGBOOST"         = model_07_boost_tree_xgboost,
    "MODEL_08__SVM"             = model_08_svm_rbf
) %>%
    # turns a list into a data frame (tibble)
    enframe(name = "model_id", value = "model")

models_tbl

# save as rds object
# models_tbl %>% write_rds("00_models/parsnip_models_tbl.rds")
models_tbl %>% write_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_models/parsnip_models_tbl.rds")

# save recipe object - same as above
recipes_tbl <- list(
    "RECIPE_01" = recipe_obj 
) %>%
    enframe(name = "recipe_id", value = "recipe")

recipes_tbl %>% write_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_models/recipes_tbl.rds")

# save metrics (functions > scripts)
calc_metrics %>% write_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_scripts/calc_metrics.rds")

 
# Reading 
models_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_models/parsnip_models_tbl.rds")

recipes_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_models/recipes_tbl.rds")
 
calc_metrics <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_scripts/calc_metrics.rds")


