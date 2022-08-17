# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(pacman)
p_load(h2o, recipes, readxl, tidyverse, tidyquant, lime, glue)


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

# bake
train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)


# 2. Models ----

# initialize
h2o.init()

# load model
automl_leader <- h2o.loadModel("R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/04_modeling/h2o_models/StackedEnsemble_BestOfFamily_1_AutoML_3_20220715_102232")

automl_leader

# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>% 
    h2o.predict(newdata = as.h2o(test_tbl)) %>% 
    as_tibble() %>% 
    bind_cols(
        test_tbl %>% 
            select(Attrition, EmployeeNumber)
    )


# Let’s investigate the 1st employee, that did indeed leave the company:
test_tbl %>% 
    slice(5) %>%   # first yes (attrition) from the prediction_tbl
    glimpse()


# |- Lime for single explanation, Part 1 ---- 
# LIME is used to determine which features contribute to the prediction (& by how much) for a single observation (i.e. local). h2o, keras & caret R packages have been integrated into lime.  Using Lime is is a 2 steps process:

# 1. Build an explainer with lime() (“recipe” for creating an explanation. It contains the ML model & feature distributions (bins) for the training data.)
# 2. Create an explanation with explain()


# 3.2 Single Explanation ----
# step 1 lime()
explainer <- train_tbl %>%
    select(-Attrition) %>%
    lime(
        model           = automl_leader,
        bin_continuous  = TRUE,
        n_bins          = 4,
        quantile_bins   = TRUE
    )

explainer


# |- Lime for single explanation, Part 2 ---- 
# LIME Algorithm 6 Steps:

# 1. Given an observation, permute it to create replicated feature data with slight value modifications.
# 2. Compute similarity distance measure between original observation and permuted observations.
# 3. Apply selected machine learning model to predict outcomes of permuted data.
# 4. Select m number of features to best describe predicted outcomes.
# 5. Fit a simple model to the permuted data, explaining the complex model outcome with m features from 
#    the permuted data weighted by its similarity to the original observation.
# 6. Use the resulting feature weights to explain local behavior.


?lime::explain

# kernel_width: Affects the lime linear model fit (R-squared value) and therefore should be 
# tuned to make sure you get the best explanations.

explanation <- test_tbl %>%
    slice(5) %>%
    select(-Attrition) %>%
    lime::explain(
        
        # Pass our explainer object
        explainer      = explainer,
        # Because it is a binary classification model: 1
        n_labels       = 1,
        # number of features to be returned
        n_features     = 8,
        # number of localized linear models
        n_permutations = 5000,
        # Let's start with 1
        kernel_width   = 1.5
    )

explanation

# In my case the R-squared value (model_r2) is a little bit low. This is what you want to look at for lime. 
# This is how you investigate your lime models. You want those values as high as possible and you can 
# adjust that using your kernel_width (0.5 or 1.5 gave me better results).

# kernel_width   = 0.5; model_r2 = 0.318
# kernel_width   = 1.0; model_r2 = 0.331
# kernel_width   = 1.0; model_r2 = 0.338


# Let’s select the columns, that are important to us.
explanation %>%
    as_tibble() %>%
    select(feature:prediction) 


# Visualizing feature importance for a single explanation
# plot_feature()
g <- plot_features(explanation = explanation, ncol = 1)
g

# Info: 4 < NumCompaniesWorked Note that this label is the result of the continuous variable binning strategy.
# One of the cuts is at 4, which is how we get this label.


# 3.3 Multiple Explanations ----
#|- Visualizing Feature Importance For Multiple Explanations ----
explanation <- test_tbl %>%
    slice(1:20) %>%              # return the first 20 observations
    select(-Attrition) %>%
    lime::explain(
        explainer = explainer,
        n_labels   = 1,
        n_features = 8,
        n_permutations = 5000,
        kernel_width   = 1.5
    )

explanation %>%
    as_tibble()

plot_features(explanation, ncol = 4)

# The plot will be pretty messy. You can change that a little bit by expanding it. But it will still be a little bit messy 
# and tough to read. It’s too much information to be reported for 20 different cases. If we did only 3 or 4 cases, we could 
# analyze it that way. But for more cases we need a better method to analyze it. That’s why the next function 
# comes into play plot_explanations():

plot_explanations(explanation)



# 4. CHALLENGE ----
# This is a two part challenge:
    
# Part 1: Recreate plot_features() ----
# Take the explanation data and use the first case to create a plot similar to the output of plot_features().

explanation %>% 
    as_tibble()

case_1 <- explanation %>%
    filter(case == 1)

case_1 %>%
    plot_features()


# Transformation
data_transformed <- case_1 %>% 
    as_tibble() %>% 
    mutate(
        feature_desc = as_factor(feature_desc) %>% 
            fct_reorder(abs(feature_weight), .desc = FALSE),
        
        key = ifelse(feature_weight > 0, 'Supports', 'Contradicts') %>% 
            fct_relevel('Supports'),
        
        case_text  = glue('Case: {case}'),
        label_text = glue('Label: {label}'),
        prob_text  = glue('Probability: {round(label_prob, 2)}'),
        r2_text    = glue('Explanation Fit: {round(model_r2, 2)}'),
    ) %>% 
    
    select(feature_desc, feature_weight, key, case_text:r2_text)
    
# Visualization
data_transformed %>% 
    ggplot(aes(feature_desc, feature_weight, fill = key)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    scale_fill_tq() +
    theme_tq() +
    labs(x = 'Feature', y = 'Weight') + 
    theme(title = element_text(size = 9)) +
    facet_wrap(~ case_text + label_text + prob_text + r2_text,
               ncol = 1, scales = 'free')
    

plot_features_tq <- function(explanation, ncol) {
    
    # Transformation
    data_transformed <- case_1 %>% 
        as_tibble() %>% 
        mutate(
            feature_desc = as_factor(feature_desc) %>% 
                fct_reorder(abs(feature_weight), .desc = FALSE),
            
            key = ifelse(feature_weight > 0, 'Supports', 'Contradicts') %>% 
                fct_relevel('Supports'),
            
            case_text  = glue('Case: {case}'),
            label_text = glue('Label: {label}'),
            prob_text  = glue('Probability: {round(label_prob, 2)}'),
            r2_text    = glue('Explanation Fit: {round(model_r2, 2)}'),
        ) %>% 
        
        select(feature_desc, feature_weight, key, case_text:r2_text)
    
    # Visualization
    data_transformed %>% 
        ggplot(aes(feature_desc, feature_weight, fill = key)) +
        geom_bar(stat = 'identity') +
        coord_flip() +
        scale_fill_tq() +
        theme_tq() +
        labs(x = 'Feature', y = 'Weight') + 
        theme(title = element_text(size = 9)) +
        facet_wrap(~ case_text + label_text + prob_text + r2_text,
                   ncol = 1, scales = 'free')
    
}

# testing
plot_features_tq(explanation = explanation, ncol = 1)

# ERROR
explanation %>% 
    filter(case %in% 1:6) %>% 
    plot_features_tq(ncol = 3)





# Part 2: Recreate plot_explanations() ----
    
# Take the full explanation data and recreate the second plot.
# You will need at least the layers geom_tile() and facet_wrap().





# HINTS:
    
# If you do get stuck on this challenge, because this is actually a rather difficult challenge, I highly recommend checking out the library lime from Thomas Pedersens’ github page https://github.com/thomasp85/lime. All of the R code is in the folder R. 


