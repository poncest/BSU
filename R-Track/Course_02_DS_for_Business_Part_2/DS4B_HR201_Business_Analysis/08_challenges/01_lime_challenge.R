# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(pacman)
p_load(h2o, recipes, readxl, tidyverse, tidyquant, lime)


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


# 4. CHALLENGE ---
#This is a two part challenge:
    
# Part 1: Recreate plot_features(). Take the explanation data and use the first case to create a plot similar to the output of plot_features().

#' @importFrom tools toTitleCase
label_both_upper <- function(labels, multi_line = TRUE, sep = ': ') {
    names(labels) <- toTitleCase(names(labels))
    label_both(labels, multi_line, sep)
}


plot_features_tq <- function(explanation, ncol = 2, cases = NULL) {
    type_pal <- c('Supports', 'Contradicts')
    
    if (!is.null(cases)) {
        explanation <- explanation[explanation$case %in% cases, , drop = FALSE]
    }
    
    if (nrow(explanation) == 0) stop("No explanations to plot", call. = FALSE)
    
    if (explanation$model_type[1] == 'regression') {
        type_pal       <- c('Positive', 'Negative')
        binned_feature <- grepl("=|>|<", explanation$feature_desc)
        explanation[!binned_feature, "feature_weight"] <- as.numeric(explanation$feature_value[!binned_feature]) * explanation$feature_weight[!binned_feature]
    }
    
    explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 1, type_pal[1], type_pal[2]), levels = type_pal)
    description <- paste0(explanation$case, '_', explanation[['label']])
    desc_width  <- max(nchar(description)) + 1
    description <- paste0(format(description, width = desc_width), explanation$feature_desc)
    explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])
    explanation$case <- factor(explanation$case, unique(explanation$case))
    explanation$`Explanation fit` <- format(explanation$model_r2, digits = 2)
    
    if (explanation$model_type[1] == 'classification') {
        explanation$probability <- format(explanation$label_prob, digits = 2)
        explanation$label <- factor(explanation$label, unique(explanation$label[order(explanation$label_prob, decreasing = TRUE)]))
        p <- ggplot(explanation) +
            facet_wrap(~ case + label + probability + `Explanation fit`, labeller = label_both_upper, scales = 'free_y', ncol = ncol)
    } else if (explanation$model_type[1] == 'regression') {
        p <- ggplot(explanation) +
            facet_wrap(~ case + prediction + `Explanation fit`, labeller = label_both_upper, scales = 'free_y', ncol = ncol)
    }
    p +
        geom_col(aes_(~description, ~feature_weight, fill = ~type)) +
        coord_flip() +
        #scale_fill_manual(values = c('steelblue', 'firebrick'), drop = FALSE) +
        scale_color_tq() +
        
        scale_x_discrete(labels = function(lab) substr(lab, desc_width + 1, nchar(lab))) +
        labs(y = 'Weight', x = 'Feature', fill = '') +
        
        #theme_lime()
        theme_tq()
        explanation <- explanation[explanation$case]
    }





explanation %>% 
as_tibble()

case_1 <- explanation %>%
    filter(case == 1)

case_1 %>%
    plot_features_tq()

#Part 2: Recreate plot_explanations():
    
#  Take the full explanation data and recreate the second plot.
# You will need at least the layers geom_tile() and facet_wrap().





# HINTS:
    
# If you do get stuck on this challenge, because this is actually a rather difficult challenge, I highly recommend checking out the library lime from Thomas Pedersens’ github page https://github.com/thomasp85/lime. All of the R code is in the folder R. 
