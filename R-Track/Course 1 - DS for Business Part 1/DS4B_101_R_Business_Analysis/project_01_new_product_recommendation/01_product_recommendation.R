# DS4B 101-R ----
# PRODUCT RECOMMENDATION FUNCTIONS

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(parsnip)
library(plotly)

source("./R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_scripts/separete_bikes_and_outlier_detection.R")

bike_orderlines_tbl <- read_rds("./R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

models_tbl <- read_rds("./R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_models/parsnip_models_tbl.rds")


# 2.0 BIKE FEATURES ----

get_bike_features <- function() {
    
    bike_features_tbl <- bike_orderlines_tbl %>% 
        
        select(price,model, category_1, category_2, frame_material) %>% 
        
        distinct() %>% 
        
        mutate(id = row_number()) %>% 
        
        select(id, everything()) %>% 
        
        separate_bike_model(keep_model_column = TRUE, append = TRUE)
        
    return(bike_features_tbl)
}

get_bike_features()
 

plot_bike_features <- function(interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
}

plot_bike_features()
plot_bike_features(interactive = FALSE)


# 3.0 SAVE FUNCTIONS ----

function_names <- c("get_bike_features", "plot_bike_features")

dump(function_names, file = "R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_scripts/plot_product_recommendation.R")


