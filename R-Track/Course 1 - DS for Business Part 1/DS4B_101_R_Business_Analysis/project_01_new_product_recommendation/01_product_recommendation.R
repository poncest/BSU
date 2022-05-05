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
    bike_feature_tbl <- get_bike_features()
    
    # VISUALIZATION
    g <- bike_feature_tbl %>% 
        
        mutate(category_2 = fct_reorder(category_2, price)) %>% 
        
        mutate(label_text = str_glue("Model: {model}
                                     Price: {scales::dollar(price)}")) %>% 
        
        ggplot(aes(x = category_2, y = price)) +
        
        geom_violin() +
        
        geom_jitter(aes(text = label_text), width = 0.1, color = '#2C3E50', alpha = 0.5) +
        
        facet_wrap(~ frame_material) + 
        coord_flip() +
        
        scale_y_continuous(labels = scales::dollar_format()) +
        
        labs(
            title = 'Prodruct Gap Analysis',
            subtitle = '',
            x = '',
            y = ''
        ) +
        
        theme_tq(14) +
        
        theme(
            plot.margin   = margin(t = 10, r = 20, b = 10, l = 20),
            strip.text    = element_text(margin = margin(5,5,5,5, unit = 'pt')),
            plot.title    = element_text(face = "bold", size = 20, margin = margin(t = 10, b = 5)),
        ) 
         
    
    # INTERACTIVE vs. STATIC
    
    if (interactive) {
        ggplotly(g, tooltip = 'text')
        
    } else {
        return(g)
    }
}

plot_bike_features()
plot_bike_features(interactive = FALSE)
plot_bike_features(interactive = TRUE)

# 3.0 SAVE FUNCTIONS ----

function_names <- c("get_bike_features", "plot_bike_features")

dump(function_names, file = "R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_scripts/plot_product_recommendation.R")




