## HR_201_Employee_Attrition_Project
## Steven Ponce 2022

# DATA PREPARATION ----
# Machine Readable ----


# Setup ----

# Libraries
library(pacman)
p_load(tidyverse, tidyquant, readxl, recipes)


# Data
path_train            <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'))
path_test             <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_test.xlsx'))
path_data_definitions <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_data_definitions.xlsx'))

 
train_raw_tbl         <- read_excel(path_train, sheet = 1)
test_raw_tbl          <- read_excel(path_test, sheet = 1)
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)


# Processing Pipeline - for machine readability
source(here::here('R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/data_processing_pipeline_rev1.R'))
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# Custom Function plot_hist_facet() ----

data <- train_raw_tbl
    
plot_hist_facet <- function(data, bins = 10, ncol = 5,
                            fct_reorder = FALSE, fct_rev = FALSE,
                            fill = palette_light()[[3]],
                            color = 'white', scale = 'free') {
    
    data_factored <- data %>% 
        mutate_if(is.character, as.factor) %>% 
        mutate_if(is.factor, as.numeric) %>% 
        gather(key = key, value = value, factor_key = TRUE)
    
    if(fct_reorder) {
        data_factored <- data_factored %>% 
            mutate(key = as.character(key) %>% as.factor())
    }
    
    if(fct_rev) {
        data_factored <- data_factored %>% 
            mutate(key = fct_rev(key))
    }
    
    g <- data_factored %>% 
        ggplot(aes(x = value, group = key)) +
        geom_histogram(bins = bins, fill = fill, color = color) + 
        facet_wrap(~ key, ncol = ncol, scale = scale) + 
        theme_tq()
    
    return(g)
    
}

#Testing plot_hist_facet function
train_raw_tbl %>% 
    plot_hist_facet(bins = 10, ncol = 5)
                            
train_raw_tbl %>% 
    select(Attrition, everything()) %>% 
    plot_hist_facet(bins = 10, ncol = 5, fct_rev = FALSE)









