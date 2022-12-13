# DS4B 102-R: PREDICTIVE WEB APPLICATIONS FOR BUSINESS ----
# DEMAND FORECAST ANALYSIS ----

# 1.0 LIBRARIES -----

# Core
library(tidyverse)
library(tidyquant)

# Interactive Visualizations
library(plotly)

# Modeling Libraries
library(parsnip)
library(timetk)

# Database
library(odbc)
library(RSQLite)


# 2.0 PROCESSED DATA ----
con <- dbConnect(RSQLite::SQLite(), "R-Track/Course_03_Shiny_Web_App_Part_1/DS4B_102_R_Shiny_Apps_1/00_data/bikes_database.db")

# con <- dbConnect(RSQLite::SQLite(), "00_data/bikes_database.db")

bikes_tbl <- tbl(con, "bikes")
bikeshops_tbl <- tbl(con, "bikeshops")
orderlines_tbl <- tbl(con, "orderlines")

processed_data_tbl <- orderlines_tbl %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id")) %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    mutate(extended_price = quantity * price) %>%
    collect()

processed_data_tbl <- processed_data_tbl %>%    
    mutate(order.date = ymd(order.date)) %>%
    separate(location, into = c("city", "state"), sep = ", ") %>%
    
    separate(description, 
             into = c("category_1", "category_2", "frame_material"),
             sep = " - ") %>%
    
    select(order.date, order.id, order.line, state, quantity, price,
           extended_price, category_1:frame_material, bikeshop.name)

dbDisconnect(con)


# 3.0 TIME SERIES AGGREGATION ----

# 3.1 DATA MANIPULATION ----
time_unit <- "quarter"

time_plot_tbl <- processed_data_tbl %>%
    
    mutate(date = floor_date(order.date, unit = time_unit)) %>%
    
    group_by(date) %>%
    summarize(total_sales = sum(extended_price)) %>%
    ungroup() %>%
    
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))

time_plot_tbl

# 3.2 FUNCTION ----

# aggregate_time_series() 

aggregate_time_series <- function(data, time_unit = "month"){
    
    output_tbl <- data %>%
        
        mutate(date = floor_date(order.date, unit = time_unit)) %>%
        
        group_by(date) %>%
        summarize(total_sales = sum(extended_price)) %>%
        ungroup() %>%
        
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))
    
    return(output_tbl)
    
    
}


# testing aggregate_time_series() 
processed_data_tbl %>% 
    aggregate_time_series(time_unit = "year")



# 3.3 TIME SERIES PLOT ----

data <- processed_data_tbl %>%
    aggregate_time_series("month")

g <- data %>%
    
    ggplot(aes(date, total_sales)) +
    
    geom_line(color = "#2c3e50") +
    geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "")


ggplotly(g, tooltip = "text")


# 3.4 FUNCTION ----

# plot_time_series()

plot_time_series <- function(data){
    
    g <- data %>%
        
        ggplot(aes(date, total_sales)) +
        
        geom_line(color = "#2c3e50") +
        geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
        geom_smooth(method = "loess", span = 0.2) +
        
        theme_tq() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "", y = "")
    
    
    ggplotly(g, tooltip = "text") 
}


# testing plot_time_series()
processed_data_tbl %>% 
    aggregate_time_series(time_unit = "month") %>% 
    plot_time_series()


# 4.0 FORECAST -----

# 4.1 SETUP TRAINING DATA AND FUTURE DATA ----

# {timetk}
# Actual Table (pipeline)
data <- processed_data_tbl %>% 
    aggregate_time_series(time_unit = "year")

# basic function of {timetk}
data %>% tk_index() %>% tk_get_timeseries_signature() 
data %>% tk_index() %>% tk_get_timeseries_summary()

tk_get_timeseries_unit_frequency()
data %>% tk_get_timeseries_variables()

data %>% tk_augment_timeseries_signature()


# train data
train_tbl <- data %>%  
    tk_augment_timeseries_signature()


future_data_tbl <- data %>% 
    tk_index() %>% 
    tk_make_future_timeseries(length_out = 12, # time_unit is set to month 
                              inspect_months = TRUE,
                              inspect_weekdays = TRUE) %>% 
    tk_get_timeseries_signature()


#' when time_unit = 'day'
#' ?tk_make_future_timeseries
#' set inspect_weekdays = TRUE (analyzes for days each week that should be removed, weekends)
#' set inspect_months = TRUE (analyzes for days each month that should be removed, holidays)

# 4.2 MACHINE LEARNING ----

# XGBoost

seed <- 123
set.seed(seed)

model_xgbost <- boost_tree(mode = "regression", 
           mtry           = 20,
           trees          = 500,
           min_n          = 3,
           tree_depth     = 8,
           learn_rate     = 0.01,
           loss_reduction = 0.01) %>% 
    
    set_engine(engine = "xgboost") %>% 
    
    fit.model_spec(formula = total_sales ~ ., data = train_tbl %>% select(-c(date, label_text, diff))) 


# 4.3 MAKE PREDICTION & FORMAT OUTPUT ---- 

# predict

future_data_tbl 

# Prediction Table
prediction_tbl <- predict(object = model_xgbost, new_data = future_data_tbl) %>% 
    bind_cols(future_data_tbl) %>% 
    select(.pred, index) %>% 
    
    rename(total_sales = .pred,
           date        = index) %>% 
    
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>% 
    
    add_column(key = "Prediction")


output_tbl <- data %>% 
    add_column(key = "Actual") %>% 
    bind_rows(prediction_tbl) 


 
# 4.4 FUNCTION ----

# generate_forecast()

# TODO  - DELETE!
length_out <-  2 
seed       <- 123


generate_forecast <- function(data, length_out = 12, seed = NULL){
    
    # train data
    train_tbl <- data %>%  
        tk_augment_timeseries_signature()
    
    
    future_data_tbl <- data %>% 
        tk_index() %>% 
        tk_make_future_timeseries(length_out = length_out, 
                                  inspect_months = TRUE,
                                  inspect_weekdays = TRUE) %>% 
        tk_get_timeseries_signature()
    
    
    # time scale
    time_scale <- data %>% 
        tk_index() %>% 
        tk_get_timeseries_summary() %>% 
        pull(scale)
    
    if (time_scale == "year") {
        
        # if time_scale IS in years, then run linear reg model
        model <- linear_reg(mode = "regression") %>% 
            set_engine(engine = "lm") %>% 
            fit.model_spec(formula = total_sales ~ ., 
                           data = train_tbl %>% select(total_sales, index.num))
        
    } else {
        
        # if time_scale is NOT in years, then run xgboost model
        seed <- seed
        set.seed(seed)
        
        model <- boost_tree(mode = "regression", 
                                   mtry           = 20,
                                   trees          = 500,
                                   min_n          = 3,
                                   tree_depth     = 8,
                                   learn_rate     = 0.01,
                                   loss_reduction = 0.01) %>% 
            
            set_engine(engine = "xgboost") %>% 
            
            fit.model_spec(formula = total_sales ~ ., data = train_tbl %>% select(-c(date, label_text, diff))) 
        
    }
    
    # Prediction Table
    prediction_tbl <- predict(object = model, new_data = future_data_tbl) %>% 
        bind_cols(future_data_tbl) %>% 
        select(.pred, index) %>% 
        
        rename(total_sales = .pred,
               date        = index) %>% 
        
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>% 
        
        add_column(key = "Prediction")
    
    
    output_tbl <- data %>% 
        add_column(key = "Actual") %>% 
        bind_rows(prediction_tbl) 
    
    return(output_tbl) 
}


# testing generate_forecast()
processed_data_tbl %>% 
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(length_out = 12, seed = 123) 


# 5.0 PLOT FORECAST ----

# 5.1 PLOT ----

# plot

data <- processed_data_tbl %>% 
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(length_out = 12, seed = 123) 


g <- data %>% 
    ggplot(aes(date, total_sales, color = key)) +
    
    geom_line() +
    geom_point(aes(text = label_text), size = 0.1) +
    geom_smooth(method = "loess", span = 0.2) +
    
    
    scale_color_tq()+
    scale_y_continuous(label = scales::dollar_format()) +
    
    labs(x = "", y = "") +
    
    theme_tq()
    
ggplotly(g, tooltip = "text")  


# 5.2 FUNCTION ----
 
# plot_forecast()

plot_forecast <- function(data){
    
    data  <- processed_data_tbl %>% 
        aggregate_time_series(time_unit = "month") %>% 
        generate_forecast(length_out = 12, seed = 123) 
        
    # Yearly - LM Smoother
    data %>% 
        tk_index() %>% 
        tk_get_timeseries_summary() %>% 
        pull(scale)
        
    # Only 1 Prediction - points
    data %>% 
        filter(key == "Prediction") %>% 
        nrow()
    
    
    g <- data %>% 
        ggplot(aes(date, total_sales, color = key)) +
        
        geom_line() +
        geom_point(aes(text = label_text), size = 0.1) +
        geom_smooth(method = "loess", span = 0.2) +
        
        
        scale_color_tq()+
        scale_y_continuous(label = scales::dollar_format()) +
        expand_limits(y = 0) +
        
        labs(x = "", y = "") +
        
        theme_tq()
    
    ggplotly(g, tooltip = "text")  

}

# testing plot_forecast()
processed_data_tbl %>% 
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(length_out = 12, seed = 123) %>% 
    plot_forecast()

#' Is seems there is a problem when we aggregate the data by years, not granular enough. 

#' In timeseries, is very rare that one algorithm fits all sizes

processed_data_tbl %>% 
    aggregate_time_series(time_unit = "year") %>% 
    generate_forecast(length_out = 2, seed = 123) %>% 
    plot_forecast()
  


# 6.0 SAVE FUNCTIONS ----

dump(c("aggregate_time_series", "plot_time_series", "generate_forecast", "plot_forecast"), 
     file = "R-Track/Course_03_Shiny_Web_App_Part_1/DS4B_102_R_Shiny_Apps_1/00_scripts/04_demand_forecast.R")
