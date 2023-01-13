# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - DATA ANALYSIS -----
# Version 1

# APPLICATION DESCRIPTION ----
# - The user will select 1 stock from the SP 500 stock index
# - The functionality is designed to pull the past 180 days of stock data
# - We will implement 2 moving averages - short (fast) and long (slow)
# - We will produce a timeseries visualization
# - We will produce automated commentary based on the moving averages

# REPRODUCIBILITY REQUIREMENTS
# - The functionality is designed to pull the past 180 days of stock data from today's date
# - Because of this, your analysis may differ from mine
# - To reproduce my analysis, replace today() with ymd("2019-08-19")

# LIBRARIES ----
library(plotly)
library(tidyquant)
library(tidyverse)
library(fs)



# 1.0 GET STOCK LIST ----
stock_list_tbl <- tq_index("SP500") %>% 
    select(symbol, company) %>% 
    arrange(symbol) %>% 
    mutate(label = str_c(symbol, company, sep = ", ")) %>% 
    select(label)

# get_stock_list()
get_stock_list <- function(stock_index = "SP500") {
    tq_index(stock_index) %>% 
        select(symbol, company) %>% 
        arrange(symbol) %>% 
        mutate(label = str_c(symbol, company, sep = ", ")) %>% 
        select(label)
}

# testing get_stock_list()
get_stock_list("SP500") 

# other index options
tq_index_options() 
get_stock_list("DOW")


# 2.0 EXTRACT SYMBOL BASED ON USER INPUT ----

user_input <- "AAPL, Apple Inc."

user_input %>% 
    str_split(pattern = ", ") %>% 
    pluck(1, 1)

# get_symbol_from_user_input()
get_symbol_from_user_input <- function(user_input){
    user_input %>% 
        str_split(pattern = ", ") %>% 
        pluck(1, 1)
}

# testing get_symbol_from_user_input()

"ABC, AmerisourceBergen Corporation" %>% get_symbol_from_user_input()
"ACGL, Arch Capital Group Ltd." %>% get_symbol_from_user_input()

# 3.0 GET STOCK DATA ----

from <- today() - days(180)
to   <- today()

"PFE" %>% 
    tq_get(get = "stock.prices", from = from, to = to) %>% 
    select(date, adjusted) %>% 
    
    # if we have a moving avg value of 5 (k =5), there should be four NA's               
    mutate(moving_avg_short = rollmean(adjusted, k = 5, na.pad = TRUE, align = 'right')) %>% 
    mutate(moving_avg_long = rollmean(adjusted, k = 50, na.pad = TRUE, align = 'right')) 


get_stock_data <- function(stock_symbol, 
                           from = today() - days(180), 
                           to = today(), 
                           moving_avg_short = 20, 
                           moving_avg_long = 50) {
    
    stock_symbol %>% 
        tq_get(get = "stock.prices", from = from, to = to) %>% 
        select(date, adjusted) %>% 
        
        # if we have a moving avg value of 5 (k =5), there should be four NA's               
        mutate(moving_avg_short = rollmean(adjusted, k = moving_avg_short, na.pad = TRUE, align = 'right')) %>% 
        mutate(moving_avg_long = rollmean(adjusted, k = moving_avg_long, na.pad = TRUE, align = 'right')) 
    
} 


# testing get_stock_data()
stock_data_tbl <- get_stock_data(stock_symbol = "PFE", from = "2018-01-01", to = "2020-06-30", 
               moving_avg_short = 5, moving_avg_long = 8)


# 4.0 PLOT STOCK DATA ----

g <- stock_data_tbl %>% 
    gather(key = "legend", value = "value", adjusted:moving_avg_long, factor_key = TRUE) %>% 
   
    ggplot(aes(date, value, color = legend)) +
    geom_line(aes(linetype = legend)) +
    
    # scales
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
    scale_color_tq()+
    
    # labs
    labs(x = "", y = "Adjusted Share Price")+ 
    
    # theme
    theme_tq() 

ggplotly(g)
    

# plot_stock_data()
plot_stock_data <- function(data) {
    g <- data %>% 
        gather(key = "legend", value = "value", adjusted:moving_avg_long, factor_key = TRUE) %>% 
        
        ggplot(aes(date, value, color = legend)) +
        geom_line(aes(linetype = legend)) +
        
        # scales
        scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
        scale_color_tq()+
        
        # labs
        labs(x = "", y = "Adjusted Share Price" )+ 
        
        # theme
        theme_tq() 
    
    plot <- ggplotly(g)
    
    return(plot)
    
}

# testing plot_stock_data()
plot_stock_data(stock_data_tbl)

"AMZN" %>% 
    get_stock_data() %>% 
    plot_stock_data()


# 5.0 GENERATE COMMENTARY ----
warning_signal <- stock_data_tbl %>% 
    tail(1) %>% 
    mutate(moving_warning_flag = moving_avg_short < moving_avg_long) %>% 
    pull(moving_warning_flag)

# develop a message

# value for moving_avg_short
n_short <- stock_data_tbl %>% pull(moving_avg_short) %>% is.na() %>% sum() + 1

# value for moving_avg_long
n_long <- stock_data_tbl %>% pull(moving_avg_long) %>% is.na() %>% sum() + 1

if (warning_signal) {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day movig average, indicating negative trend") %>% str_wrap(width = 80)
    
} else {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day movig average, indicating positive trend") %>% str_wrap(width = 80)
}



generate_commentary <- function(data, user_input) {
    warning_signal <- data %>% 
        tail(1) %>% 
        mutate(moving_warning_flag = moving_avg_short < moving_avg_long) %>% 
        pull(moving_warning_flag)
    
    # value for moving_avg_short
    n_short <- data %>% pull(moving_avg_short) %>% is.na() %>% sum() + 1
    
    # value for moving_avg_long
    n_long <- data %>% pull(moving_avg_long) %>% is.na() %>% sum() + 1
    
    if (warning_signal) {
        str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day movig average, indicating negative trend") %>% str_wrap(width = 80)
        
    } else {
        str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day movig average, indicating positive trend") %>% str_wrap(width = 80)
    }
}

# testing generate_commentary()
generate_commentary(data = stock_data_tbl, user_input = "AMZN")
generate_commentary(data = stock_data_tbl, user_input = "AAPL")


get_stock_data(stock_symbol = "BMY", moving_avg_short = 10, moving_avg_long = 80) %>% 
    plot_stock_data()  

get_stock_data(stock_symbol = "BMY", moving_avg_short = 5, moving_avg_long = 80) %>% 
    generate_commentary(user_input = "BMY")  

# 6.0 TEST WORKFLOW ----



# 7.0 SAVE SCRIPTS ----
