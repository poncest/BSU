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



# 4.0 PLOT STOCK DATA ----



# 5.0 GENERATE COMMENTARY ----



# 6.0 TEST WORKFLOW ----



# 7.0 SAVE SCRIPTS ----
