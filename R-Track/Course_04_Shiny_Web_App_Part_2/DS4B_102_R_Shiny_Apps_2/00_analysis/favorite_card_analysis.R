# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - FAVORITE CARD ANALYSIS -----
# Version 1

# APPLICATION DESCRIPTION ----
# - The user will select 1 stock from the SP 500 stock index
# - [DONE] The functionality is designed to pull the past 180 days of stock data 
# - We will cover the historic data to 2 moving averages - short (fast) and long (slow)
# - We will make a function to generate the moving average cards

# SETUP ----
library(tidyquant)
library(tidyverse)

source(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/stock_analysis_functions.R"))
source(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/info_card.R"))

stock_list_tbl <- get_stock_list("SP500")

favorite_list_on_start <- c("AAPL", "GOOG", "NFLX")


# 1.0 Get Stock Data for Each Favorite ----
stock_data_favorites_tbl <- favorite_list_on_start %>% 
    map(get_stock_data) %>% 
    set_names(favorite_list_on_start)


# 2.0 Get Moving Average Data for Each Stock History ----
data <- stock_data_favorites_tbl$AAPL

get_stock_mavg_info <- function(data) {
    
    n_short <- data %>% pull(moving_avg_short) %>% is.na() %>% sum() + 1
    n_long  <- data %>% pull(moving_avg_long) %>% is.na() %>% sum() + 1
    
    data %>% 
        tail(1) %>% 
        mutate(
            moving_avg_flag = moving_avg_short < moving_avg_long,
            n_short         = n_short,
            nlong           = n_long,
            pct_change      = (moving_avg_short - moving_avg_long) / moving_avg_long
        )
}

# testing get_stock_mavg_info()
stock_data_favorites_tbl %>% 
    map(get_stock_mavg_info)


# 3.0 Generate Favorite Card ----


# 4.0 Generate All Favorite Cards in a TagList ----



# 5.0 Save Functions ----
dump(c("get_stock_mavg_info", "generate_favorite_card", 'generate_favorite_cards'), 
     file = "00_scripts/generate_favorite_cards.R", append = FALSE)
