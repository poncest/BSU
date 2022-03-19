# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PRIMER ON PURRR ----
# Programmatically getting Excel files into R
excel_paths_tbl <- 
    fs::dir_info('R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/')

paths_chr <- 
    excel_paths_tbl %>% 
    pull(path)

# What Not To Do: Don't use for loops

excel_list <- list()
for (path in paths_chr) {
    excel_list[[path]] <- read_excel(path)
}

excel_list


# What to Do: Use map()
?map

# method 1 - function name
excel_list2 <- 
    paths_chr %>% 
        map(read_excel) %>% 
        set_names(paths_chr)

# method 2 - anonymous function
paths_chr %>% 
    map(~ read_excel(.))

# method 3 - function specified with function()
paths_chr %>% 
    map(function(x) read_excel(path = x))


# Reading Excel Sheets

excel_sheets('R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx') %>% 
    
    map(~ read_excel(path = 'R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx', sheet = .)) 



# 2.0 MAPPING DATA FRAMES ----

# 2.1 Column-wise Map ----




# 2.2 Map Variants ----





# 2.3 Row-wise Map ----





# 3.0 NESTED DATA ----

# Unnest


# Nest



# Mapping Nested List Columns














# 4.0 MODELING WITH PURRR ----

# 4.1 Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))




# 4.2 Modeling Primer ----

# Data Preparation



# Making a loess model



# Working With Broom


    
    
# Visualizing results
    




# 4.3 Function To Return Fitted Results ----




# 4.4 Test Function on Single Element ----


# 4.5 Map Function to All Categories ----

# Map Functions



# Visualize Results



