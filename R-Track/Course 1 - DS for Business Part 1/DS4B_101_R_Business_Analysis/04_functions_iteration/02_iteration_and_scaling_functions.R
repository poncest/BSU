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

bike_orderlines_tbl %>% is.list()

# map over columns
bike_orderlines_tbl %>% 
    map(~ class(.)[1])

# note that `order_date` has two classes:POSIXct and POSIXt


# 2.2 Map Variants ----
?map

# character map
bike_orderlines_tbl %>% 
    map_chr(~ class(.)[1])

# dataframe map
bike_orderlines_tbl %>% 
    map_df(~ class(.)[1]) %>% 
    gather()

# pct of missing values
bike_orderlines_tbl %>% 
    map_df(~ sum(is.na(.)) / length(.)[1]) %>% 
    gather()



# 2.3 Row-wise Map ----

# read all files in a directory
excel_tbl <- excel_paths_tbl %>% 
    select(path) %>% 
    mutate(data = path %>%  map(read_excel))

excel_list
excel_tbl

# 3.0 NESTED DATA ----

# Unnest
excel_tbl

excel_tbl$data

excel_tbl$data[2]

excel_tbl_unnested <- excel_tbl %>% 
    mutate(.id = 1:3) %>% 
    unnest(data) 

# Nest
excel_tbl_nested <- excel_tbl_unnested %>% 
    group_by(.id, path) %>% 
    nest()

# did not drop the NA values
excel_tbl_nested$data

# Mapping Nested List Columns

# first tbl
excel_tbl_nested$data[[1]] %>% 
    # only grabs columns that are not NAs
    select_if(~ !is.na(.) %>% all())

# examples
x <- rep(NA,5)
x
!is.na(x) %>% all()


y <- c(1:4, NA_real_)
y
!is.na(y) %>% all()


# Method 1: creating a function outside pur::map()

# step 1: create a function that can be mapped to one element
select_non_na_columns <- function(data) {
    
    data %>% 
        select_if(~ !is.na(.) %>% all())
    
} 

# step 2: extract an element, and test the function
excel_tbl_nested$data[[1]] %>% 
    select_non_na_columns()
    

# step 3: use mutate() + map() 
excel_tbl_nested_fixed <- excel_tbl_nested %>% 
    mutate(data_fixed = data %>% map(select_non_na_columns))

# before
excel_tbl_nested_fixed$data[1]
# after
excel_tbl_nested_fixed$data_fixed[1]




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
    
    # Add Loess Smoother (locally estimated scatterplot smoothing)
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))


# 4.2 Modeling Primer ----

# Data Preparation
sales_by_m_cross_country_tbl <- rolling_avg_3_tbl %>% 
    filter(category_2 == 'Cross Country Race') %>% 
    select(month_end, total_price) %>% 
    mutate(month_end_num = as.numeric(month_end))

sales_by_m_cross_country_tbl %>% 
    ggplot(aes(x = month_end_num,
               y = total_price)) +
    
    geom_point() +
    geom_smooth(method = 'loess', se = FALSE, span = 0.2)


# Making a loess model
?loess() 

fit_loess_cross_country <- sales_by_m_cross_country_tbl %>% 
    # not a tidy function (do not %>% )
    loess(total_price ~ month_end_num, span = 0.2, data = .)


# Working With Broom 

fit_loess_cross_country %>% 
    # extract the fitted results
    broom::augment() %>%   
    
    # Visualizing results
    ggplot(aes(x = month_end_num,
               y = total_price)) +
    
    geom_point() +
    geom_line(aes(y = .fitted))


# 4.3 Step 1: Function To Return Fitted Results ----

rolling_avg_3_tbl_nested <- rolling_avg_3_tbl %>% 
    
    group_by(category_1, category_2) %>% 
    nest() %>% 
    ungroup()

# test data
data <- rolling_avg_3_tbl_nested$data[[1]] 


tidy_loess <- function(data) {
    
    data_formatted <- data %>% 
        select(month_end, total_price) %>% 
        mutate(month_end_num = as.numeric(month_end))
    
    fit_loess <- loess(formula = total_price ~ month_end_num,
                       data    = data_formatted,
                       span    = 0.2)
    
    output_tbl <- fit_loess %>% 
        broom::augment() %>% 
        select(.fitted)
    
    return(output_tbl)
           
}


# 4.4 Step 2: Test Function on Single Element ----

# to test the function change [[num]]
rolling_avg_3_tbl_nested$data[[3]] %>% 
    tidy_loess()


# 4.5 Step 3: Map Function to All Categories ----

# Map Functions 

loess_tbl_nested <- rolling_avg_3_tbl_nested %>% 
    mutate(fitted = data %>% map(tidy_loess))

loess_tbl_nested$fitted[[1]]

loess_tbl_nested %>% 
    unnest()

# Visualize Results 

loess_tbl_nested %>% 
    unnest() %>% 
    
    ggplot(aes(x = month_end, y = total_price, color = category_2)) + 
    
    geom_point() +
    geom_line(aes(y = .fitted), color = 'blue', size = 2) +
    geom_smooth(method = 'loess', span =0.2, se = FALSE) +
    facet_wrap(~ category_2, scales = 'free_y')


