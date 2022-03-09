# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")


glimpse(bike_orderlines_tbl)


# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>% 
    
    select(order_id, order_line, total_price, quantity) %>% 
    group_by(order_id) %>% 
    summarise(
        total_quantity = sum(quantity),
        total_price = sum(total_price)
    ) %>% 
    ungroup()


# Scatter Plot
order_value_tbl %>% 
    ggplot(aes(x = total_quantity,
               y = total_price)) + 
    
    geom_point(size = 2, alpha = 0.5) + 
    geom_smooth(method = 'lm', se = FALSE) + 
    
    theme_classic()


# 2.0 Line Plots ----
# - Great for time series

# Goal: Describe revenue by Month, expose cyclic nature

# Data Manipulation

revenue_month_tbl <- bike_orderlines_tbl %>% 
    
    select(order_date, total_price) %>% 
    
    mutate(
        year_month = floor_date(order_date, unit = 'month') %>% ymd()
    ) %>% 
    group_by(year_month) %>% 

    summarise(
        revenue = sum(total_price),
    ) %>% 
    
    ungroup()

# Line Plot
revenue_month_tbl %>% 
    ggplot(aes(x = year_month,
               y = revenue)) + 
    
    geom_line(linetype = 1, size = .5) + 
    geom_smooth(method = 'loess', span = 0.2) + 
    theme_classic()
    
 


# 3.0 Bar / Column Plots ----
# - Great for categories

# Goal: Sales by Descriptive Category

# Data Manipulation


# Bar Plot






# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable


# Goal: Unit price of bicycles
# Histogram


# Goal: Unit price of bicylce, segmenting by frame material
# Histogram


# Density




# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions


# Goal: Unit price of models, segmenting by category 2

# Data Manipulation


# Box Plot


# Violin Plot & Jitter Plot







# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation


# Adding text to bar chart


# Filtering labels to highlight a point





