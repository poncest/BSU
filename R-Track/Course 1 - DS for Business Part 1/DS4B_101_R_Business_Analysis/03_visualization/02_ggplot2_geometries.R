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

revenue_by_cat2_tbl <- bike_orderlines_tbl %>% 
    
    select(category_2, total_price) %>% 
    group_by(category_2) %>% 
    summarise(revenue = sum(total_price)) %>% 
    ungroup()


# Bar Plot
revenue_by_cat2_tbl %>% 
    # sort by revenue
    mutate(category_2 = category_2 %>%  as_factor() %>% 
               fct_reorder(revenue)) %>% 
    
    ggplot(aes(x = category_2,
               y = revenue)) +
    
    geom_col()+
    coord_flip()+
    theme_classic()




# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable

# Goal: Unit price of bicycles
# Histogram

bike_orderlines_tbl %>% 
    select(model, price) %>% 
    ggplot(aes(x = price)) +
    geom_histogram(bins = 20, fill = '#5c9f92', color = 'snow')+
    theme_classic() 

# Goal: Unit price of bicycle, segmenting by frame material

# Histogram
bike_orderlines_tbl %>% 
    distinct(model, frame_material, price) %>% 
    ggplot(aes(x = price, fill = frame_material)) +
    geom_histogram(bins = 20)+
    facet_wrap(~ frame_material, ncol = 1) +
    scale_fill_tq()+
    theme_tq()


# Density 
bike_orderlines_tbl %>% 
    distinct(model, frame_material, price) %>% 
    ggplot(aes(x = price, fill = frame_material)) +
    geom_density(alpha = 0.5)+
    #facet_wrap(~ frame_material, ncol = 1) +
    scale_fill_tq()+
    theme_tq() + 
    theme(legend.position = 'bottom')



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





