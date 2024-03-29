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
unit_price_cat_2_tbl <-  bike_orderlines_tbl %>% 
    select(category_2, model, price) %>% 
    distinct() %>% 
    
    # reoder factors
    mutate(
        category_2 = as.factor(category_2) %>% fct_reorder(price)
    ) 
    

# Box Plot
unit_price_cat_2_tbl %>% 
    ggplot(aes(x = category_2,
               y = price)) +
    geom_boxplot() +
    coord_flip() +
    theme_tq()

# Rain Plot
library(gghalves)

revenue_by_cat2_fct_tbl   <- bike_orderlines_tbl %>% 
    select(category_2, model, price) %>% 
    distinct() %>% 
    mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))


ggplot(data = revenue_by_cat2_fct_tbl, aes(x = category_2,
                                           y = price)) +
    geom_half_boxplot(side = "r", fill = "#2e3c50", color = "gray80") +
    geom_half_point(side = "l", alpha = .5) +
    coord_flip() +
    theme_tq()


# Violin Plot & Jitter Plot
unit_price_cat_2_tbl %>% 
    ggplot(aes(x = category_2,
               y = price)) +
    
    geom_jitter(width = 0.15, color = '#2C3E50') + 
    geom_violin(alpha = 0.5)+
    coord_flip() + 
    theme_tq()


# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    group_by(year) %>% 
    summarise(
        revenue = sum(total_price)
    ) %>% 
    ungroup()


# Adding text to bar chart
# Filtering labels to highlight a point

revenue_by_year_tbl %>% 
    
    # bar labels
    mutate(revenue_text = scales::dollar(revenue, scale = 1e-6, suffix = 'M')) %>% 

    ggplot(aes(x = year, y = revenue)) +
    geom_col(fill = '#2C3E50') + 
    geom_smooth(method = 'lm', se = FALSE) +
    
    geom_text(aes(label = revenue_text),
                  vjust = 1.5, color = 'snow') + 
    
    geom_label(data = revenue_by_year_tbl %>% filter(year %in% c(2013)),
               label = 'Major Demand This Year',
               size = 5,
               fontface = 'bold',
               fill = '#1F78b4',
               color = 'white',
               vjust = -0.5) +
    
    expand_limits(y = 2e7)+
    
    theme_tq() 



