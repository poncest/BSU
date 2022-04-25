# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# INTERACTIVE PLOTS ----

# GOAL: DEVELOP INTERACTIVE PLOTS FOR A SALES REPORT


# LIBRARIES & DATA ----

# Main
library(tidyverse)
library(lubridate)
 
# Visualization
library(tidyquant)
library(plotly)


path_bike_orderlines <- "R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds"

bike_orderlines_tbl <- read_rds(path_bike_orderlines) 

bike_orderlines_tbl

# 1.0 TOTAL SALES BY MONTH ----

# 1.1 Preparing Time Series Data ----
total_sales_m_tbl <- bike_orderlines_tbl %>% 
    
    select(order_date, total_price) %>% 
    
    # round down the month
    mutate(date_rounded = floor_date(order_date, unit = 'month')) %>% 
    
    group_by(date_rounded) %>% 
    summarise(total_sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    # text labels
    mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)},
                                 Date: {date_rounded %>% format('%B %Y')}"))

total_sales_m_tbl

# Formatting Dates
# - strftime: https://devhints.io/strftime

'2011-01-07 00:00:00' %>% 
    as_datetime() %>% 
    format('%B %Y') 

?format

# 1.2 Interactive Plot ----

# Step 1: Create ggplot with text feature

# Static Plot
g1 <- total_sales_m_tbl %>% 
    ggplot(aes(x = date_rounded, y = total_sales)) +
    
    # geoms
    geom_point(aes(text = label_text), color = '#2C3E50') +
    geom_smooth(method = 'loess', span = 0.2) +
    
    # format
    theme_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    # always show y-axis at 0 for timeseries
    expand_limits(y = 0) +
    
    # labels
    labs(
        title = 'Total Sales',
        x = '',
        y = 'Revenue (USD)'
    )

g1

# Step 2: Use ggplotly()

# interactive
ggplotly(g1, tooltip = 'text')


# 1.3 Plot Total Sales Function ----

plot_total_sales <- function(unit = 'weekly', date_format = '%B %Y', interactive = TRUE) {
    
    # handle data
    data_tbl <- bike_orderlines_tbl %>% 
        
        select(order_date, total_price) %>% 
        
        # round down the month
        mutate(date_rounded = floor_date(order_date, unit = unit)) %>% 
        
        group_by(date_rounded) %>% 
        summarise(total_sales = sum(total_price)) %>% 
        ungroup() %>% 
        
        # text labels
        mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)},
                                 Date: {date_rounded %>% format(date_format)}"))
    
    
    # make the plot
    g1 <- data_tbl %>% 
        ggplot(aes(x = date_rounded, y = total_sales)) +
        
        # geoms
        geom_point(aes(text = label_text), color = '#2C3E50') +
        geom_smooth(method = 'loess', span = 0.2) +
        
        # format
        theme_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        # always show y-axis at 0 for timeseries
        expand_limits(y = 0) +
        
        # labels
        labs(
            title = 'Total Sales',
            x = '',
            y = 'Revenue (USD)'
        )
    
    
    # static vs interactive logic
    if (interactive) {
        return(ggplotly(g1, tooltip = 'text'))  # interactive
    } else{
       return(g1)                               # static
    } 
    
} 

plot_total_sales(unit = 'week', date_format = '%B %d, %Y', interactive = TRUE)


# 1.4 Test Our Function ----
plot_total_sales(unit = 'day', date_format = '%B %d, %Y', interactive = TRUE)
plot_total_sales(unit = 'week', date_format = '%B %d, %Y', interactive = TRUE)
plot_total_sales(unit = 'month', date_format = '%B %d, %Y', interactive = TRUE)
plot_total_sales(unit = 'quarter', date_format = '%B %d, %Y', interactive = TRUE)
plot_total_sales(unit = 'year', date_format = '%B %d, %Y', interactive = TRUE)


# 2.0 CATEGORY 2 SALES BY MONTH ----

# 2.1 Preparing Time Series Data ----

category_2_sales_m_tbl <- bike_orderlines_tbl %>% 
    select(order_date, category_1, category_2, total_price) %>% 
    mutate(date_rounded = floor_date(order_date, unit = 'month')) %>% 
    
    group_by(date_rounded, category_1, category_2) %>% 
    summarise(total_sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)}
                                 Date: {date_rounded %>%  format('%B %Y')}")) %>% 
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(date_rounded, total_sales))
 
# 2.2 Interactive Plot ----

# Step 1: Create
category_2_sales_m_tbl 


# Step 2: Use ggplotly()



# 2.3 Plot Categories Function ----



# 2.4 Test Our Function ----




# 3.0 SAVE FUNCTIONS ----






