# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CATEGORICAL DATA MANIPULATION ----

library(tidyverse)
library(tidyquant)

bike_orderlines_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl
 


# 1.0 Factor Basics ----

# What is a Factor?
# A way of managing categorical data

# Why do we want factors? 
# 1. Can group numeric values into bin (think price = low, medium, high)
# 2. Can reorder categories for visualization (fct_reorder)
# 3. Can manipulate categories much easier (fct_lump)
# 4. Machine learning and modeling algorithms may require factor data type for categorical data. 

# 2.0 Motivating Example -----

# Manipulation

sales_by_cat_2_tbl <-  bike_orderlines_tbl %>% 
    select(category_2, total_price) %>% 
    group_by(category_2) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% 
    arrange(desc(sales))
 

# Plotting

# the order (desc sales) is not preserved -  they're in alph order
sales_by_cat_2_tbl %>% 
    ggplot(aes(x = sales, y = category_2)) +
    geom_point(size = 5) +
    theme_tq()

# how to fix it?
sales_by_cat_2_tbl <-  bike_orderlines_tbl %>% 
    select(category_2, total_price) %>% 
    group_by(category_2) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% 
    arrange(desc(sales)) %>% 
    mutate(category_2 = category_2 %>% 
               as_factor() %>% fct_rev())

# now the order is preserved
sales_by_cat_2_tbl %>% 
    ggplot(aes(x = sales, y = category_2)) +
    geom_point(size = 5, color = '#2C3E50') +
    labs(title = 'Sales Category 2') +
    scale_x_continuous(labels = scales::dollar_format()) +
    expand_limits(x = 0) +
    theme_tq()

# function to create a base plot
plot_sales <- function(data){
    data %>% 
        ggplot(aes(x = sales, y = category_2)) +
        geom_point(size = 5, color = '#2C3E50') +
        labs(title = 'Sales Category 2') +
        scale_x_continuous(labels = scales::dollar_format()) +
        expand_limits(x = 0) +
        theme_tq()
}

sales_by_cat_2_tbl %>% 
    plot_sales()

# 3.0 Forcats Basics ---- 


# 3.1 Inspecting Factors ----

# Vector


# Tibble



# 3.2 Creating Factors: as_factor() vs as.factor() ----





# 3.3 Reording Factors: fct_reorder() and fct_rev() ----





# 3.4 Time-Based Reordering: fct_reorder2() ----





# 3.5 Creating "Other" Category - fct_lump() & fct_shift() ----





