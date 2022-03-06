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
sales_by_cat_2_tbl %>% 
    pull(category_2) %>% 
    levels()

sales_by_cat_2_tbl %>% 
    pull(category_2) %>% 
    as.numeric()


# Tibble
sales_by_cat_2_tbl %>% 
    mutate(
        category_2 = category_2 %>% fct_rev(),
        label = category_2 %>% as.character(),
        value = category_2 %>% as.numeric()
    )


# 3.2 Creating Factors: as_factor() vs as.factor() ----
sales_by_cat_2_tbl %>% 
    mutate(
        category_2 =  as.character(category_2),
        category_2_as_factor = as_factor(category_2) %>% as.numeric(),
        category_2_as.factor = as.factor(category_2) %>% as.numeric()
    )


# 3.3 Reording Factors: fct_reorder() and fct_rev() ----
sales_by_cat_2_tbl %>% 
    arrange(desc(sales)) %>% 
    
    mutate(
        sales_neg  = -sales,
        category_2 = category_2 %>% fct_reorder(sales_neg),
        values     = category_2 %>% as.numeric() 
    ) %>% 
    
    plot_sales()



# 3.4 Time-Based Reordering: fct_reorder2() ----

sales_by_cat_2_qtr_tbl <- bike_orderlines_tbl %>% 
    
    # data by quarters
    mutate(
        order_date = order_date %>% floor_date('quarter') %>% ymd()
    ) %>% 
    
    group_by(category_2, order_date) %>%
    summarise(sales = sum(total_price)) %>% 
    ungroup()


sales_by_cat_2_qtr_tbl %>% 
    
    # order by sales from High to Low
    mutate(
        categrory_2 = category_2 %>% fct_reorder2(order_date, sales)
    ) %>% 
    
    ggplot(aes( x= order_date, y = sales, color = category_2))+
    geom_point() +
    geom_line() + 
    facet_wrap(~ category_2) + 
    
    theme_tq() + 
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = 'M'))


# 3.5 Creating "Other" Category - fct_lump() & fct_relevel() ----
sales_by_cat_2_tbl %>% 
    
    mutate(
        # take the first 6 and weight them by sales
        category_2 = category_2 %>% 
            fct_lump(n = 6, 
                     w = sales, 
                     other_level = 'All Other Bike Categories')
    ) %>% 
    
    group_by(category_2) %>% 
    summarise(sales = sum(sales)) %>% 
    
    # fixing the order of `all other bikes cat`
    # after = 0, moves `all other bikes cat` to the front
    mutate(category_2 = category_2 %>% 
               fct_relevel('All Other Bike Categories', after = 0)) %>% 
    
    plot_sales()
 




