# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis ----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)


# 2.0 Importing Files ----

bikes_tbl <- read_excel(path = 'R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx')
bikeshop_tbl <- read_excel(path ='R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikeshops.xlsx')
orderlines_tbl <- read_excel(path ='R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/orderlines.xlsx')

# 3.0 Examining Data ----

glimpse(bikes_tbl)
glimpse(bikeshop_tbl)
glimpse(orderlines_tbl)

# 4.0 Joining Data ----

# method 1 
left_join(orderlines_tbl, bikes_tbl, by = c('product.id' = 'bike.id'))

# method 2 and prefer method!
bike_orderlines_join_tbl <- orderlines_tbl %>% 
    left_join(bikes_tbl, by = c('product.id' = 'bike.id')) %>% 
    left_join(bikeshop_tbl, by = c('customer.id' = 'bikeshop.id'))

glimpse(bike_orderlines_join_tbl)


# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_join_tbl %>% 
    
    # separate description into cat 1, cat 2, and frame
    separate(description, 
             into = c('category.1', 'category.2', 'frame.material'),
             sep = ' - ',
             remove = TRUE) %>% 
    
    # separate location into city and state
    separate(location, 
             into = c('city', 'state'),
             sep = ', ',
             remove = FALSE) %>% 
    
    mutate(total.price = price * quantity) %>% 
    
    # reorganize, remove unnecessary columns
    select(-...1, -location) %>% 
    select(-ends_with('.id')) %>% 
    
    # add the order.id back to the df
    bind_cols(bike_orderlines_join_tbl %>% select(order.id)) %>% 
    
    # reorder columns
    select(contains('date'), contains('id'), contains('order'),
           quantity, price, total.price,
           everything()) %>% 
    
    # rename columns
    set_names(names(.) %>% str_replace_all('\\.', '_'))


glimpse(bike_orderlines_join_tbl)

names(bike_orderlines_join_tbl)



# 6.0 Business Insights ----

# 6.1 Sales by Year ----

# Step 1 - Manipulate

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
    
    # selecting columns to focus on and adding a year column
    select(order_date, total_price) %>% 
    mutate(year = year(order_date)) %>% 
    
    # group by year and summarizing sales
    group_by(year) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    # format sales values to text ($)
    mutate(sales_text = scales::dollar(sales))


# Step 2 - Visualize

sales_by_year_tbl %>% 
    ggplot(aes(x = year, y = sales)) + 
    
    # geometries
    geom_col(fill = '#2C3E50') + #palette_light() 
    geom_label(aes(label = sales_text)) + 
    geom_smooth(method = 'lm', se = FALSE) + 
    
    # Formatting
    theme_tq() + 
    scale_y_continuous(label = scales::dollar) +
    
    # labels
    labs(
        title = 'Overall Revenue by Year',
        subtitle = 'Upward Trend',
        x = '',
        y = 'Revenue'
    )
    


# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>% 
    
    # selecting the columns and add a year column
    select(order_date, total_price, category_2) %>% 
    mutate(year = year(order_date)) %>% 

    # group by and summarize on the year and cat2
    group_by(year, category_2) %>% 
    summarize(sales = sum(total_price)) %>% 
    ungroup() %>% 
    
    # format $ text
    mutate(sales_text = scales::dollar(sales))  


# Step 2 - Visualize
sales_by_year_cat_2_tbl %>% 
    ggplot(aes(x = year,
               y = sales,
               fill = category_2)) +
    
    # geometries
    geom_col() + 
    geom_smooth(method = 'lm', se = FALSE) +
    
    # facet
    facet_wrap(~ category_2, ncol = 3,
               scales = 'free_y') +  ### individual y-axis 
    
    # format
    theme_tq() + 
    scale_fill_tq() +
    scale_y_continuous(labels = scales::dollar) +
    
    
    #labels
    labs(
        title = 'Revenue by Year and Category 2',
        subtitle = 'Each product category has an upward trend',
        x = '',
        y = 'Revenue',
        # rename legend title
        fill = 'Product Secondary Category'
    )
    


# 7.0 Writing Files ----

# creating a new folder - `data_wrangled_student`
fs::dir_create('00_data/bike_sales/data_wrangled_student')

# 7.1 Excel ----
bike_orderlines_wrangled_tbl %>% 
    write_xlsx('00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx')
    

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
    write_csv('00_data/bike_sales/data_wrangled_student/bike_orderlines.csv')

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
    write_rds('00_data/bike_sales/data_wrangled_student/bike_orderlines.rds')
