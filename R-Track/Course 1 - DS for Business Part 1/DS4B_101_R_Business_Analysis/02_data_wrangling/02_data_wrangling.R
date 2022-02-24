# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# DATA WRANGLING OVERVIEW ----


library(tidyverse)
library(readxl)

bikes_tbl <- read_excel("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx")

orderlines_tbl <- read_excel("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/orderlines.xlsx")

bike_orderlines_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bikes_tbl)
glimpse(orderlines_tbl)
glimpse(bike_orderlines_tbl)

# 1.0 Selecting Columns with select() ----

# select function
bike_orderlines_tbl %>% 
select(order_date, order_id, order_line)

bike_orderlines_tbl %>% 
    select(1:3)

bike_orderlines_tbl %>% 
    select(starts_with('order_'))

# reduce columns for a visualization
bike_orderlines_tbl %>% 
    select(order_date, total_price, category_1, category_2)
    
# rearrange columns
bike_orderlines_tbl %>% 
    select(bikeshop_name:state, everything())
    
# select helpers
bike_orderlines_tbl %>% 
    select(contains('price'))

bike_orderlines_tbl %>% 
    select(ends_with('price'))

bike_orderlines_tbl %>% 
    select(starts_with('price'))


# pull function
bike_orderlines_tbl %>% 
    select(total_price) %>% 
    pull() %>% 
    mean()

bike_orderlines_tbl %>% 
    pull(model) 
    

# select_if()
bike_orderlines_tbl %>% 
    select_if(is.character)

bike_orderlines_tbl %>% 
    select_if(is.numeric)   

# returns everything that is not numeric
bike_orderlines_tbl %>% 
    select_if(~ !is.numeric(.))



# 2.0 Arranging with arrange() and desc() ----

bikes_tbl %>% 
    select(model, price) %>% 
    arrange(price)

bikes_tbl %>% 
    select(model, price) %>% 
    arrange(desc(price))
 


# 3.0 Filtering Rows with filter() ----

# 3.1 filter(): formula filtering ----

bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > 1500)

bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > mean(price))

bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > 5000 | (price < 1000)) %>% 
    arrange(desc(price)) %>% 
    View()

bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > 6000,
           model %>% str_detect('Supersix')               )


# Filtering for one or more conditions exactly using == and %in%

bike_orderlines_tbl %>% 
    filter(category_2 %in% c("Over Mountain", 'Trail', 'Endurance Road'))
    
    
bike_orderlines_tbl %>% 
    filter(category_2 == "Over Mountain")
    
    
bike_orderlines_tbl %>% 
    filter(category_2 != "Over Mountain") 

# everything except...
bike_orderlines_tbl %>% 
    filter(!(category_2 %in% c("Over Mountain", 'Trail', 'Endurance Road')))
    

# 3.2 slice(): filtering with row number(s) ----

# top 5
bikes_tbl %>% 
    arrange(desc(price)) %>% 
    slice(1:5)
    
#  bottom 5 
bikes_tbl %>% 
    arrange(price) %>% 
    slice(1:5)

#  bottom 5 
bikes_tbl %>% 
    arrange(desc(price)) %>% 
    slice(93 :  97)

#  bottom 5 ???
bikes_tbl %>% 
    arrange(desc(price)) %>% 
    slice(nrow(.)-4 : nrow(.))


# 3.2 distinct(): Unique values ----

bike_orderlines_tbl %>% 
    distinct(category_1)

bike_orderlines_tbl %>% 
    distinct(category_1, category_2)

bike_orderlines_tbl %>% 
    distinct(bikeshop_name, city, state)


# 4.0 Adding Columns with mutate() ----

# adding column
bilke_orderlines_prices <-  bike_orderlines_tbl %>% 
    select(order_date, model, quantity, price) %>% 
    mutate(total_price = quantity * price)

# overwrite column
bilke_orderlines_prices %>% 
    mutate(total_price = log(total_price)) 

# transformations
bilke_orderlines_prices %>% 
    mutate(
        total_price = quantity * price,
        total_price_log = log(total_price),
        total_price_sqrt = total_price^0.5
        )

# adding flag (binary)
bilke_orderlines_prices %>% 
    mutate(is_supersix = model %>% str_to_lower() %>% str_detect('supersix')) %>% 
    filter(is_supersix)


# bining with ntile()
bilke_orderlines_prices %>% 
    mutate(total_price_bin = ntile(total_price, 3)) # high, low, medium
 

# case_when() - more flexible binning

# numeric to categorical
bilke_orderlines_prices %>% 
    mutate(total_price_bin = ntile(total_price, 3)) %>% 
    mutate(total_price_bin2 = case_when(
        total_price > quantile(total_price, 0.66) ~ 'High',  # prob .66 = 3rd quantile
        total_price > quantile(total_price, 0.33) ~ 'Medium',  
        TRUE ~ 'Low'  # catchall
    ))

# text to categorical
bilke_orderlines_prices %>% 
    mutate(bike_type = case_when(
      model %>% str_to_lower() %>% str_detect('supersix') ~ 'Supersix',
      model %>% str_to_lower() %>% str_detect('jekyll') ~ 'Jekyll',
      TRUE ~ 'Not Supersix or Jekyll'  # catchall (all others)
    ))


A# 5.0 Grouping & Summarizing with group_by() and summarize() ----




# 6.0 Renaming columns with rename() and set_names() ----

# 6.1 rename: One column at a time ----


# 6.2 set_names: All columns at once ---




# 7.0 Reshaping (Pivoting) Data with spread() and gather() ----

# 7.1 spread(): Long to Wide ----


# 7.2 gather(): Wide to Long ----




# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel) ----




# 9.0 Binding Data by Row or by Column with bind_rows() and bind_col() ----

# 9.1 bind_cols() ----




# 9.2 bind_rows() ----



