# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Date & Lubridate Basics ----

# 1.1 Character vs Date/Datetime
order_date_tbl <- bike_orderlines_tbl %>% 
    select(order_date)

order_date_tbl %>% 
    pull(order_date) %>% 
    head(12)

order_date_tbl %>% 
    pull(order_date) %>% 
    class()

order_date_tbl %>% 
    mutate(
        order_date_chr = as.character(order_date),
        order_date_chr2 = order_date_chr %>% str_c('00:00:00')
    ) %>% 
    
    # 1.2 Date Classes
    mutate(
        order_date_date = ymd(order_date_chr),
        order_date_date_dttm = ymd_hms(order_date_chr2)
    )


# 1.3 Lubridate Functions

# Conversion

# mm/dd/yy
'06/01/18' %>%  mdy()

# time stamp
'06/01/18 12:30,15' %>%  mdy_hms()

'January 1, 1985'%>%  mdy()


# Extractor

# yyyy/mm/dd
'2011-01-01' %>% ymd() %>% year()
'2011-01-01' %>% ymd() %>% month(label = TRUE, abbr = FALSE)
'2011-01-01' %>% ymd() %>% day()
'2011-01-01' %>% ymd() %>% wday(label = TRUE, abbr = FALSE)


# Helpers

now()
today()


# Periods & Durations - Add/subract time to/from a date

today() + days(17)

# duration
today() + ddays(17)

# next leap year will be 29 Feb 2024
today() + years(3) # Period, accounts for leap years

today() + dyears(3) # Duration, does NOT accounts for leap years

# Intervals - Calculate time-based distance 

# not what we want
i <- interval(today(), today() + ddays(12))
class(i)

# how many days in interval
i / ddays(1)  # interval / days 

# how many hours in interval
i / dhours(1)  # interval / hours

# how many minutes in interval
i / dminutes(1)  # interval / minutes 


order_date_tbl %>% 
    mutate(
        today = today(),
        diff_days = interval(order_date, today) / ddays(1)
    )

# 2.0 Time-Based Data Grouping ----

# year
bike_sales_y_tbl <- bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    
    # lubridate
    mutate(
        order_date = ymd(order_date),
        year = year(order_date)
    ) %>% 
    
    # dplyr
    group_by(year) %>% 
    summarize(
        sales = sum(total_price)
    ) %>% 
    ungroup()
    
# month   
bike_sales_m_tbl <-  bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    
    # lubridate
    mutate(
        order_date = ymd(order_date),
        year = year(order_date),
        month = month(order_date, label = TRUE)
    ) %>% 
    
    # dplyr
    group_by(year, month) %>% 
    summarize(
        sales = sum(total_price)
    ) %>% 
    ungroup()

# floor date (grouping)

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>% 
    
    # lubridate
    mutate(
        order_date = ymd(order_date),
        year_month = floor_date(order_date, unit = 'month')
    ) %>% 
    
    # group_by + summarize
    group_by(year_month) %>% 
    summarize(
        sales = sum(total_price)
    ) %>% 
    ungroup()
    


# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----

# we're going to calculate the % diff
bike_sales_y_tbl %>% 
    mutate(
        sales_lag_1 = lag(sales, n = 1)
    ) %>% 
    
    # handling NA's
    mutate(
        sales_lag_1 = case_when(
            is.na(sales_lag_1) ~ sales, # if NA, return sales value
            TRUE ~ sales_lag_1) # otherwise, return sales lag value
    ) %>% 
    
    # diff's and % diff
    mutate(
        diff_1 = sales - sales_lag_1,
        pct_diff_1 = (diff_1 / sales_lag_1), 
        pct_diff_1_chr = scales::percent(pct_diff_1)
    )

# function to calculate pct diff
# notice it is harcoded for `sales`

calculate_pct_diff <- function(data){
    
    data %>% mutate(
        sales_lag_1 = lag(sales, n = 1)
    ) %>% 
        
        # handling NA's
        mutate(
            sales_lag_1 = case_when(
                is.na(sales_lag_1) ~ sales, # if NA, return sales value
                TRUE ~ sales_lag_1) # otherwise, return sales lag value
        ) %>% 
        
        # diff's and % diff
        mutate(
            diff_1 = sales - sales_lag_1,
            pct_diff_1 = (diff_1 / sales_lag_1), 
            pct_diff_1_chr = scales::percent(pct_diff_1)
        )
}

# using the function
bike_sales_m_tbl %>% 
    calculate_pct_diff()


# 3.2 Difference from first observation ----

bike_sales_y_tbl %>% 
    mutate(
        sales_2011 = first(sales),
        diff_2011 = sales - sales_2011,
        pct_diff_2011 = diff_2011 / sales_2011,
        pct_diff_2011_chr = scales::percent(pct_diff_2011)
    )
 





# 4.0 Cumulative Calculations ----




# 5.0 Rolling Calculations ----



# 6.0 Filtering Date Ranges ---- 



