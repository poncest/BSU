# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TEXT MANIPULATION ----

library(tidyverse)
library(lubridate)


bike_orderlines_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl

bikes_tbl <- readxl::read_excel("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx")

bikes_tbl

 
# 1.0 Basics ----

# 1.1 Detection: Used with filter() ----

# Vector
c('Supersix Evo Black Inc.', 'Supersix Evo Hi-Mod Team') %>% 
    str_detect(patter = 'Supersix')


# Tibble
bikes_tbl %>% 
    select(model) %>% 
    mutate(
        supersix = model %>% str_detect(patter = 'Supersix') %>% 
            as.numeric(),
        
        black = model %>% str_detect(patter = 'Black') %>% 
            as.numeric(),
    )


# 1.2 Case & Concatenation ----

# Case
bikeshop_name <- 'Ithaca Mountain Climbers'

str_to_lower(bikeshop_name)
str_to_upper(bikeshop_name)
str_to_title(bikeshop_name)


# Concatenation

# Vector
order_id <- 1
order_line <- 1

# sentence
str_c('Order Line: ', order_id, '.' , order_line,
      ' sent to Customer: ', bikeshop_name)

str_glue("Order Line: {order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}")



# Tibble
bike_orderlines_tbl %>% 
    select(bikeshop_name, order_id, order_line) %>% 
    mutate(
        purchase_statement = str_glue(
            "Order Line: {order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}") %>% 
            as.character()
    )

# 1.3 Separating Text: See tidyr::separate() ----

# Vector
c("Road - Elite Road - Carbon - Road - Elite Road") %>% 
    str_split(pattern = ' - ')


# better - return as a matrix
c("Road - Elite Road - Carbon - Road - Elite Road") %>% 
    str_split(pattern = ' - ', simplify = TRUE)


# Tibble
# much better option
bikes_tbl %>% 
    select(description) %>% 
    separate(col    = description, 
             into   = c('category_1', 'category_2', 'frame_material'),
             sep    = ' - ',
             remove = FALSE)


# 1.4 Trimming Text ----

'  text with spaces   ' %>%  str_trim(side = 'both')
'  text with spaces   ' %>%  str_trim(side = 'right')
'  text with spaces   ' %>%  str_trim(side = 'left')


# 1.5 Replacement: Used with mutate() [and optionally case_when()] ----

# Vector
c('CAAD12', 'CAAD', 'CAAD8') %>% 
    # replace all numbers with ''
    str_replace(pattern     = '[0-9]+',
                replacement = '')


# Tibble
bikes_tbl %>% 
    select(model) %>% 
    mutate(
        model_num_removed = str_replace(string      = model,
                                        pattern     = '[0-9]+',
                                        replacement = '') %>% 
            str_trim()
    ) 
    



# 1.6 Formatting Numbers ----

# values


# percents



# 1.7 Formatting Column Names ----

# Replacing text in column names



# Appending text to column names


# Appending text to specific column names


# 2.0 Feature Engineering with Text -----
# Investigating "model" and extracting well-formatted features

