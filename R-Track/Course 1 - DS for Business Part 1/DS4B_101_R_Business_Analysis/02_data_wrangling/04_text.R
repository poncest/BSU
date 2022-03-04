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

value <- 1e6

1e6 %>% scales::number(prefix = '$')

(value/1e6) %>% scales::number(prefix = '$', suffix = "M")

value %>% scales::number(prefix = '$', big.mark = ',')

value %>% scales::dollar()

value %>% scales::dollar(scale = 1/1e6, suffix = 'M')


# percents

pct <-  0.15

pct %>% scales::number(scale = 100, suffix = '%')

pct %>% scales::percent()


# 1.7 Formatting Column Names ----

# Replacing text in column names

bike_orderlines_tbl %>% 
    set_names(names(.) %>% str_replace('_', '.') %>% toupper())

# Appending text to column names
bike_orderlines_tbl %>% 
    set_names(str_glue('{names(.)}_bike')) 

# Appending text to specific column names
bike_orderline_colnames_tbl <- bike_orderlines_tbl %>% 
    
    rename_at(.vars = vars(model:frame_material), 
              .funs = ~ str_c('prod_', .)) %>% 
    
    rename_at(.vars = vars(bikeshop_name:state), 
              .funs = ~ str_c('cust_', .)) %>% 
    
    glimpse()


bike_orderline_colnames_tbl %>% 
    select(contains('prod_'), total_price)

bike_orderline_colnames_tbl %>% 
    select(contains('cust_'), total_price)


# 2.0 Feature Engineering with Text -----
# Investigating "model" and extracting well-formatted features

bikes_tbl %>% 
    select(model) %>% 
    
    # fix typo
    mutate(model = case_when(
        model ==  'CAAD Disc Ultegra' ~ 'CAAD12 Disc Ultegra',
        TRUE ~ model)) %>% 
    
    # separate model col into more columns
    separate(col    = model, 
             into   = str_c('model_', 1:6),
             sep    = ' ', 
             remove = FALSE,
             fill   = 'right',
             extra  = 'drop') 
#%>% View()

