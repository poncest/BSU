# DATA BASICS ----

library(pacman)
p_load(tidyverse)

# Data Types ----

# concatenate vector
a <-  c(1,2,3)
class(a)

b <- c('low', 'medium', 'high')
class(b)


# Data Structure
ab_tbl <- tibble(a,b)

# load rds file
read_rds('00_data/bike_sales/data_wrangled_student/bike_orderlines.rds')
