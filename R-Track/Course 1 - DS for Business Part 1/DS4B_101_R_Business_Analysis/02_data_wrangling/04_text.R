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


# Tibble



# 1.2 Case & Concatenation ----


# Case


# Concatenation

# Vector



# Tibble


# 1.3 Separating Text: See tidyr::separate() ----

# Vector


# Tibble



# 1.4 Trimming Text ----



# 1.5 Replacement: Used with mutate() [and optionally case_when()] ----

# Vector


# Tibble




# 1.6 Formatting Numbers ----

# values


# percents



# 1.7 Formatting Column Names ----

# Replacing text in column names



# Appending text to column names


# Appending text to specific column names


# 2.0 Feature Engineering with Text -----
# Investigating "model" and extracting well-formatted features

