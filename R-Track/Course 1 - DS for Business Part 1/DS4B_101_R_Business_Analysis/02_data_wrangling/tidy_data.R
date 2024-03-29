# TIDY DATA EXAMPLE ----

library(tidyverse)
library(readxl)


bikeshop_revenue_wide_tbl <- read_excel("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/02_data_wrangling/bikeshop_revenue_formatted_wide.xlsx")

# Wide Format ----

bikeshop_revenue_wide_tbl


# Long Format ----

bikeshop_revenue_long_tbl <- bikeshop_revenue_wide_tbl %>%
    select(-Total) %>%
    gather(key = "category_1", value = "sales", Mountain, Road)

bikeshop_revenue_long_tbl


# Analyze

model <- lm(sales ~ ., data = bikeshop_revenue_long_tbl)

model 
