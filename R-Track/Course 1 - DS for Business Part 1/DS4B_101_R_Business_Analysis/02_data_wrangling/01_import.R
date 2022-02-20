# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# IMPORTING DATA INTO R ----


# 1.0 Load libraries ----

# Contains readr
library(tidyverse)   

# Excel Connection
library(readxl)
library(writexl)

# Database Connection
library(odbc) 
library(RSQLite)

# 2.0 readr ----

# 2.1 CSV ----
bike_orders_csv_tbl <- readr::read_csv('R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.csv')

# which rows has problems - No issues since read_csv was updated (improved)
readr::problems(bike_orders_csv_tbl)

bike_orders_csv_tbl %>% 
    slice(7916)

# specifying column type
read_csv('R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.csv', 
        col_types = cols(
            order_id = col_double()
        ) ) %>% 
    slice(7916)


# 2.2 RDS ----

# preferred save method
bike_orders_rds_tbl <- readr::read_rds('R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds')

bike_orders_rds_tbl %>% 
    slice(7916)


# 3.0 Excel ----

# to see how many sheets the file has
readxl::excel_sheets('R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.xlsx')

bike_orders_excel_tbl <-readxl::read_excel('R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.xlsx', sheet = 'Sheet1')



# 4.0 Databases  ----

# using Chinook data (sqlite)

# connection
con <- RSQLite::dbConnect(
    drv = SQLite(),
    dbname = 'R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/chinook/Chinook_Sqlite.sqlite')

# listing the tables available
dbListTables(con)

# examining the tables - we're viewing only
tbl(con, 'Album')

# if we need to pull the table to your local memory
tbl(con, 'Album') %>% collect()

# saving
album_tbl <- tbl(con, 'Album') %>% collect()
artist_tbl <- tbl(con, 'Artist') %>% collect()

# disconnecting from db
dbDisconnect(con)

con




