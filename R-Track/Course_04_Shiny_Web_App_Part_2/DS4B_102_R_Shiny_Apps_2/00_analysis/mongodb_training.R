# BUSINESS SCIENCE ----
# DS4B 202-R ----
# MONGO DB TRAINING -----
# Version 1

# LIBRARIES ----
library(mongolite) # Resource: https://jeroen.github.io/mongolite/
library(jsonlite)

library(config)

library(tidyverse)
library(lubridate)


# 1.0 CONNECTION TO REMOTE MONGODB ---- 

# Setup config Package & database YAML

#sys.getenv()
Sys.setenv(R_CONFIG_ACTIVE = "default")

config <- config::get(file = "R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/01_stock_analyzer_app/config.yml")
config

mongo_connect <- function(collection, database, 
                          hots     = config$host,
                          username = config$username,
                          password = config$password) {
    
    mongo(
        collection = collection,
        url        = str_glue("mongodb+srv://{username}:{password}@{host}/{database}")
    )
    
}


# Connect to MongoDB Atlas Cloud Database
mongo_connect(collection = "mtcars", database = "rstats")



# 2.0 ADD DATA ----

# Connect to collection
mongo_connection <- mongo_connect(collection = "mtcars", database = "rstats")

# Adding data
mtcars %>% 
    as_tibble(rownames = "model") %>% 
    mongo_connection$insert()

# 3.0 QUERYING DATA ----
mongo_connection$find()  # pull everything

mongo_connection$find(limit = 6) %>% 
    toJSON() %>% 
    prettify()

mongo_connection$find(query = '"{model": "Hornet Sportabout"}') %>% 
    as_tibble()


mongo_connection$count()


# 4.0 MODIFYING A COLLECTION ----

new_car_tbl <- tibble(
    model = "Ford F150",
    mpg   = 9.8,
    cyl   = 8,
    disp  = 275.8,
    hp    = 180,
    drat  = 3.07,
    wt    = 7.85,
    qsec  = 23.45,
    vs    = 0,
    am    = 1,
    gear  = 4,
    carb  = 3
)

new_car_tbl

# 4.1 Insert New Record
mongo_connection$insert(new_car_tbl)

mongo_connection$count()

mongo_connection$find(query = '{"model": "Ford F150"}')

tibble(
    model = "Ford F150"
) %>% 
    toJSON() %>% 
    str_remove_all(pattern = "^\\[|\\]$") %>%  # remove first and last square brackets
    prettify() %>% 
    mongo_connection$find(query = .) %>% 
    as_tibble()


# 4.2 Change a Record

mongo_connection$update(query  = '{"model": "Ford F150"}',
                        update = '{"$set" : {"mpg" : 10.8} }')

# not updated because F250 does not exist in the dataset
mongo_connection$update(query  = '{"model": "Ford F250"}',
                        update = '{"$set" : {"mpg" : 10.8} }')

mongo_connection$find(query = '{"model": "Ford F250"}')


# If a record does not match, add a new record
mongo_connection$update(query  = '{"model": "Ford F250"}',
                        update = '{"$set" : {"mpg" : 10.8} }',
                        upsert = TRUE)

mongo_connection$find(query = '{"model": "Ford F250"}')

mongo_connection$count()

mongo_connection$find() %>% as_tibble() %>% tail()



# 4.3 Remove a record

mongo_connection$remove(query = '{"model": "Ford F250"}')

mongo_connection$find() %>% as_tibble() %>% tail()

# 4.4 Remove entire table (be careful)

# mongo_connection$drop()


# 4.5 Disconnecting from Database ----

mongo_connection$disconnect()
    
# 5.0 NESTED STRUCTURES ----

mongo_connection <- mongo_connect(
    database   = "stock_analyzer",
    collection = "user_base"
)

mongo_connection$drop()
mongo_connection$count()

user_base_tbl <- tibble(
    user           = c("user1", "user2"),
    password       = c("pass1", "pass2"), 
    permissions    = c("admin", "standard"),
    name           = c("User One", "User Two"),
    favorites      = list(c("AAPL", "GOOG", "NFLX"), c("MA", "V", "FB")),
    last_symbol    = c("GOOG", "NFLX"),
    user_settings  = list(tibble(moving_avg_short = 20, moving_avg_long = 50, time_window = 180), 
                          tibble(moving_avg_short = 30, moving_avg_long = 90, time_window = 365)),
    account_created = c(ymd_hms("2019-05-12 12:31:09"), ymd_hms("2019-06-04 06:18:02"))
) 


# Converting to JSON

user_base_tbl %>% toJSON()

user_base_tbl %>% toJSON() %>% prettify()

user_base_tbl %>% toJSON(POSIXt = "mongo") %>% prettify()


# Adding nested structure to mongodb

mongo_connection$insert(user_base_tbl)


# Retrieve - Preserves nested structure and format

mongo_connection$find() %>% as_tibble() 


# 6.0 STOCK ANALYZER APP - CRUD WORKFLOW -----

# Create new collection
mongo_connection <- mongo_connect(
    database   = "stock_analyzer",
    collection = "user_base_test"
)

mongo_connection$drop()
mongo_connection$count()
mongo_connection$find()


# 6.1 Add User Data ----

mongo_connection$insert(user_base_tbl)


# 6.2 Get User Data ----
# read_user_base <- function() {
#     user_base_tbl <<- read_rds(path = "00_data_local/user_base_tbl.rds")
# }


mongo_read_user_base <- function(database = "stock_analyzer", collection = "user_base_test") {
    
    mongo_connection <- mongo_connect(
        database   = database,
        collection = collection,
        host       = config$host,
        username   = config$username,
        password   = config$password
        )
    
    user_base_tbl <<- mongo_connection$find() %>% as_tibble() # save `user_base_tbl` to our Global Environment
    
    mongo_connection$disconnect()
    
}

# testing

rm(user_base_tbl)

mongo_read_user_base()

mongo_read_user_base(database = "stock_analyzer", collection = "user_base")

# 6.3 What shinyauthr does... ----



# 6.5 Update Mongo ----

# update_and_write_user_base <- function(user_name, column_name, assign_input) {
#     user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
#     write_rds(user_base_tbl, path = "00_data_local/user_base_tbl.rds")
# }



# Before update


# After update


# 7.0 Save Functions ----

dump(c("mongo_connect", "mongo_get_user_base", "mongo_update_user_record"), 
     file = "00_scripts/crud_operations_mongodb.R", append = FALSE)


