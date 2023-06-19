mongo_connect <-
function(collection, database, 
          hots     = config$host,
          username = config$username,
          password = config$password) {
    
    mongo(
        collection = collection,
        url        = str_glue("mongodb+srv://{username}:{password}@{host}/{database}")
    )
    
}
mongo_read_user_base <-
function(database = "stock_analyzer", collection = "user_base_test",
         hots     = config$host,
         username = config$username,
         password = config$password) {
    
    mongo_connection <- mongo_connect(
        database    = database,
        collection  = collection,
        host        = host, 
        username    = username, 
        password    = password 
    )
    
    user_base_tbl <<- mongo_connection$find() %>% as_tibble()
    
    mongo_connection$disconnect()
    
}
mongo_update_and_write_user_base <-
function(user_name, column_name, assign_input, 
         database    = "stock_analyzer",
         collection  = "user_base_test",
         host        = config$host, 
         username    = config$username, 
         password    = config$password
         ) {
    
    user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
    
    mongo_connection <- mongo_connect(
        database    = database,
        collection  = collection,
        host        = host, 
        username    = username, 
        password    = password
    )
    
    # Query String
    query_string <- str_c('{"user": "', user_name, '"}')
    
    # Update String
    update_string <- user_base_tbl %>%
        filter(user == user_name) %>%
        select(-user, -password, -permissions) %>%
        toJSON(POSIXt = "mongo") %>%
        str_remove_all(pattern = "^\\[|\\]$") 
    
    # Update
    mongo_connection$update(
        query  = query_string, 
        update = str_c('{"$set" : ', update_string, ' }') 
    )
    
    mongo_connection$disconnect()
}
