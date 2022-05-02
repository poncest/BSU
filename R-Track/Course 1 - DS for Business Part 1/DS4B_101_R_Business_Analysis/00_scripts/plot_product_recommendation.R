get_bike_features <-
function() {
    
    bike_features_tbl <- bike_orderlines_tbl %>% 
        
        select(price,model, category_1, category_2, frame_material) %>% 
        
        distinct() %>% 
        
        mutate(id = row_number()) %>% 
        
        select(id, everything()) %>% 
        
        separate_bike_model(keep_model_column = TRUE, append = TRUE)
        
    return(bike_features_tbl)
}
plot_bike_features <-
function(interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
}
