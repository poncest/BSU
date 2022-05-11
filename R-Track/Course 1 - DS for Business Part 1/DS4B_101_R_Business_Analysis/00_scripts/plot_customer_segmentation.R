get_customer_segments <-
function(k = 4, seed = 123) {
    
    # 1.0 CUSTOMER TRENDS
    customer_trends_tbl <- bike_orderlines_tbl %>% 
        
        select(bikeshop_name, price, model, category_1, 
               category_2, frame_material, quantity) %>% 
        
        group_by_at(vars(bikeshop_name:frame_material)) %>% 
        summarise(total_qty = sum(quantity)) %>% 
        ungroup() %>% 
        
        group_by(bikeshop_name) %>% 
        mutate(pct = total_qty / sum(total_qty)) %>% 
        ungroup()
    
    
    customer_product_tbl <- customer_trends_tbl %>% 
        select(bikeshop_name, model, pct) %>% 
        spread(key = model, value = pct, fill = 0)
    
    # 2.0 MODELING: K-MEANS CLUSTERING
    set.seed(seed)
    kmeans_obj <- customer_product_tbl %>% 
        select(-bikeshop_name) %>% 
        kmeans(centers = k, nstart = 100)
    
    kmeans_tbl <- kmeans_obj %>% 
        augment(customer_product_tbl) %>% 
        select(bikeshop_name, .cluster)
    
    # 3.0 UMAP
    umap_configuration <- umap.defaults
    umap_configuration$random_state <- seed

    upmap_obj <- customer_product_tbl %>% 
        select(-bikeshop_name) %>% 
        as.matrix() %>% 
        umap(config = umap_configuration) 
    
    umap_tbl <- upmap_obj %>% 
        pluck('layout') %>% 
        as_tibble() %>% 
        set_names(c('X', 'Y')) %>% 
        bind_cols(customer_product_tbl %>% select(bikeshop_name))

    
        # 4.0 COMBINE UMAP & K-MEANS
    combined_tbl <- umap_tbl %>% 
        left_join(kmeans_tbl, by = 'bikeshop_name') %>% 
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                    Cluster: {.cluster}"))
    
    return(combined_tbl)
}
