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

    customer_product_tbl %>% 
        select(-bikeshop_name) %>% 
        as.matrix() %>% 
        umap(config = umap_configuration)

    
        # 4.0 COMBINE UMAP & K-MEANS
    
    
}
plot_customer_segments <-
function(k = 4, seed = 123, interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
    # INTERACTIVE VS STATIC
    
    
}
plot_customer_heatmap <-
function(interactive = TRUE) {
    
    # DATA MANIPULATION
    pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
        
        select(bikeshop_name, category_1, category_2, quantity) %>%
        
        group_by(bikeshop_name, category_1, category_2) %>%
        summarise(total_qty = sum(quantity)) %>%
        ungroup() %>%
        
        group_by(bikeshop_name) %>%
        mutate(pct = total_qty / sum(total_qty)) %>%
        ungroup() %>%
        
        mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev()) %>%
        
        mutate(label_text = str_glue("Customer: {bikeshop_name}
                                     Category: {category_1}
                                     Sub-Category: {category_2}
                                     Quantity Purchased: {total_qty}
                                     Percent of Sales: {scales::percent(pct, accuracy = 0.1)}"))
    
    # VISUALIZATION
    g <- pct_sales_by_customer_tbl %>%
        ggplot(aes(category_2, bikeshop_name)) +
        
        # Geoms
        geom_tile(aes(fill = pct)) +
        geom_text(aes(label = scales::percent(pct, accuracy = 0.1),
                      text = label_text),
                  size = 3) +
        facet_wrap(~ category_1, scales = "free_x") +
        
        # Formatting
        scale_fill_gradient(low = "white", high = "#2c3e50") +
        theme_tq() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))
        ) +
        labs(title = "Heatmap of Purchasing Habits")
    
    # INTERACTIVE VS STATIC
    if (interactive) {
        
        g <- g +
            labs(x = "", y = "") 
        
        return(ggplotly(g, tooltip = "text"))
        
    } else {
        
        g <- g +
            labs(x = "Bike Type (Category 2)", y = "Customer")
        
        return(g)
         
    }
    
}
plot_customer_behavior_by_cluster <-
function(top_n_products = 10, 
                                              k = 4, seed = 123, 
                                              interactive = TRUE) {
    
    # DATA MANIPULATION
    
     
    # VISUALIZATION
    
    
    # INTERACTIVE VS STATIC
    
    
}
