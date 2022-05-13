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
plot_customer_segments <-
function(k = 4, seed = 123, interactive = TRUE) {
    
    # DATA MANIPULATION 
    combined_tbl <- get_customer_segments(k = k, seed = seed)
    
    # VISUALIZATION
    g <- combined_tbl %>% 
        
        ggplot(aes(X, Y, color = .cluster)) +
        
        # geoms
        geom_point(aes(text = label_text)) + 
        
        # format
        labs(
            title    = 'Customer Segmentation: 2D Projection',
            subtitle = 'UMAP 2D Projection with K-Means Cluster Assigment'
        ) +
        
       scale_color_tq() +
       theme_tq() + 
       theme(legend.position = 'none')    
    

    # INTERACTIVE VS STATIC
    if (interactive) {
        ggplotly(g, tooltip = 'text')
        
    } else {
        g + 
            geom_label_repel(aes(label = label_text),
                             size = 2.5)
    }
     
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
    combined_tbl <- get_customer_segments(k = k, seed = seed)
     
    top_n_tbl <- bike_orderlines_tbl %>% 
        select(bikeshop_name, model, category_1,category_2, price, quantity) %>% 
        
        group_by_at(.var = vars(bikeshop_name:price)) %>% 
        summarise(total_qty = sum(quantity)) %>% 
        ungroup() %>% 
        
        # top 10 products
        group_by(bikeshop_name) %>% 
        #.by_group - If TRUE, will sort first by grouping variable. 
        # Applies to grouped data frames only.
        arrange(desc(total_qty), .by_group = TRUE) %>% 
        slice(1:top_n_products) %>% 
        ungroup() %>% 
        
        left_join(
            combined_tbl %>% select(bikeshop_name, .cluster), by = 'bikeshop_name'
        ) %>% 
        
        # labels
        mutate(label_text = str_glue("Bikeshop: {bikeshop_name}
                                     Model: {model}
                                     Category 1: {category_1}
                                     Category 2: {category_2}
                                     Price: {scales::dollar(price)}"))
    
    
    # VISUALIZATION
    
    # New facet label names for .cluster variable
    new_labels <- c("1" = "Cluster 1", 
                    "2" = "Cluster 2", 
                    "3" = "Cluster 3", 
                    "4" = "Cluster 4")

    g <- top_n_tbl %>% 
        ggplot(aes(x = category_1, y = price, color = .cluster)) +
        
        # geoms
        geom_violin() +
        
        geom_jitter(aes(text = label_text),
                    width = 0.2, alpha = 0.5, size = 2) +
        
        facet_wrap(~ .cluster, labeller = labeller(.cluster = new_labels)) + 
        #coord_flip() + 
        
        # format
        theme_tq(12) + 
        theme(
            legend.position = 'none',
            strip.text = element_text(margin = margin(5,5,5,5, unit = 'pt'),
                                      size = 16), 
             ) +
        
        scale_color_tq() + 
        scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
        
        # labs
        labs(
            title = str_glue("Top {top_n_products} Bike Models by Customer and Cluster"),
            subtitle = '',
            x = 'Category 1',
            y = 'Price (Log Scale)'
        )
     
    # INTERACTIVE VS STATIC
    if (interactive) {
        ggplotly(g, tooltip = 'text')  
            
        # layout(
        # title = list(
        #     text = list(
        #         text = str_glue("Top {top_n_products} Bike Models, by Customer and Cluster",
        #                         '<br>', 
        #                         '<sup>', 
        #                         '',
        #                         '</sup>'))))

        
        } else {
            return(g)
    }
}
