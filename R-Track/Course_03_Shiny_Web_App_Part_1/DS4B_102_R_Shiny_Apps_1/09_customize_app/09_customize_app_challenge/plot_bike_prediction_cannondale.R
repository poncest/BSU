


plot_bike_prediction_cannondale <-
    function(data, interactive = TRUE ) {
        
        g <- data %>% 
            # category_2 as factor
            mutate(category_2 = fct_reorder(category_2, price)) %>% 
            
            # label text column
            mutate(label_text = str_glue("Unit Price: {scales::dollar(price, accuracy = 1)}
                                 Model: {model}
                                 Bike Type: {category_1}
                                 Bike Family: {category_2}
                                 Frame Material {frame_material}")) %>% 
            
            ggplot(aes(x = category_2, y = price, color = estimate)) +
            
            # geometries
            geom_violin() +
            geom_jitter(aes(text = label_text), width = 0.1, alpha = 0.5, size = 2) +
            
            # scales
            scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
            scale_color_manual(values = c("#000000", "#77bb00"))+
            coord_flip() + 
            
            # facets
            facet_wrap(~ frame_material) +
            
            # labs
            labs(title = "", x = "", y = "Log Scale") +
            
            # theme
            theme_tq()+
            theme(
                strip.text.x     = element_text(margin = margin(5,5,5,5)),
                strip.background = element_rect(fill = "#000000"),
                axis.text        = element_text(color = "black"),
                axis.title       = element_text(color = "black"),
                legend.text      = element_text(color = "black"),
                )
        
        # interactive
        if (interactive) {
            return(ggplotly(g, tooltip = "text"))
        } else {
            return(g)   
        }
    }
