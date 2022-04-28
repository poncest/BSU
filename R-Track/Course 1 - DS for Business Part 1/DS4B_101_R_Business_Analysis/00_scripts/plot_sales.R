plot_total_sales <-
function(unit = 'weekly', date_format = '%B %Y', interactive = TRUE) {
    
    # handle data
    data_tbl <- bike_orderlines_tbl %>% 
        
        select(order_date, total_price) %>% 
        
        # round down the month
        mutate(date_rounded = floor_date(order_date, unit = unit)) %>% 
        
        group_by(date_rounded) %>% 
        summarise(total_sales = sum(total_price)) %>% 
        ungroup() %>% 
        
        # text labels
        mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)},
                                 Date: {date_rounded %>% format(date_format)}"))
    
    
    # make the plot
    g1 <- data_tbl %>% 
        ggplot(aes(x = date_rounded, y = total_sales)) +
        
        # geoms
        geom_point(aes(text = label_text), color = '#2C3E50') +
        geom_smooth(method = 'loess', span = 0.2) +
        
        # format
        theme_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        # always show y-axis at 0 for timeseries
        expand_limits(y = 0) +
        
        # labels
        labs(
            title = 'Total Sales',
            x = '',
            y = 'Revenue (USD)'
        )
    
    
    # static vs interactive logic
    if (interactive) {
        return(ggplotly(g1, tooltip = 'text'))  # interactive
    } else{
       return(g1)                               # static
    } 
    
}
plot_categories <-
function(category_1 = 'All', category_2 = 'All',
                            unit = 'month', date_format = "%B %Y",
                            ncol = 1, scales = 'free_y',
                            interactive = TRUE) {
    
    # handle data
    
    data_tbl <- bike_orderlines_tbl %>% 
        select(order_date, category_1, category_2, total_price) %>% 
        mutate(date_rounded = floor_date(order_date, unit = unit)) %>% 
        
        group_by(date_rounded, category_1, category_2) %>% 
        summarise(total_sales = sum(total_price)) %>% 
        ungroup() %>% 
        
        mutate(label_text = str_glue("Sales: {scales::dollar(total_sales)}
                                 Date: {date_rounded %>%  format(date_format)}")) %>% 
        
        mutate(category_2 = as_factor(category_2) %>% fct_reorder2(date_rounded, total_sales))
    
    
    # handle input
    cat_1_text <- str_to_lower(category_1)
    cat_2_text <- str_to_lower(category_2)
    
    
    # create filter logic
    if(cat_1_text != "all") {
        
        data_tbl <- data_tbl %>% 
            filter(category_1 %>% 
                       str_to_lower() %>% 
                       str_detect(pattern = cat_1_text))
    }
    
    if(cat_2_text != "all") {
        
        data_tbl <- data_tbl %>% 
            filter(category_2 %>% 
                       str_to_lower() %>% 
                       str_detect(pattern = cat_2_text))
    }
    
     
    # make plot
    g2 <- data_tbl %>% 
        ggplot(aes( x= date_rounded, y = total_sales, color = category_2)) + 
        
        # geoms
        geom_point(aes(text = label_text),color = '#2C3E50') + 
        geom_smooth(method = 'loess', span = 0.2) + 
        
        # facet
        facet_wrap(~ category_2, scales = scales, ncol = ncol) + 
        
        # format
        expand_limits(y = 0) + 
        theme_tq() +
        theme(
            legend.position = 'none',
            strip.text = element_text(margin = margin(5,5,5,5), size = 12)
        )  +
        
        scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = 'K'))+
        scale_color_tq() + 
        
        labs(
            title = 'Sales by Category 2',
            x = '',
            y = ''
        )
    
    
    # static vs. interactive plot
    if (interactive) {
        return(ggplotly(g2, tooltip = 'text'))  # interactive
    } else{
        return(g2)                               # static
    } 
    
}
