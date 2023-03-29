get_stock_mavg_info <-
function(data) {
    
    n_short <- data %>% pull(moving_avg_short) %>% is.na() %>% sum() + 1
    n_long  <- data %>% pull(moving_avg_long) %>% is.na() %>% sum() + 1
    
    data %>% 
        tail(1) %>% 
        mutate(
            moving_avg_warning_flag = moving_avg_short < moving_avg_long,
            n_short                 = n_short,
            n_long                  = n_long,
            pct_change              = (moving_avg_short - moving_avg_long) / moving_avg_long
        )
}
generate_favorite_card <-
function(data) {
    column(
        width = 3,
        info_card(
            title = as.character(data$stock),
            value = str_glue("{data$n_short}-Day <small>vs {data$n_long}-Day</small>") %>% HTML(),
            sub_value      = data$pct_change %>% scales::percent(accuracy = 0.1),
            sub_text_color = ifelse(data$moving_avg_warning_flag, "danger", "success"),
            sub_icon       = ifelse(data$moving_avg_warning_flag, "arrow_down", "arrow-up")
        )
    )
}
generate_favorite_cards <-
function(favorites,
                                    from = today() - days(180),
                                    to   = today(),
                                    moving_avg_short = 20,
                                    moving_avg_long  = 50){

favorites %>% 
    
    # Step 1 - pull the stock data and mavg calculations as a list
    map(.f = function(x) {
        
        x %>% 
            get_stock_data(
                from = from,
                to   = to,
                moving_avg_short = moving_avg_short,
                moving_avg_long  = moving_avg_long
            )
    }) %>% 
    
    set_names(favorites) %>% 
    
    # Step 2 - within each list, pull the last row 
    map(.f = function(data) {
        
        data %>% 
            get_stock_mavg_info()
    }) %>% 
    
    # Step 3 - stacks all the rows together 
    bind_rows(.id = "stock") %>% 
    mutate(stock = as_factor(stock)) %>%         # keep the order by using a factor
    split(.$stock) %>%                           # split into a list with the named stock
    
    # Step 4
    map(.f = function(data){
        data %>% generate_favorite_card()
        
    }) %>% 
    
    # Step 5
    tagList()
}
