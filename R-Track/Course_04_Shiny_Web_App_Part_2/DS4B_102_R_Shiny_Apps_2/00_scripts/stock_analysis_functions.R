get_stock_list <-
function(stock_index = "SP500") {
    tq_index(stock_index) %>% 
        select(symbol, company) %>% 
        arrange(symbol) %>% 
        mutate(label = str_c(symbol, company, sep = ", ")) %>% 
        select(label)
}
get_stock_data <-
function(stock_symbol, 
                           from = today() - days(180), 
                           to = today(), 
                           moving_avg_short = 20, 
                           moving_avg_long = 50) {
    
    stock_symbol %>% 
        tq_get(get = "stock.prices", from = from, to = to) %>% 
        select(date, adjusted) %>% 
        
        # if we have a moving avg value of 5 (k =5), there should be four NA's               
        mutate(moving_avg_short = rollmean(adjusted, k = moving_avg_short, na.pad = TRUE, align = 'right')) %>% 
        mutate(moving_avg_long = rollmean(adjusted, k = moving_avg_long, na.pad = TRUE, align = 'right')) 
    
}
get_symbol_from_user_input <-
function(user_input){
    user_input %>% 
        str_split(pattern = ", ") %>% 
        pluck(1, 1)
}
plot_stock_data <-
function(data) {
    g <- data %>% 
        gather(key = "legend", value = "value", adjusted:moving_avg_long, factor_key = TRUE) %>% 
        
        ggplot(aes(date, value, color = legend)) +
        geom_line(aes(linetype = legend)) +
        
        # scales
        scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
        scale_color_tq()+
        
        # labs
        labs(x = "", y = "Adjusted Share Price" )+ 
        
        # theme
        theme_tq() 
    
    plot <- ggplotly(g)
    
    return(plot)
    
}
generate_commentary <-
function(data, user_input) {
    warning_signal <- data %>% 
        tail(1) %>% 
        mutate(moving_warning_flag = moving_avg_short < moving_avg_long) %>% 
        pull(moving_warning_flag)
    
    # value for moving_avg_short
    n_short <- data %>% pull(moving_avg_short) %>% is.na() %>% sum() + 1
    
    # value for moving_avg_long
    n_long <- data %>% pull(moving_avg_long) %>% is.na() %>% sum() + 1
    
    if (warning_signal) {
        str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day movig average, indicating negative trend") 
        
    } else {
        str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day movig average, indicating positive trend") 
    }
}
