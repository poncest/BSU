# DS4B 101-R ----
# CUSTOMER SEGMENTATION FUNCTIONS

# 1.0 LIBRARIES ----
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(ggrepel)
library(plotly)


bike_orderlines_tbl <- read_rds("./R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

# 2.0 HEAT MAP PLOT ----
# - Refer to Chapter 4: Advanced Heat Map
plot_customer_heatmap <- function(interactive = TRUE) {
    
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

# testing function
plot_customer_heatmap(interactive = TRUE)   # interactive
plot_customer_heatmap(interactive = FALSE)  # static


# 3.0 CUSTOMER SEGMENTATION PLOT ----
get_customer_segments <- function(k = 4, seed = 123) {
    
    # 1.0 CUSTOMER TRENDS
    
    
    
    # 2.0 MODELING: K-MEANS CLUSTERING
    
    
    # 3.0 UMAP
    
    
    # 4.0 COMBINE UMAP & K-MEANS
    
    
}

plot_customer_segments <- function(k = 4, seed = 123, interactive = TRUE) {
    
    # DATA MANIPULATION
    
    
    # VISUALIZATION
    
    
    # INTERACTIVE VS STATIC
    
    
}

plot_customer_segments(k = 4, seed = 123, interactive = TRUE)
plot_customer_segments(k = 4, seed = 123, interactive = FALSE)


# 4.0 VISUALIZE CUSTOMER BEHAVIOR ----

plot_customer_behavior_by_cluster <- function(top_n_products = 10, 
                                              k = 4, seed = 123, 
                                              interactive = TRUE) {
    
    # DATA MANIPULATION
    
     
    # VISUALIZATION
    
    
    # INTERACTIVE VS STATIC
    
    
}

plot_customer_behavior_by_cluster(top_n_products = 10, 
                                  k = 4, seed = 123,
                                  interactive = TRUE)

plot_customer_behavior_by_cluster(top_n_products = 10, 
                                  k = 4, seed = 123,
                                  interactive = FALSE)

# 5.0 SAVE FUNCTIONS ----

function_names <- c("get_customer_segments", "plot_customer_segments",
                    "plot_customer_heatmap", "plot_customer_behavior_by_cluster")

dump(function_names, file = "00_scripts/plot_customer_segmentation.R")