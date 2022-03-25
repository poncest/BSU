# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----


library(tidyverse)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant)

bike_orderlines_tbl <- read_rds("R-Track/Course 1 - DS for Business Part 1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 CUSTOMER TRENDS ----
# - GOAL: Mine Customer Purchase History for similarity to other "like" customers
# - TECHNIQUES: K-Means Clustering, UMAP 2D Projection

# 1.1 Get Customer Trends ----
customer_trend_tbl <- bike_orderlines_tbl %>% 
    
    select(bikeshop_name, price, model, category_1, category_2, frame_material, quantity ) %>% 
    
    # summarization & group by (aggregate)
    # quantity is the measure we're going to aggregate
    group_by(bikeshop_name, price, model, category_1, category_2, frame_material) %>% 
    
    summarise(quantity_purchased = sum(quantity)) %>% 
    ungroup() %>% 
    
    # proportions (normalized)
    group_by(bikeshop_name) %>% 
    mutate(prop_of_total = quantity_purchased / sum(quantity_purchased)) %>% 
    ungroup()

 
# 1.2 Convert to User-Item Format (e.g. Customer-Product) ----

customer_product_tbl <- customer_trend_tbl %>% 
    select(bikeshop_name, model, prop_of_total) %>% 
    spread(key = model, value = prop_of_total, fill = 0)



# 2.0 MODELING: K-MEANS CLUSTERING ----

# 2.1 Performing K-Means ---- 
?kmeans

kmeans_obj <- customer_product_tbl %>% 
    select(-bikeshop_name) %>% 
    kmeans(centers = 5, nstart = 100)

kmeans_obj$centers
kmeans_obj$cluster

# we want to set the `nstart`to be > 1

# 2.2 Tidying a K-Means Object ----

broom::tidy(kmeans_obj)
broom::glance(kmeans_obj)
broom::augment(kmeans_obj, customer_product_tbl) %>% 
    select(bikeshop_name, .cluster)

# 2.3 How many centers (customer groups) to use? ----

# initial function that works with one element

center <- 3

kmeans_mapper <- function(centers = 3) {
    
    # matrix
    customer_product_tbl %>% 
        select(-bikeshop_name) %>% 
        
        #kmeans
        kmeans(centers = centers, nstart = 100)
}

# test function
kmeans_mapper(3) %>% 
    glance()


# mapping the function to many elements
kmeans_mapped_tbl <- tibble(centers = 1:15) %>% 
    mutate(k_means = centers %>% map(kmeans_mapper)) %>% 
    mutate(glance = k_means %>% map(glance))


kmeans_mapped_tbl %>% 
    unnest(glance) %>% 
    select(centers, tot.withinss)

# 2.4 Scree Plot ----
kmeans_mapped_tbl %>% 
    unnest(glance) %>% 
    select(centers, tot.withinss) %>% 
    
    # visualization
    ggplot(aes(x = centers, y = tot.withinss)) + 
    geom_point(color = '#2c3e50', size = 4)+
    geom_line(color = '#2c3e50', size = 1)+
    ggrepel::geom_label_repel(aes(label = centers), color = '#2c3e50') +
    
    # formatting  
    theme_tq()+
    labs(
        title = 'Scree Plot',
        subtitle = 'Measures the distance that each of the customers are from the closes K-Means center',
        caption = 'Conslusion: Based on the Scree Plot, we select 4 clusters to segment the customer base' )


# 3.0 VISUALIZATION: UMAP ---- 

# 3.1 Use UMAP to get 2-D Projection ----


# 3.2 Use K-Means to Add Cluster Assignments ----


# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----




# 4.0 ANALYZE PURCHASING TRENDS ----




