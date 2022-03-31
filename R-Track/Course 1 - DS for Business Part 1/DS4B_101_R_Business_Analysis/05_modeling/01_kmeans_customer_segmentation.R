# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CUSTOMER SEGMENTATION ----


library(tidyverse, quietly = TRUE)
library(broom)
library(umap)
library(ggrepel)
library(tidyquant, quietly = TRUE)

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
?umap

umap_obj <- customer_product_tbl %>% 
    select(-bikeshop_name) %>% 
    umap()

umap_results_tbl <- umap_obj$layout %>% 
    as_tibble() %>% 
    set_names(c('x', 'y')) %>% 
    bind_cols(
        customer_product_tbl %>% select(bikeshop_name)
    )

umap_results_tbl %>% 
    ggplot(aes(x, y)) + 
    geom_point() +
    geom_label_repel(aes(label = bikeshop_name),
                     size = 3)

# 3.2 Use K-Means to Add Cluster Assignments ----
umap_results_tbl 

kmeans_4_obj <- kmeans_mapped_tbl %>% 
    pull(k_means) %>% 
    pluck(4)
    
kmeans_4_clusters_tbl <- kmeans_4_obj %>% 
    augment(customer_product_tbl) %>% 
    select(bikeshop_name, .cluster)

# join
umap_kmeans_4_results_tbl <- umap_results_tbl %>% 
    left_join(kmeans_4_clusters_tbl)


# 3.3 Visualize UMAP'ed Projections with Cluster Assignments ----

umap_kmeans_4_results_tbl %>% 
    
    # labels
    mutate(label_text = str_glue('Customer: {bikeshop_name}
                                 Cluster: {.cluster}')) %>% 
    
    ggplot(aes(x, y, color = .cluster)) +
    
    # geometries
    geom_point() + 
    geom_label_repel(aes(label = label_text), size = 3) +
    
    # format
    theme_tq() +
    scale_color_tq() +
    
    theme(legend.position = 'none') +
    
    # labs
    labs(
        title ='Customer Segmentation: 2D Projection',
        subtitle = 'UMAP 2D Prjection with K-Means Cluster Assigment',
        caption = 'Conclusion: 4 Customer Segments identified using 2 algorithms'
    ) 
    

# 4.0 ANALYZE PURCHASING TRENDS ----

customer_trend_tbl %>% 
    pull(price) %>% 
    quantile(probs = c(0, 0.33, 0.66, 1)) 

?quantile


cluster_trends_tbl <- customer_trend_tbl %>% 
    
    # join cluster assignment by bikeshop name
    left_join(umap_kmeans_4_results_tbl) %>% 
    
    # encoding price
    mutate(
        price_bin = case_when(
            price <= 2240 ~ 'low',
            price >= 4260 ~ 'medium',
            TRUE ~ 'high'
        )) %>% 
    
    # selecting specific cols
    select(.cluster, model, contains('price'), 
           category_1:quantity_purchased) %>% 
    
    # aggregate qty purchased by cluster and product attributes
    group_by_at(.vars = vars(.cluster:frame_material)) %>% 
    
    summarise(
        total_quantity = sum(quantity_purchased)
    ) %>% 
    
    ungroup() %>% 
    
    # normalize the data - prop of total
    group_by(.cluster) %>% 
    
    mutate(
        prop_of_total = total_quantity / sum(total_quantity)
        ) %>%
    
    ungroup() 

# cluster 1 - medium/high price - road model preference
cluster_trends_tbl %>% 
    filter(.cluster == 1) %>% 
    arrange(desc(prop_of_total)) %>% 
    mutate(cum_prop = cumsum(prop_of_total)) 
    

get_cluster_trends <- function(cluster = 1) {
    
    cluster_trends_tbl %>% 
        filter(.cluster == cluster) %>% 
        arrange(desc(prop_of_total)) %>% 
        mutate(cum_prop = cumsum(prop_of_total)) 
    
}

get_cluster_trends(cluster = 1)   


# cluster 2 - high/low price - road model preference
get_cluster_trends(cluster = 2) 

# cluster 3 - high/low price - mountain model preference, aluminum preference
get_cluster_trends(cluster = 3) 

# cluster 4 - high/medium price - mountain model preference, carbon preference
get_cluster_trends(cluster = 4) 


# update visualization

# cluster labels
cluster_label_tbl <- tibble(
    .cluster = 1:4,
    .cluster_label = c(
        'Medium/High Price, Road Model',
        'High/Low Price, Road model',
        'High/Low Price, Mountain Model, Aluminum Frame',
        'High/Medium Price, Mountain Model, Carbon Frame')
) %>% 
    
    mutate(.cluster = as_factor(as.character(.cluster))) 
    
    

umap_kmeans_4_results_tbl %>% 
    left_join(cluster_label_tbl) %>% 
    
    # labels
    mutate(label_text = str_glue('Customer: {bikeshop_name}
                                 Cluster: {.cluster}
                                 {.cluster_label}')) %>% 
    
    ggplot(aes(x, y, color = .cluster)) +
    
    # geometries
    geom_point() + 
    geom_label_repel(aes(label = label_text), size = 3) +
    
    # format
    theme_tq() +
    scale_color_tq() +
    
    theme(legend.position = 'none') +
    
    # labs
    labs(
        title ='Customer Segmentation: 2D Projection',
        subtitle = 'UMAP 2D Prjection with K-Means Cluster Assigment',
        caption = 'Conclusion: 4 Customer Segments identified using 2 algorithms') 


