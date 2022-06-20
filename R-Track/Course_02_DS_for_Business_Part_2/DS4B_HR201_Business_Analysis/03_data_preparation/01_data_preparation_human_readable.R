## HR_201_Employee_Attrition_Project
## Steven Ponce 2022

# DATA PREPARATION ----
# Human Readable ----

# Libraries
library(pacman)
p_load(tidyverse, tidyquant, readxl)


# Load Data
# train
path_train            <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'

path_data_definitions <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_data_definitions.xlsx'

train_raw_tbl         <- read_excel(path_train, sheet = 1)

definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)


# Processing Pipeline - for people readability
source(here::here('R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/data_processing_pipeline_rev1.R'))

train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)


# For example, lets examine readability before/after
# Before (coded)
train_raw_tbl %>% 
    ggplot(aes(Education)) +
    geom_bar()
    
# After (decoded)
train_readable_tbl %>% 
    ggplot(aes(Education)) +
    geom_bar() 
    

# Tidy data ----    
definitions_tbl <- definitions_raw_tbl %>% 
    rename(x = ...1, y = ...2) %>% 
    fill(x, .direction = 'down') %>% 
    drop_na() %>% 
    separate(col = y, into = c('key', 'value'), sep = " '", remove = TRUE) %>% 
    mutate(value = str_remove(string = value, pattern = "'") %>% str_trim()) %>% 
    mutate(key = as.numeric(key)) %>%
    rename(column_name = x)

# Turning the definition tibble into a list
definitions_list <- definitions_tbl %>% 
    split(.$column_name) %>% 
    map(~ select(., -column_name)) %>% 
    map(~ mutate(., value = as_factor(value)))
    
# calling the list
definitions_list[[1]]
definitions_list[['Education']] 

# for loop to iterate (configure col names)
for (i in seq_along(definitions_list)) {
    
    list_name <- names(definitions_list)[i]
    
    # reset col names
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, '_value'))
    
}

# updated definition list (after running the for loop)
definitions_list

# crating a list - human readable
data_merged_tbl <- list(HR_Data = train_raw_tbl) %>% 
    # adding the definition list tibble
    append(definitions_list, after = 1) %>% 
    # join them
    reduce(left_join) %>% 
    # removing column names from the definition list
    select(-one_of(names(definitions_list))) %>%
    # rename columns (remove _value)
    set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
    # sort by names
    select(sort(names(.)))

glimpse(data_merged_tbl) 

# does a feature needs to follow a specific order/levels (low, med, high)?
data_merged_tbl %>% 
    select_if(is.character) %>% 
    glimpse()

data_merged_tbl %>% 
    distinct(BusinessTravel)

data_merged_tbl %>% 
    mutate_if(is.character, as.factor) %>% 
    select(where(is.factor)) %>% 
    glimpse()

# inspecting the levels
data_merged_tbl %>% 
    mutate_if(is.character, as.factor) %>% 
    select(where(is.factor)) %>% 
    map(levels)

# reodering the factors
data_processed_tbl <- data_merged_tbl %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate(
        BusinessTravel = BusinessTravel %>% fct_relevel('Non-Travel', 'Travel_Rarely', 'Travel_Frequently'),
        MaritalStatus = MaritalStatus %>% fct_relevel('Single', 'Married', 'Divorced')
    )

data_processed_tbl %>% 
    select_if(is.factor) %>% 
    map(levels)


# Processing Pipeline ----

# new function (REVIEW)
definitions_raw_tbl -> definitions_tbl
train_raw_tbl       -> data

process_hr_data_readable_01 <- function(data, definitions_tbl) {
    
    definitions_list <- definitions_tbl %>%
        fill(`...1`, .direction = "down") %>%
        filter(!is.na(`...2`)) %>%
        separate(`...2`, into = c("key", "value"), sep = " '", remove = TRUE) %>%
        rename(column_name = `...1`) %>%
        mutate(key = as.numeric(key)) %>%
        mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
        split(.$column_name) %>%
        map(~ select(., -column_name)) %>%
        map(~ mutate(., value = as_factor(value))) 
    
    
    # for loop to iterate (configure col names)
    for (i in seq_along(definitions_list)) {
        list_name <- names(definitions_list)[i]
        # reset col names
        colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, '_value'))
    }

    # crating a list - human readable
    data_merged_tbl <- list(HR_Data = data) %>% 
        # adding the definition list tibble
        append(definitions_list, after = 1) %>% 
        # join them
        reduce(left_join) %>% 
        # removing column names from the definition list
        select(-one_of(names(definitions_list))) %>%
        # rename columns (remove _value)
        set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
        # sort by names
        select(sort(names(.))) %>% 
        mutate_if(is.character, as.factor) %>% 
        mutate(
            BusinessTravel = BusinessTravel %>% fct_relevel('Non-Travel', 'Travel_Rarely', 'Travel_Frequently'),
            MaritalStatus = MaritalStatus %>% fct_relevel('Single', 'Married', 'Divorced')
        )
    
    return(data_merged_tbl) 
}

# testing process_hr_data_readable_01()
process_hr_data_readable_01(train_raw_tbl, definitions_tbl = definitions_raw_tbl) %>% 
    glimpse()

