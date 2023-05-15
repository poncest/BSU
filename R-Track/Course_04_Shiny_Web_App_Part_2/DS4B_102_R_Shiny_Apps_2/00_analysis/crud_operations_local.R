# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - PERSISTENT DATA -----
# Version 1


# CRUD WORKFLOWm ----


library(tidyverse)

# 1.0 WORKFLOW FOR CRUD OPERATIONS USING BASE R ----

user_base_tbl <<- read_rds(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds"))

user_base_tbl[user_base_tbl$user == "user1", ][["last_symbol"]] <- "MA"

user_base_tbl[user_base_tbl$user == "user1", ][["favorites"]] <- list(c("AAPL", "GOOG", "NFLX", "MA")) 

user_base_tbl[user_base_tbl$user == "user1", ][["user_settings"]]

user_settings <- tibble(
    moving_avg_short = 15,
    moving_avg_long  = 500,
    time_window      = 365
    )

user_settings 

user_base_tbl[user_base_tbl$user == "user1", ][["user_settings"]] <- list(user_settings)


write_rds(user_base_tbl, path = here:here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds"))

data <- read_rds(file = "R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds")

data %>% 
    filter(user == "user1") %>% 
    pull(user_settings)


# 2.0 MODULARIZE FOR LOCAL STORAGE ----

read_user_base <- function() {
    user_base_tbl <<- read_rds(file = "R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds")
}

read_user_base()


# update_user_base()
update_user_base <- function(user_name, column_name, assign_input) {
    user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
}

update_user_base(user_name = "user1", column_name = "last_symbol", assign_input = "MA")
user_base_tbl


# write_user_base()
write_user_base <- function() {
    write_rds(
        user_base_tbl,
        file = "R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds"
        )
}

write_user_base()

read_user_base()

user_base_tbl


# combined function
update_amd_write_user_base <- function(user_name, column_name, assign_input) {
    
    user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
    
    write_rds(
        user_base_tbl,
        file = here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds")
    )
}

# 3.0 CHECK WORKFLOW ----

read_user_base()
user_base_tbl

update_amd_write_user_base(user_name = "user1", column_name = "last_symbol", assign_input = "V")


# SAVE FUNCTIONS ----
dump(c("read_user_base", "update_amd_write_user_base"), 
       file = "R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/crud_operations_local.R" )


