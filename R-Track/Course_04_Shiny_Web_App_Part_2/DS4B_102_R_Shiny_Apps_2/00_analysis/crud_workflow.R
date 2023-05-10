# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - PERSISTENT DATA -----
# Version 1
# CRUD WORKFLOW


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