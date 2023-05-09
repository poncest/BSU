# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - PERSISTENT DATA -----
# Version 1
# CRUD WORKFLOW


user_base_tbl <<- read_rds(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds"))

user_base_tbl[user_base_tbl$user == "user1", ]["last_symbol"] <- "MA"