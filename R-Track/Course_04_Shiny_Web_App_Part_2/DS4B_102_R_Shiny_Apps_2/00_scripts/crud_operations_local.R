read_user_base <-
function() {
    user_base_tbl <<- read_rds(file = "R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds")
}
update_amd_write_user_base <-
function(user_name, column_name, assign_input) {
    
    user_base_tbl[user_base_tbl$user == user_name, ][[column_name]] <<- assign_input
    
    write_rds(
        user_base_tbl,
        file = here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_data_local/user_base_tbl.rds")
    )
}
