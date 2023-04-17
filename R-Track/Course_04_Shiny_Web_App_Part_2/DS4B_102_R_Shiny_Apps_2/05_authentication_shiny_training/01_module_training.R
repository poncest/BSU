# BUSINESS SCIENCE ----
# DS4B 202-R ----
# AUTHENTICATION & MODULE TRAINING -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Render a Login Dialog Page
# - Dynamically render a UI upon authentication
# - Create a module to produce the login
# - Use the shinyauthr package

library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyjs)
library(shinyauthr) # devtools::install_github("business-science/shinyauthr")

ui <- navbarPage(
    title = "Module Training", 
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    
    tabPanel(
        useShinyjs(),
        title = "Login Module",
        
        h2("No Module"),
        
        div(
            id = "login",
            style = "width: 500px; max-width: 100%; margin: 0 auto;; padding: 20px;",
            div(
                class = "well",
                h2(class = "text-center", "Please Login"),
                
                textInput(inputId     = "user_name", 
                          label       = tagList(icon("user"), "User Name"), 
                          placeholder = "Enter user name"),
                
                passwordInput(inputId     = "password", 
                              label       = tagList(icon("unlock-alt"), "Password"),
                              placeholder = "Enter password"),
                
                div(
                    class = "text-center",
                    actionButton(inputId = "login_button", "Log in", 
                                 class = "btn-primary", style = "color:white;")
                    
                )
            )
        ),
        
        h2("Using A Module"),
        
        # TODO
        
        h2('Using Shiny Auth')
        
        # TODO
    )
)

server <- function(input, output, session) {
    
    user_base_tbl <- tibble(
        user_name = "user1",
        password  = "pass1"
    )
    
    # NO MODULE ----
    
    
    # MODULE ----
    
    
    # SHINYAUTHR ----
    
    
}

shinyApp(ui, server)
