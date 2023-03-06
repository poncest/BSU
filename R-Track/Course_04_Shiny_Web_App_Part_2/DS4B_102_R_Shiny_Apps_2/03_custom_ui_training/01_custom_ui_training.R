# BUSINESS SCIENCE ----
# DS4B 202-R ----
# CUSTOM UI TRAINING -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Make custom shiny functions for  Bootstrap 3 components: Jumbotron, Info Card, and Thumbnail

# LIBRARIES ----
# Shiny
library(shiny)
library(shinythemes)

# Core
library(tidyverse)

# CUSTOM FUNCTIONS ----



# UI ----
ui <- fixedPage(
    title = "Custom UI Components",
    
    themeSelector(),
    
    h1(class = "page-header", "Custom UI Training"),
    
    # JUMBOTRON COMPONENT ----
    div(
        class = "container",
        id = "jumbotron",
        h2("Jumbotron Component"),
        
        column(
         width = 12,
         div(
             class = "jumbotron",
             style = "background-image:url('data_science_team.jpg'); background-size:cover;",
             
             div(
                 class = "jumbotron-ui-box text-default bg-primary",
                 style = "background-color: rgba(0,0,0,0.5); padding:25px",
                 p("placeholder")
             )
           )
        )
    ),
    
    # CARD COMPONENT ----
    div(
        class = "container",
        id = "cards",
        h2("Info Card")
    ),
    
    # THUMBNAIL CHALLENGE ----
    
    div(
        class = "container",
        id    = "thumbnails",
        h2("Thumbnails")
    ),
    
    div(style = "height:400px;")
    
    
)

# SERVER ----
server <- function(input, output, session) {
    
}

shinyApp(ui, server)