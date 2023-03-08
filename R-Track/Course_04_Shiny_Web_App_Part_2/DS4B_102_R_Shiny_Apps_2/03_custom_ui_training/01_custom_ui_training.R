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
jumbotron <- function(..., background_img = NULL) {
    
    if (is.null(background_img)){
        style_jumbotron = ""
    } else{
        style_jumbotron  <-  str_glue("background-image:url({background_img}); background-size:cover;")
    }
    
    div(
        class = "jumbotron",
        style = style_jumbotron,
        
        div(
            class = "jumbotron-ui-box text-default bg-default",
            style = "color: white; background-color: rgba(0,0,0,0.5); padding:25px",
            
            # User Input Go Here
            ...
        )
    )
    
}



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
                 class = "jumbotron-ui-box text-default bg-default",
                 style = "color: white; background-color: rgba(0,0,0,0.5); padding:25px",
                 
                 # User Input Go Here
                 h1("Why Learn Shiny?", style = "color: white;"),
                 a(href = "#", class = "btn btn-primary", "Learn More")
             )
           ),
         column(
             width = 12,
             jumbotron(
                 background_img = 'data_science_team.jpg',
                 
                 # User Input Go Here
                 h1("Why Learn Shiny?", style = "color: white;"),
                 a(href = "#", class = "btn btn-primary", "Learn More")
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