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
jumbotron <- function(..., background_img = NULL, 
                      ui_box_bg_color = "rgba(0,0,0,0.5)", ui_box_text_color = "white") {
    
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
            style = str_glue("color: {ui_box_text_color}; background-color: {ui_box_bg_color}; padding:25px;"),
            
            # User Input Go Here
            ...
        )
    )
}

info_card <- function(title, value, sub_value,
                      main_icon      = "chart-line",
                      sub_icon       = "arrow-up",
                      bg_color       = "default",
                      text_color     = "default",
                      sub_text_color = "success"){
    
    div(
        class = "panel panel-default",
        div(
            class = str_glue("panel-body bg-{bg_color} text-{text_color}"),
            p(class = "pull-right", icon(class = "fa-3x", main_icon)),
            h4(title),
            h5(value),
            p(
                class = str_glue("text-{sub_text_color}"),
                icon(sub_icon),
                tags$small(sub_value)
            )
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
             
             ## Jumbotron Function
             jumbotron(
                 background_img    = 'data_science_team.jpg',
                 ui_box_bg_color   = "rgba(0,125,255,0.35)", 
                 ui_box_text_color = "white",
                 
                 # User Input Go Here
                 h1("Why Learn Shiny?"),
                 a(href = "#", class = "btn btn-primary", "Learn More")
                 )
            )
        )
    ),
    
    # CARD COMPONENT ----
    div(
        class = "container",
        id = "cards",
        h2("Info Card"),
        
        column(
            width = 4,
            div(
                class = "panel panel-default",
                div(
                    class = "panel-body bg-default text-default",
                    p(class = "pull-right", icon(clas = "fa-3x", "chart-line")),
                    h4("APPL"),
                    h5("20-Day <small> vs. 50-day </small>" %>% HTML()),
                    p(
                        class = "text-sucess",
                        icon("arrow-up"),
                        tags$small("20%")
                    )
                )
            )
        ),
        column(
            width = 4,
            
            ## Info Card Function
            info_card(
                title   = "NFLX", 
                value      = p("20-Day", tags$small("vs. 50-Day")), 
                sub_value  = "-10%", 
                sub_icon   = "arrow-dowmn", 
                sub_text_color = "danger" )
            )
        
    ),
    
    # THUMBNAIL CHALLENGE ----
     
    div(
        class = "container",
        id    = "thumbnails",
        h2("Thumbnails"),
        
        column(
            width = 4,
            div(
                class = "thumbnail text-center",
                img(src = "data_science_team.jpg"),
                div(
                    class = "caption",
                    
                    # User Input (...)
                    h3("Learn Data Science in Weeks, Not Years"),
                    p("These students are learning Shiny through Business Science University!"),
                    a(class = "btn btn-primary btn-sm", href = "#", "Learn More")
                )
            )
        )
    ),
    
    div(style = "height:400px;")
    
    
)

# SERVER ----
server <- function(input, output, session) {
    
}

shinyApp(ui, server)

