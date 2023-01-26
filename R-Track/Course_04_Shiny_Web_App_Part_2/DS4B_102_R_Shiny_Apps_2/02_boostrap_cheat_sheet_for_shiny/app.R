# BOOSTRAP CHEAT SHEET FOR SHINY ----
#DS4B 202-R ----


# LIBRARIES ----
library(pacman)
p_load(shiny, tidyverse, plotly)


# USER INTERFACE ----
ui <- shiny::fluidPage(
    title = "Bootstrap Cheat Sheet for Shiny",
    
    div(
        class = "container",
        id    = "page",
        
        ## HEARDER ----
        # https://getbootstrap.com/docs/3.3/components/#page-header
        
        # shiny format
        h1(class = "page-header", "Bootstrap Cheat Sheet", tags$small("For Shiny")),
        
        p(class = "lead",  
          "This cheat sheet is the first of the", 
          "Expert Shiny Application Development Course",
          "by Business Science")
        
        
        # HTML format
        # HTML(
        #     '<h1 class = "page-header">
        #         Bootstrap Cheat Sheet
        #         <small>For Shiny</small>
        #         </h1>)'
        # )
        
    )
    
    )


# SERVER ----
server <- function(input, output, session) {
     
}

shinyApp(ui = ui, server = server)