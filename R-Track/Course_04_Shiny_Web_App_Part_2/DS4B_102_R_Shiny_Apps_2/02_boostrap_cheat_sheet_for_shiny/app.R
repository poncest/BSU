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
          a(href   = "https://university.business-science.io/",
            target = "_blank", "Expert Shiny Application Development Course"),
          "by Business Science"
         ),
        
        ### 1.0 BOOTSTRAP GRID SYSTEM ----
        h2("1.0 Boostrap Grid System"),
        
        div(
            class = "container text-center",
            "Placeholder" 
        )
        
       )
    )


# SERVER ----
server <- function(input, output, session) {
     
}

shinyApp(ui = ui, server = server)