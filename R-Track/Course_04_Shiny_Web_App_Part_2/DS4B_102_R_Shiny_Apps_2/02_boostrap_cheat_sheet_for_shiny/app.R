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
            fluidRow(
                column(width = 4, class = "bg-primary", p("Grid width 4")),
                column(width = 4, class = "bg-warning", p("Grid width 4")),
                column(width = 4, class = "bg-danger", p("Grid width 4"))
            ),
            fluidRow(
                column(width = 3, class = "bg-primary", p("Grid width 3")),
                column(width = 9, class = "bg-warning", p("Grid width 9"))
            )
        ),
        
        hr(),
        
        ### 2.0 WORKING WITH TEXT ----
        h2("2.0 Working With Text"),
        
        p(class = "lead", "Business Science University help us learn Shiny"),
        
        fluidRow(
            column(width = 6,
                   p("We are creating a Boostrap for Shiny cheat sheet"),
                   p(strong("In section 1"), "we learn about the", strong(em("Boostrap Grid System."))),
                   p(tags$mark("In Section 2, "), ", we learned about working with text in", code("Boostrap"), "."),
                   
                   ),
            column(width = 6,
                   tags$blockquote(
                       class = "blockquote-reverse", 
                       p("Learning data sicience, consistency is more important that quantity."),
                       
                       tags$footer("Quote by", tags$cite(title = "Matt Dancho", "Matt Dancho"))
                   )
             )
        ),
        
        div(style = "height: 400px;"),
            
        )
      )
    


# SERVER ----
server <- function(input, output, session) {
     
}

shinyApp(ui = ui, server = server)