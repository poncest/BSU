# BOOSTRAP CHEAT SHEET FOR SHINY ----
#DS$B 202-R ----

library(pacman)
p_load(shiny, tidyverse, plotly)


ui <- shiny::fluidPage(title = "Bootstrap Cheat Sheet for Shiny")



server <- function(input, output, session) {
     
}

shinyApp(ui = ui, server = server)