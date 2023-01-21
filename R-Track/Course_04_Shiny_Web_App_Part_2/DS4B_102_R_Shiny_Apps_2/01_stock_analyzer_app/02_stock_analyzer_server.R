# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - SERVER -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Connect the stock dropdown, interactive plot and commentary using shiny server operations


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyquant)
library(tidyverse)

source(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/stock_analysis_functions.R"))

stock_list_tbl <- get_stock_list("SP500")
# stock_data_tbl <- get_stock_data("AAPL", from = "2018-01-01", to = "2019-01-01")


# UI ----
ui <- fluidPage(
    title = "Stock Analyzer",
    
    # 1.0 HEADER ----
    div(
        h1("Stock Analyzer", "by Business Science"),
        p("This is the first mini-project completed in our", "Expert Shiny Applications Course (DS4B 202-R)")
    ),
    
    # 2.0 APPLICATION UI -----
    div(
        column(
            width = 4, 
            wellPanel(
                pickerInput(
                    inputId = "stock_selection", 
                    label   = "Stock List (Pick One to Analyze)",
                    choices = stock_list_tbl$label,
                    multiple = FALSE, 
                    selected = stock_list_tbl %>% filter(label %>% str_detect("AAPL")) %>% pull(label),
                    options = pickerOptions(
                        actionsBox = FALSE,
                        liveSearch = TRUE,
                        size = 10
                    )
                ),
                actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
                verbatimTextOutput(outputId = "selected_symbol")
            )
            
        ),
        column(
            width = 8, 
            div(
                div(h4(textOutput(outputId = "plot_header"))),
                div(
                    # verbatimTextOutput(outputId = "stock_data")
                    # stock_data_tbl %>% plot_stock_data()
                )
            )
        )
    ),
    
    # 3.0 ANALYST COMMENTARY ----
    div(
        column(
            width = 12,
            div(
                div(h4("Analyst Commentary")),
                div(
                    # stock_data_tbl %>% generate_commentary(user_input = "Placeholder")
                )
            )
        ) 
    )
)
 
# SERVER ----
server <- function(input, output, session) {
    
    # stock symbol ----
    stock_symbol <- eventReactive(input$analyze, {
        get_symbol_from_user_input(input$stock_selection)
    }, ignoreNULL = FALSE)
    
    output$selected_symbol <- renderPrint(stock_symbol())
    
    
    # Plot Header ----
    plot_header <- eventReactive(input$analyze, {
        input$stock_selection
    }, ignoreNULL = FALSE) 
    
    output$plot_header <- renderText({
        plot_header() 
    })
    
    # Get Stock Data ----
    stock_data_tbl <- reactive({
        stock_symbol() %>% get_stock_data(
            from = today() - days(180), 
            to = today(),
            moving_avg_short = 20,
            moving_avg_long = 50)
    })
    
    # output$stock_data <- renderPrint(stock_data_tbl())
}


# RUN APP ----
shinyApp(ui = ui, server = server)


