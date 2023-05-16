# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - PERSISTENT DATA -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Perform CRUD Operations
# - Use local data storage via RDS File


# LIBRARIES ---- 
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shinyauthr)  # devtools::install_github("business-science/shinyauthr")

library(plotly)
library(tidyquant)
library(tidyverse)

source(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/stock_analysis_functions.R"))
source(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/info_card.R"))
source(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/generate_favorite_cards.R"))
source(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/panel_card.R"))

source(here::here("R-Track/Course_04_Shiny_Web_App_Part_2/DS4B_102_R_Shiny_Apps_2/00_scripts/crud_operations_local.R"))

stock_list_tbl <- get_stock_list("SP500")

# UI ----
ui <- tagList(
    # CSS ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("cyborg")),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # JS ----
    shinyjs::useShinyjs(),
    
    # User Login ----
    verbatimTextOutput(outputId = "creds"),   # delete
    shinyauthr::loginUI(
        id    = "login", 
        title = tagList(h2(class = "text-center", "Stock Analyzer", tags$small("Business Science")),
                        p(class = "text-center", "Please Log In")),
        login_title = "Enter"
    ),
    
    # Website
    uiOutput(outputId = "website")
)



# SERVER ----
server <- function(input, output, session) {
    
    # 0.0 READ DATA ----
    read_user_base()
    
    # 0.0 USER LOGIN ----
    
    
    # 0.1 Credentials ----
    credentials <- callModule(
        module   = shinyauthr::login, 
        id       = "login", 
        data     = user_base_tbl, 
        user_col = user, 
        pwd_col  = password, 
        log_out  = reactive(logout_init())
    )
    
    logout_init <- callModule(
        module = shinyauthr::logout,
        id     = "logout",
        active = reactive(credentials()$user_auth)
    )
    
    # 0.2 Instantiating User Information ----
    reactive_values <- reactiveValues()
    
    observe({
        if (credentials()$user_auth) {
            
            user_data_tbl <- credentials()$info
            
            reactive_values$permisions     <- user_data_tbl$permissions
            reactive_values$user_name      <- user_data_tbl$name
            reactive_values$favorites_list <- user_data_tbl %>% pull(favorites) %>% pluck(1)
            reactive_values$last_symbol    <- user_data_tbl$last_symbol
            reactive_values$user_settings  <- user_data_tbl$user_settings
        }    
    })
    
    output$creds <- renderPrint({
        list(
            reactive_values$permisions,
            reactive_values$user_name,
            reactive_values$favorites_list,
            reactive_values$last_symbol,
            reactive_values$user_settings
        )
    })
    
    
    # 1.0 SETTINGS ----
    
    # 1.1 Toggle Settings ----
    observeEvent(input$settings_toggle, {
        toggle(id = "input_settings", anim = TRUE)
    })
    
    # 1.2 Stock Symbol ----
    observeEvent(input$analyze, {
        update_and_write_user_base(
            user_name    = credentials()$info$user,
            column_name  = "last_symbol",
            assign_input =  get_symbol_from_user_input(input$stock_selection)
        )
    })
    
    stock_symbol <- eventReactive(input$analyze, {
        get_symbol_from_user_input(input$stock_selection)
    }, ignoreNULL = FALSE)
    
    # 1.3 User Input ----
    stock_selection_triggered <- eventReactive(input$analyze, {
        input$stock_selection
    }, ignoreNULL = FALSE) 
    
    # 1.4 Apply & Save Setting ----
    observeEvent(input$apply_and_save, {
        
        user_settings_tbl <- tibble(
            moving_avg_short = input$moving_avg_short,
            moving_avg_long  = input$moving_avg_long,
            time_window      = input$time_window
        )
        
        update_and_write_user_base(
            user_name    = credentials()$info$user,
            column_nmae  = "user_settings",
            assign_input = list(user_settings_tbl)
        )
    })
    
    moving_avg_short <- eventReactive(input$apply_and_save, {
        input$moving_avg_short
    }, ignoreNULL = FALSE)
    
    moving_avg_long <- eventReactive(input$apply_and_save, {
        input$moving_avg_long
    }, ignoreNULL = FALSE)
    
    time_window <- eventReactive(input$apply_and_save, {
        input$time_window
    }, ignoreNULL = FALSE)
    
    selected_tab <- eventReactive(input$apply_and_save, {
        # Selected Argument (tabset)
        if (is.character(input$tab_panel_stock_chart)) {
            # Tab already selected
            selected_tab <- input$tab_panel_stock_chart
            
        } else {
            # Tab panel not build yet
            selected_tab <- NULL
        }
        selected_tab
        
    }, ignoreNULL = FALSE)
    
    # 1.5 Get Stock Data ----
    stock_data_tbl <- reactive({
        stock_symbol() %>% get_stock_data(
            from = today() - time_window(), 
            to   = today(),
            moving_avg_short = moving_avg_short(),
            moving_avg_long  = moving_avg_long())
    })
    
    
    # 2.0 FAVORITE CARDS ----
    
    # 2.1 Reactive Values - User Favorites ----
    
    
    # 2.2 Add Favorites ----
    observeEvent(input$favorites_add, {
        
        new_symbol <- get_symbol_from_user_input(input$stock_selection)
        
        reactive_values$favorites_list <- c(reactive_values$favorites_list, new_symbol) %>% unique()
    })
    
    # 2.3 Render Favorite Cards ----
    output$favorite_cards <- renderUI({
        
        if (length(reactive_values$favorites_list) > 0) {
            generate_favorite_cards(
                favorites        = reactive_values$favorites_list,
                from             = today() - time_window(), 
                to               = today(),
                moving_avg_short = moving_avg_short(),
                moving_avg_long  = moving_avg_long()
            )
        }
    })
    
    # 2.4 Delete Favorites ----
    observeEvent(input$favorites_clear, {
        modalDialog(
            title     = "Clear Favorites", 
            size      = "m",
            easyClose = TRUE,
            
            p("Are you sure you want to remove favorites?"),
            br(),
            div(
                selectInput(inputId = "drop_list", 
                            label   = "Remove Single Favorite",
                            choices = reactive_values$favorites_list %>% sort()),
                
                actionButton(inputId = "remove_single_favorite",
                             label   = "Clear Single",
                             class   = "btn-warning"),
                
                actionButton(inputId = "remove_all_favorites",
                             label   = "Clear ALL",
                             class   = "btn-danger")
            ),
            
            footer = modalButton("Exit")
        ) %>% showModal()
    })
    
    # 2.4.1 Clear Single ----
    observeEvent(input$remove_single_favorite, {
        
        reactive_values$favorites_list <- reactive_values$favorites_list %>%
            .[reactive_values$favorites_list != input$drop_list]
        
        updateSelectInput(session = session, 
                          inputId = "drop_list", 
                          choices = reactive_values$favorites_list %>% sort())
    })
    
    # 2.4.2 Clear All ----
    observeEvent(input$remove_all_favorites, {
        
        reactive_values$favorites_list <- NULL
        
        updateSelectInput(session = session, 
                          inputId = "drop_list", 
                          choices = reactive_values$favorites_list %>% sort())
    })
    
    # 2.5 Show/Hide Favorites ----
    observeEvent(input$favorites_toggle, {
        shinyjs::toggle(id = "favorite_card_section", anim = TRUE, animType = "slide")
    })
    
    # 3.0 FAVORITE PLOTS ----
    
    # 3.1 Plot Header ----
    output$plot_header <- renderText({
        stock_selection_triggered() 
    })
    
    # 3.2 Plotly Plot ----
    output$plotly_plot <- renderPlotly({
        stock_data_tbl() %>% plot_stock_data()
    })
    
    
    # 3.3 Favorites Plots ----
    
    output$stock_charts <- renderUI({
        
        # First Tab Panel
        tab_panel_1 <- tabPanel(
            title = "Last Analysis",
            panel_card(
                title = stock_symbol(),
                plotlyOutput(outputId = "plotly_plot")
            )
        )
        
        # Favorite Panels
        favorite_tab_panels <- NULL
        if (length(reactive_values$favorites_list) > 0) {
            
            favorite_tab_panels <- reactive_values$favorites_list %>%
                map(.f = function(x) {
                    tabPanel(
                        title = x,
                        panel_card(
                            title = x,
                            x %>%
                                get_stock_data(
                                    from = today() - time_window(),
                                    to   = today(),
                                    moving_avg_short = moving_avg_short(),
                                    moving_avg_long  = moving_avg_long()
                                ) %>%
                                plot_stock_data()
                        )
                    )
                })
        }
        
        # Building the Tabset Panel
        do.call(
            what = tabsetPanel,
            args = list(tab_panel_1) %>%
                append(favorite_tab_panels) %>%
                append(list(id = "tab_panel_stock_chart", type = "pills", selected = selected_tab() ))
        )
        
    })
    
    # 4.0 COMMENTARY ----
    # 4.1 Generate Commentary ----
    output$analyst_commentary <- renderText({
        generate_commentary(data = stock_data_tbl(), user_input = stock_selection_triggered())
        
    })
    
    # 5.0 RENDER WEBSITE ----
    output$website <- renderUI({
        
        req(credentials()$user_auth)
        
        
        navbarPage(
            title = "Stock Analyzer", 
            inverse = FALSE, 
            collapsible = TRUE,
            
            theme = shinytheme(theme = 'cyborg'),
            
            header = div(
                class = "pull-right",
                style = "padding-right: 20px;",
                p("Welcome, ", reactive_values$user_name)
            ),
            
            tabPanel(
                title = "Analysis",
                
                # 5.1.0 HEADER ----
                div(
                    class = "container",
                    id    = "header",
                    h1(class = "page-header", "Stock Analyzer", tags$small("by Business Science")),
                    p(class = "lead", "This is the first mini-project completed in our", 
                      a(href = "https://www.business-science.io/", target = "_blank", "Expert Shiny Applications Course (DS4B 202-R)"))
                ),
                
                # 5.2.0 FAVORITES ----
                div(
                    class = "container hidden-sm, hidden-xs",
                    id = "favorite_container",
                    
                    # 5.2.1 INPUTS --- 
                    div(
                        class = "",
                        column(
                            width = 12,
                            h5(class = "pull-left", "Favorites"),
                            actionButton(class = "pull-right", inputId = "favorites_clear", "Clear Favorites"),
                            actionButton(class = "pull-right", inputId = "favorites_toggle", "Show/Hide"),
                        )
                    ),
                    
                    # 5.2.2 FAVORITE CARDS ---
                    div(
                        class = "row",
                        id = "favorite_cards_section", 
                        uiOutput(outputId = "favorite_cards",
                                 class = "container")
                    )
                ),
                
                
                # 5.3.0 APPLICATION UI -----
                div(
                    class = "container",
                    id    = "application_ui",
                    
                    # 5.3.1 USER INPUTS ----
                    column(
                        width = 4, 
                        wellPanel(
                            div(
                                id = "input_main",
                                pickerInput(
                                    inputId = "stock_selection", 
                                    label   = "Stock List (Pick One to Analyze)",
                                    choices = stock_list_tbl$label,
                                    multiple = FALSE, 
                                    selected = stock_list_tbl %>% 
                                        filter(label %>% str_detect(pattern = paste0(reactive_values$last_symbol, ","))) %>% 
                                        pull(label),
                                    options = pickerOptions(
                                        actionsBox = FALSE,
                                        liveSearch = TRUE,
                                        size = 10
                                    )
                                ) 
                            ),
                            div(
                                id = "input_buttons",
                                actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
                                div(
                                    class = "pull-right",
                                    actionButton(inputId = "favorites_add", label = NULL, icon = icon("heart")),
                                    actionButton(inputId = "settings_toggle", label = NULL, icon = icon("cog"))
                                )
                            ),
                            div(
                                id = "input_settings",
                                hr(),
                                sliderInput(inputId = "moving_avg_short", 
                                            label = "Short Moving Average (Days)", 
                                            value = reactive_values$user_settings %>% pluck(1) %>% pull(moving_avg_short),   
                                            min   = 5, 
                                            max   = 40),
                                
                                sliderInput(inputId = "moving_avg_long", 
                                            label = "Long Moving Average (Days)", 
                                            value = reactive_values$user_settings %>% pluck(1) %>% pull(moving_avg_long),   
                                            min   = 50, 
                                            max   = 120),
                                
                                sliderInput(inputId = "time_window", 
                                            label = "Time Window (Days)", 
                                            value = reactive_values$user_settings %>% pluck(1) %>% pull(time_window),   
                                            min   = 180, 
                                            max   = 730),
                                
                                actionButton(inputId = "apply_and_save", label = "Apply & Save", icon = icon("save"))
                            ) %>% hidden()
                        )
                    ),
                    
                    # 5.3.2 PLOT PANEL ----
                    
                    column(
                        width = 8, 
                        uiOutput(outputId = "stock_charts")
                    )
                ),
                
                # 5.4.0 ANALYST COMMENTARY ----
                div(
                    class = "container",
                    id    = "commentary",
                    column(
                        width = 12,
                        div(
                            class = "panel",
                            div(class = "panel-header", h4("Analyst Commentary")),
                            div(
                                class = "panel-body",
                                textOutput(outputId = "analyst_commentary")
                            )
                        )
                    ) 
                ) 
            ),
        ) 
    })
}

# RUN APP ----
shinyApp(ui = ui, server = server)
