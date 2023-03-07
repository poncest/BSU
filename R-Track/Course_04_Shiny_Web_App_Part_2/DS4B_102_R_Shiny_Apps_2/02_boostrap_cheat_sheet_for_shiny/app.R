# BOOSTRAP CHEAT SHEET FOR SHINY ----
#DS4B 202-R ----


# LIBRARIES ----
library(pacman)
p_load(shiny, tidyverse, plotly, shinythemes)
p_load(shinyjs, DT, lubridate)


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
        
        # TOC ----
        h2("Table of Content"),
        tags$ul(
            tags$li("1.0 Boostrap Grid System" %>% a(href = "#section_01")),  
            tags$li("2.0 Working With Text" %>% a(href = "#section_02")), 
            tags$li("3.0 Text Alignment" %>% a(href = "#section_03")), 
            tags$li("4.0 Lists" %>% a(href = "#section_04")), 
            tags$li("5.0 Contextual Colors & Background" %>% a(href = "#section_05")), 
            tags$li("6.0 Buttons" %>% a(href = "#section_06")), 
            tags$li("7.0 Images" %>% a(href = "#section_07")), 
            tags$li("8.0 Thumbnails" %>% a(href = "#section_08")), 
            tags$li("9.0 Navbars" %>% a(href = "#section_09")), 
            tags$li("10.0 Navs" %>% a(href = "#section_10")), 
            tags$li("11.0 Sidebar Layout" %>% a(href = "#section_11")), 
            tags$li("12.0 Jumbotron" %>% a(href = "#section_12")), 
            tags$li("13.0 Panels" %>% a(href = "#section_12")), 
            tags$li("14.0 Mobile" %>% a(href = "#section_13")), 
            tags$li("15.0 CSS & Theme" %>% a(href = "#section_15")), 
            tags$li("16.0 JavaScript (ShinyJS)" %>% a(href = "#section_16")), 
        ),
        
        # TODO - add: (1) %>% a(href = "#section_01"), and (2) div(id = 'section_01'), to all sections
        
        ### 1.0 BOOTSTRAP GRID SYSTEM ----
        div(id = 'section_01'),
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
        div(id = 'section_02'),
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
        
        hr(),
        
        ### 3.0 TEXT ALIGNMENT ----
        div(id = 'section_03'),
        h2("3.0 Text Alignment"),
        
        div(
            class = "container", 
            id    = "text_alignment_1",
            p(class = "text_left text-lowercase", "Left-aligned Lowercase Text"),
            p(class = "text-center text-uppercase", "Center-aligned Uppercase Text"),
            p(class = "text-right text-capitalized", "Right-aligned capitalized Text")
        ), 
        
        div(
            class = "container", 
            id    = "text_alignment_2",
            fluidRow(
                p(class = "text_left text-lowercase", "Left-aligned Lowercase Text") %>% 
                    column(width = 4, class = "bg-primary"),
                p(class = "text-center text-uppercase", "Center-aligned Uppercase Text") %>% 
                    column(width = 4, class = "bg-success"),
                p(class = "text-right text-capitalized", "Right-aligned capitalized Text") %>% 
                    column(width = 4, class = "bg-info")
            )  
        ), 
        
        hr(),
        
        ### 4.0 LISTS ----
        div(id = 'section_04'),
        h2("4.0 Lists"),
        
        tags$ul(
            tags$li("Item 1"),
            tags$li("Item 2"),
            tags$li("Item 3"),
            tags$li("Item 4")
        ),
        
        tags$ol(
            tags$li("Item 1"),
            tags$li("Item 2"),
            tags$li("Item 3"),
            tags$li("Item 4")
        ),
        
        tags$ul(
            class = "list-inline", 
            tags$li("Item 1"),
            tags$li("Item 2"),
            tags$li("Item 3"),
            tags$li("Item 4")
        ),
        
        hr(),
        
        ### 5.0 CONTEXTUAL COLORS & BACKGROUNDS ----
        div(id = 'section_05'),
        h2("5.0 Contextual Colors & Background"),
        
        p(class = "text-primary", "Hello R"),
        p(class = "text-success", "Hello R"),
        p(class = "text-info", "Hello R"),
        p(class = "text-warning", "Hello R"),
        p(class = "text-danger", "Hello R"),
        
        p(class = "bg-primary", "Hello R"),
        p(class = "bg-success", "Hello R"),
        p(class = "bg-info", "Hello R"),
        p(class = "bg-warning", "Hello R"),
        p(class = "bg-danger", "Hello R"),
        
        hr(),
        
        ### 6.0 BUTTONS ----
        div(id = 'section_06'),
        h2("6.0 Buttons"),
       
        h3("a. Contextual Buttons"),
        div(
            class = "container",
            a(href = "https://www.business-science.io/", class = "btn btn-default", "Go to Business Science"),
            a(href = "https://www.business-science.io/", class = "btn btn-primary", "Go to Business Science"),
            a(href = "https://www.business-science.io/", class = "btn btn-success", "Go to Business Science"),
            a(href = "https://www.business-science.io/", class = "btn btn-warning", "Go to Business Science"),
            a(href = "https://www.business-science.io/", class = "btn btn-danger", "Go to Business Science"),
            a(href = "https://www.business-science.io/", class = "btn btn-info", "Go to Business Science")
        ),
       
        br(),
        
        h3("b. Sizing Buttons"),
        div(
            class = "container",
            a(href = "https://www.business-science.io/", class = "btn btn-default btn-lg", "Go to Business Science"),
            a(href = "https://www.business-science.io/", class = "btn btn-primary btn-md", "Go to Business Science"),
            a(href = "https://www.business-science.io/", class = "btn btn-success btn-sm", "Go to Business Science"),
            a(href = "https://www.business-science.io/", class = "btn btn-warning btn-xs", "Go to Business Science")
        ),
        
        br(),
        
        h3("c. Shiny Buttons"),
        div(
            class = "container",
            shiny::actionButton(
                inputId = "btn_1", 
                label   = "Shiny Button - Click Me!",
                class   = "btn-primary btn lg", 
                icon    = icon("sync", class = "fa-1x", lib = "font-awesome")),
            
            shiny::textOutput(outputId = "btn_1_txt")
            
        ),
        
        hr(),
        
        ### 7.0 IMAGES ----
        div(id = 'section_07'),
        h2("7.0 Images"),
        div(
            class = "container",
            column(width = 4, 
                   img(class = "thumbnail img-responsive", src = "business-science-logo.png", style = "width:200px")),
            
            column(width = 4,
                   img(class = "img-rounded img-responsive", src = "business-science-logo.png", style = "width:200px")),
            
            column(width = 4,
                   img(class = "img-circle img-responsive", src = "matt-pic.jpg", style = "width:200px"))
        ),
          
        hr(),
        
        ### 8.0 THUMNAILS ----
        div(id = 'section_08'),
        h2("8.0 Thumbnails"),
        fluidRow(
            column(width = 4,
                   div(
                       class = "thumbnail text-center",
                       #style = "paddding: 20px",
                       img(class = "img-rounded img-responsive", src = "matt-pic.jpg"),
                       h3("Thumbnail Label"),
                       p("Text about this thumbnail"),
                       a(class = "btn btn-primary btn-sm", href = "#", "Learn More")
                   )
               ), 
            column(width = 4,
                   div(
                       class = "thumbnail text-center",
                       #style = "paddding: 20px",
                       img(class = "img-rounded img-responsive", src = "matt-pic.jpg"),
                       h3("Thumbnail Label"),
                       p("Text about this thumbnail"),
                       a(class = "btn btn-primary btn-sm", href = "#", "Learn More")
                   )
            ),
            column(width = 4,
                   div(
                       class = "thumbnail text-center",
                       style = "paddding: 20px",
                       img(class = "img-rounded img-responsive", src = "matt-pic.jpg"),
                       h3("Thumbnail Label"),
                       p("Text about this thumbnail"),
                       a(class = "btn btn-primary btn-sm", href = "#", "Learn More")
                   )
            ),
        ),
        
        
        hr(),
         
        ### 9.0 NAVBARS ----
        div(id = 'section_09'),
        h2("9.0 Navbars"),
        navbarPage(title = "Business Science", inverse = TRUE, collapsible = TRUE,
                   tabPanel(title = "What is Shiny?", value = "page_1",
                            h1("What is Shiny?", tags$small("A Framework for Building Web Apps with R")),
                            p("All of the cool feastures of shiny")
                   ),
                   tabPanel(title = "What is Boostrap?", value = "page_2",
                            h2("What is Boostrap?", tags$small("A Web Framework that Extends HTML and CSS")),
                            p("All of the cool feastures of boostrap")
                   ),
                   navbarMenu(title = "Using Shiny and Booststrap", 
                              tabPanel(title = "Make Plots"),
                              tabPanel(title = "Add Shiny Components"),
                              "-----",
                              tabPanel(title = "More Info")
                              )
        ),
        
        
        hr(),
        
        ### 10.0 NAVS ----
        div(id = 'section_10'),
        h2("10.0 Navs"),
        
        
        h3("Tabset Panel"),
        tabsetPanel(
            id = "tabset_1", type = 'tabs', 
            tabPanel(title = "Shiny", h3("What is Shiny?"), p("Shiny is awesome")),
            tabPanel(title = "Boostrap", h3("What is Boostrap?"), p("Boostrap is awesome")),
        ),
        
        h3("Pills"),
        tabsetPanel(
            id = "tabset_2", type = 'pills', 
            tabPanel(title = "Shiny", h3("What is Shiny?"), p("Shiny is awesome")),
            tabPanel(title = "Boostrap", h3("What is Boostrap?"), p("Boostrap is awesome")),
        ),
        
        br(),
        
        h3("Navlist Panel"),
        navlistPanel(
            id = "navlist_1", 
            tabPanel(title = "Shiny", h3("What is Shiny?"), p("Shiny is awesome")),
            tabPanel(title = "Boostrap", h3("What is Boostrap?"), p("Boostrap is awesome")),
        ),
        
        hr(),
        
        ### 11.0 SIDEBAR LAYOUT ----
        div(id = 'section_11'),
        h2("11.0 Sidebar Layout"),
        
        sidebarLayout(
            sidebarPanel = sidebarPanel(
                width = 3,
                p("UI Elements Go Here"), 
                shiny::dateRangeInput(inputId = "date_range_1", label = "Enter a Date Range")
                ),
            mainPanel = mainPanel("Plot Elements and Analysis Go Here", width = 9),
            ),
        
        hr(),
        
        ### 12.0 JUMBOTRON ----
        div(id = 'section_12'),
        h2("12.0 Jumbotron"),
        
        div(
            class = "jumbotron", 
            #style = "background:url('business-science-logo.png');background-size:cover;",
            div(
                lass = "container",
                style = "background-color:#9999999c",
                h1("Learning Shiny"),
                p("It's your solution for building web applications with ", code("R")),
                "Learn More" %>% a(class ="btn btn-lg btn-primary", href = "#") %>% p()
            )
        ),  
        
        hr(),
        
        ### 13.0 PANELS ----
        div(id = 'section_13'),
        h2("13.0 Panels"),
        div(
            class = "panel panel-primary",
            div(
                class = "panel-heading",
                h3("Chart Title - Scatterplot of MPG vs. Wt.")
            ),
            div(
                class = "panel-body bg-info",
                p("Insert Chart"),
                plotlyOutput(outputId = "mtcars"),
            ),
            div(
                class = "panel-footer bg-info",
                style = "background-color: #d9edf7;",
                p("Footer - Caption: This is a plot of vehicle weight vs. fuel efficiency") %>% tags$small()
            )
        ),
        
        hr(),
        
        ### 14.0 MOBILE ----
        div(id = 'section_14'),
        h2("14.0 Mobile"),

        fluidRow(
            class = "hidden-xs",
            div(
                class="jumbotron", 
                h1("Learning Shiny"),
                p("lead", "Will help you distribute interactive data products"),
                a(class = "btn bt-primary, btn-lg", href = "#", "Learn more")
                )
             ),
        
        fluidRow(
            class = "hidden-sm hidden-md hidden-lg",
            div(
                class = "thumbnail text-center",
                img(class = "img-responsive", style = "width:200px;", src = "business-science-logo.png"),
                h3("Learning Shiny"),
                p(class = "lead", "Will help you distribute interactive data products."),
                a(class = "btn btn-primary btn-sm", href = "#", "Learn more")
            )
        ),
        
        hr(),
        
        ### 15.0 CSS & THEME ----
        div(id = 'section_15'),
        h2("15.0 CSS & Theme"),
        
        fluidPage(
            theme = shinytheme("flatly"),
            # themeSelector()  # good for testing
            
            # using 'sketchy theme from bootswatch (https://bootswatch.com/sketchy/)
            tags$head(
                # tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
                tags$link(rel = "stylesheet", type = "text/css", href = "my_styles.css")
                     )
            ),
        
        hr(),
        
        ### 16.0 JAVASCRIPT ----
        div(id = 'section_16'),
        h2("16.0 JavaScript (ShinyJS)"),
        
       useShinyjs(),
       fluidRow(
           column(
               width = 4,
               class = "well",
               
               # Toggle Button
               shiny::actionButton(inputId = "toggle_form", label = "Toggle Form"),
               
               # Controls
               div(
                   id = 'controls',
                   br(),
                   shiny::textInput(inputId = "first_name", label = "First Name", placeholder = "Enter your first name"),
                   shiny::textInput(inputId = "email", label = "Email", placeholder = "Enter your email"),
                   shiny::actionButton(inputId = "submit_form", label = "Submit")
               ) %>% shinyjs::hidden(),
               
               # Thank You
               div(
                   id = "thank_you",
                   br(),
                   div(
                       class = "alert alert-success",
                       role  = "alert",
                       p("Thank you!", shiny::actionButton(inputId = "close_alert", label = "X", class = "pull-right btn-xs"))
                   )
               )%>% shinyjs::hidden()
                                                  
           ),
           column(
               width = 8,
               DTOutput(outputId = "new_user_dt")
           ) 
       ),
        
        
        div(style = "height: 400px;")
            
        ) 
      )
    


# SERVER ----
server <- function(input, output, session) {
    
    counter <<- 0
    btn_1_click <- eventReactive(input$btn_1, {
        TRUE
        counter <<- counter + 1
    })
    
    output$btn_1_txt <- renderText({
        if(btn_1_click()) {
            str_glue("You click me {counter} times!")
        } 
    })
    
    # Plot mtcars
    output$mtcars <- renderPlotly({
        g <- mtcars %>% 
            ggplot(aes(wt, mpg)) +
            geom_point()
        
        ggplotly(g)
    })
    
    # 16.0 ShinyJS ----
    
    # toggle form
    shinyjs::onclick(id = "toggle_form", {
        shinyjs::toggle(id = "controls", anim = TRUE)
    })
    
    # submit button
    observe({
        shinyjs::toggleState(id = "submit_form", condition = {
            !(input$first_name == "") && !(input$email == "")
        })
    })
    
    new_user_tbl <- eventReactive(eventExpr = input$submit_form, {
        new_user_tbl <- tibble(
            first_name = input$first_name,
            email      = input$email,
            timestamp  = lubridate::now()
        )
    })
    
    output$new_user_dt <- renderDataTable({
        new_user_tbl() %>% datatable()
    })
    
    # show thank you
    observeEvent(eventExpr = input$submit_form, {
        shinyjs::toggle(id = "thank_you", anim = TRUE, animType = "fade", time = 0.25)
    })
    
    # hide thank you
    shinyjs::onclick(id = "close_alert", {
        shinyjs::toggle(id = "thank_you", anim = TRUE, animType = "fade", time = 0.25)
    })
}

shinyApp(ui = ui, server = server)

