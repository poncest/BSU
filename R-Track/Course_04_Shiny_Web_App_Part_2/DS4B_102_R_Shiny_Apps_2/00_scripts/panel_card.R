panel_card <- function(title, ..., footer = NULL) {
    
    ftr <- NULL
    if (!is.null(footer)) ftr <- div(class = "panel-footer", footer)
    
    div(
        class = "panel", 
        div(
            class = "panel-header",
            h4(title)
        ),
        div(
            class = "panel-body",
            ...
        ),
        ftr
    )
    
}

# Testing panel_card()
# panel_card(title = "My Title", 
#            p("This is some text."), 
#            footer = tags$small("Caption: The is my panel_card() function"))
