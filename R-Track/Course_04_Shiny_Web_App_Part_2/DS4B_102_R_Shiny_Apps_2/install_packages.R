# BUSINESS SCIENCE ----
# DS4B 202-R ----
# PACKAGE INSTALLATION -----
# Version 1

# CRAN PACKAGES ----
main_packages_cran <- c(
    "shiny",
    "shinyWidgets",
    "shinythemes",
    "shinyjs",
    # shinyauthr, # devtools::install_github("business-science/shinyauthr")
    
    "mongolite",
    "jsonlite",
    "config",
    
    "plotly",
    "tidyquant",
    "tidyverse",
    
    "devtools",
    "diffr"
)

install.packages(main_packages_cran)

# DEVELOPMENT PACKAGES ----
devtools::install_github("business-science/shinyauthr")

