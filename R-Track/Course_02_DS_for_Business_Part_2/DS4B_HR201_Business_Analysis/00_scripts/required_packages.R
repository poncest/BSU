

# Specify your packages
my_packages <- c(
    "h2o",        # High performance machine learning
    "lime",       # Explaining black-box models
    "recipes",    # Creating ML preprocessing recipes
    "tidyverse",  # Set of pkgs for data science: dplyr, ggplot2, purrr, tidyr, ...
    "tidyquant",  # Financial time series pkg - Used for theme_tq ggplot2 theme
    "glue",       # Pasting text
    "cowplot",    # Handling multiple ggplots
    "GGally",     # Data understanding - visualizations
    "skimr",      # Data understanding - summary information
    "fs",         # Working with the file system - directory structure
    "readxl",     # Reading excel files
    "writexl"     # Writing to excel files
)


# Extract not installed packages                    
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    

# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                               

