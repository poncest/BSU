# RECOMMENDATION ALGORITHM ----

# 1.0 Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)    # Make sure v0.1.3 or laters is installed. If not restart & install.packages("recipes") to update.


# Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline_rev1.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)



# 2.0 Correlation Analysis - Machine Readable ----
source("00_Scripts/plot_cor.R")

# 2.1 Recipes ----


# 2.2 Correlation Visualization ----




# 3.0 Recommendation Strategy Development Worksheet ----




# 4.0 Recommendation Algorithm Development ----

# 4.1 Personal Development (Mentorship, Education) ----


# 4.2 Professional Development (Promotion Readiness) ----


# 4.3 Work Life Balance ----




