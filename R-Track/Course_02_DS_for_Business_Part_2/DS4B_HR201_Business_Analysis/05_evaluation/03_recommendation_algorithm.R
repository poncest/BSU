# RECOMMENDATION ALGORITHM ----

# 1.0 Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)    # Make sure v0.1.3 or later is installed. If not restart & install.packages("recipes") to update.


# Load Data
path_train            <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'))
path_test             <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_test.xlsx'))
path_data_definitions <- (here::here('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_data_definitions.xlsx'))


train_raw_tbl         <- read_excel(path_train, sheet = 1)
test_raw_tbl          <- read_excel(path_test, sheet = 1)
definitions_raw_tbl   <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)



# Processing Pipeline
source(here::here("R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/data_processing_pipeline_rev1.R"))
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)



# 2.0 Correlation Analysis - Machine Readable ----
source(here::here("R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/plot_cor.R"))


# 2.1 Recipes ----
train_readable_tbl

# factor names
factor_names <- c("JobLevel", "StockOptionLevel")

# Recipe 
# modified the final recipe from `02_data_preparation_machine_readable.R`

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>% 
    step_zv(all_predictors()) %>% 
    step_mutate_at(factor_names, fn = as.factor) %>% 
    step_discretize(all_numeric(), min_unique = 1) %>%             # bins
    step_dummy(all_nominal(), one_hot = TRUE) %>% 
    prep()

recipe_obj

train_corr_tbl <- bake(object = recipe_obj, new_data = train_readable_tbl)
train_corr_tbl %>% glimpse()

tidy(recipe_obj)
tidy(recipe_obj, number = 3)
 

# 2.2 Correlation Visualization ----

# Manipulate Data

cor_level <- 0.06

correlation_results_tbl <- train_corr_tbl %>% 
    select(-Attrition_No) %>% 
    get_cor(target = Attrition_Yes, fct_reorder = TRUE, fct_rev = TRUE) %>% 
    filter(abs(Attrition_Yes) >= cor_level) %>% 
    mutate(
        relationship = case_when(
            Attrition_Yes > 0 ~ "Supports",
            TRUE              ~ "Contradicts"
        )
    ) %>% 
    
    mutate(feature_text = as.character(feature)) %>% 
    separate(feature_text, into = "feature_base", sep = "_", extra = "drop") %>% 
    mutate(feature_base = as_factor(feature_base) %>%  fct_rev())
    


# Create Visualization 

legnth_unique_groups <- correlation_results_tbl %>% 
    pull(feature_base) %>% 
    unique() %>% 
    length()


correlation_results_tbl %>% 
    ggplot(aes(Attrition_Yes, feature_base, color = relationship)) +
    
    # geoms
    geom_point() +
    geom_label(aes(label = feature, vjust = -0.5)) +
    
    #  scales
    expand_limits(x = c(-0.3, 0.3), y = c(1, legnth_unique_groups + 1)) +
    scale_x_continuous() +
    scale_y_discrete() + 
    scale_color_tq() +
    
    # labs
    labs(
        title = "Correlation Analysiss: Recommendation Strategy Development",
        subtitle = "Discretizing features to help idientify a strategy"
    ) +
    
    # themes
    theme_tq()

    

# 3.0 Recommendation Strategy Development Worksheet ----

# 4.0 Recommendation Algorithm Development ----

# 4.1 Strategy Group: Personal Development (Mentorship, Education) ----

# YearsAtCompany	
#   YAC -High - Likely to stay /  YAC - Low - Likely to leave	
#   Tie promotion if low to advance faster / Mentor if YAC low

# TotalWorkingYears	
#   TWY -High - Likely to stay /  TWYC - Low - Likely to leave	
#   Tie Low TWY to training & formation/mentorship

# YearsInCurrentRole	
#   More time in current role related to lower attrition	
#   Incentivize specialize or promote / Mentorship Role

# JobInvolvement	
#   High JI - Likely to stay / Low JI - Likely to leave	
#   Create personal development plan if low / High - seek leadership role	

# JobSatisfaction	
#   JS -High - Likely to stay /  JS - Low - Likely to leave	
#   Low - create personal development plan / High - mentorship roles

# PerformanceRating
#   Low - personal development plan / High - mentorship roles


# Good, Better, Best Approach

# (Worst Case) Create Personal Development Plan: JobInvolment, JobSatisfaction, PerformanceRating

# (Better Case) Promote Training and Formation: YearsAtCompany, TotalWorkingYears

# (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction

# (Best Case 2) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating


# verifying the level for `JobInvolvment` feature
train_readable_tbl %>% 
    pull(PerformanceRating) %>%                
    levels()


train_readable_tbl %>% 
    select(YearsAtCompany, TotalWorkingYears, YearsInCurrentRole,
           JobInvolvement, JobSatisfaction, PerformanceRating) %>% 
    
    # convert factors to numeric
    mutate(across(where(is.factor), as.numeric)) %>% 
    
    # Personal Development Recommendations
    mutate(
        personal_development_startegy = case_when(
            
            # (Worst Case) Create Personal Development Plan: JobInvolment, JobSatisfaction, PerformanceRating
            PerformanceRating == 1 |                                              # low
                JobSatisfaction == 1 |                                            # low
                JobInvolvement <= 2       ~ "Create Personal Development Plan",   # low & medium
            
            # (Better Case) Promote Training and Formation: YearsAtCompany, TotalWorkingYears
            YearsAtCompany < 3 |
                TotalWorkingYears < 6     ~ "Promote  Training Formation",
            
            # (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
            (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
                PerformanceRating >= 3 &
                JobSatisfaction == 4      ~ "Seek Mentorship Role",
            
            # (Best Case 2) Seek Leadership Role: JobInvolvement, JobSatisfaction, PerformanceRating
            JobInvolvement >= 3 &
                PerformanceRating >= 3 &
                JobSatisfaction >= 3      ~ "Seek Leadership Role",
            
            # Catch All
            TRUE                          ~ "Retain and Maintain"
        )
    ) 

    # pull(personal_development_startegy) %>% 
    # table()  
    
    
# hot to 'see' the levels for a feature
train_readable_tbl %>% 
    pull(PerformanceRating) %>% 
    levels()

# how to 'see' the bins
tidy(recipe_obj, number = 3) %>% 
    filter(str_detect(terms, "YearsAtCompany"))

tidy(recipe_obj, number = 3) %>% 
    filter(str_detect(terms, "TotalWorkingYears"))



# 4.2 Professional Development (Promotion Readiness) ----

# JobLevel
#   Employees with Job Level 1 are leaving / Job Level 2 staying
#   Promote faster for high performers

# YearsAtCompany
#   YAC - High - Likely to stay / YAC - LOW - Likely to leave
#   Tie promotion if low to advance faster / Mentor if YAC low

# YearsInCurrentRole
#   More time in current role related to lower attrition
#   Incentivize specialize or promote 

# Additional Features 
#   JobInvolvement - Important for promotion readiness, incentivizes involvment for leaders and early promotion
#   JobSatisfaction - Important for specialization, incentivizes satisfaction for mentors
#   PerformanceRating - Important for any promotion


# Good Better Best Approach

# Ready For Rotation: YearsInCurrentRole, JobSatisfaction (LOW)

# Ready For Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

# Ready For Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

# Ready For Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

# Ready For Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

# Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating


# Implement Strategy Into Code
train_readable_tbl %>%
    select(JobLevel, YearsInCurrentRole, 
           JobInvolvement, JobSatisfaction, PerformanceRating) %>%
   
    # convert factors to numeric
    mutate(across(where(is.factor), as.numeric)) %>% 
    
    mutate(
        professional_development_strategy = case_when(
            
            # Ready For Rotation: YearsInCurrentRole, JobSatisfaction (LOW)
            YearsInCurrentRole >= 2 &                                              
                JobSatisfaction <= 2       ~ "Ready for Rotation",   
            
            # Ready For Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
            JobLevel == 1 &
                YearsInCurrentRole >= 2 &
                JobInvolvement >= 3 &
                PerformanceRating >= 3      ~ "Ready for Promotion", 
            
            # Ready For Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
            JobLevel == 2 &
                YearsInCurrentRole >= 2 &
                JobInvolvement >= 4 &
                PerformanceRating >= 3      ~ "Ready for Promotion",
            
            # Ready For Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
            JobLevel == 3 &
                YearsInCurrentRole >= 3 &
                JobInvolvement >= 4 &
                PerformanceRating >= 3      ~ "Ready for Promotion",
            
            # Ready For Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
            JobLevel == 4 &
                YearsInCurrentRole >= 4 &
                JobInvolvement >= 4 &
                PerformanceRating >= 3      ~ "Ready for Promotion",
            
            # Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating
            YearsInCurrentRole >= 4 &
                JobSatisfaction >= 4 &
                PerformanceRating >= 3      ~ "Incentivize Specialization",
            
            # Catch All
            TRUE                            ~ "Retain and Maintain"
        )
    )
    # ) %>%
    # pull(professional_development_strategy) %>%
    # table() 

tidy(recipe_obj, number = 3) %>%
    filter(str_detect(terms, "YearsInCurrentRole"))


# 4.3 Work Environment Strategy ----

# EnvironmentSatisfaction
#   Employees with low environment satisfaction are more likely to leave	
#   Improve the workplace environment

# WorkLifeBalance
#   Bad worklife balance - more likely to leave	
#   Improve the workplace balance

# BusinessTravel
#   More BT- more like to leave / Less BT - more likely to stay	
#   Reduce business travel where possible

# DistanceFromHome
#   DFH - High -more likely to stay 	
#   Monitor worklife balance


# Good Better Best Approach

#

#

#

#

# Implement Strategy Into Code
train_readable_tbl %>%
    select(JobLevel, YearsInCurrentRole, 
           JobInvolvement, JobSatisfaction, PerformanceRating) %>%
    
    # convert factors to numeric
    mutate(across(where(is.factor), as.numeric)) %>% 
    
    mutate(
        professional_development_strategy = case_when(












