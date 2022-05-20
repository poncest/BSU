
# Libraries
library(pacman)
p_load(tidyverse, tidyquant, readxl)

# Load Data
# train
path_train <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'

train_raw_tbl <- read_excel(path_train, sheet = 1)

# # test
# path_test <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_test.xlsx'
# 
# test_tbl <- read_excel(path_test, sheet = 1)


# Data Subset
dept_job_role_tbl <- train_raw_tbl %>% 
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

dept_job_role_tbl

# 1. Business Science Problem Framework ----

# 1A. View Business as Machine ----

# BSU's Department and Job Role
# Define Objectives
# Asses Outcomes: TBD


dept_job_role_tbl %>% 
    group_by(Attrition) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(pct = n / sum(n))

# 1B. Understand the Drivers ----

# Investigate Objectives: 16% Attrition
# Synthesize Outcomes: High Counts and High Percentages
# Hypothesize Drivers: Job Role and Departments

# Department ----
dept_job_role_tbl %>% 
    
    group_by(Department, Attrition) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    
    group_by(Department) %>% 
    mutate(pct = n / sum(n)) %>% 
    ungroup()

# Job Role ----
dept_job_role_tbl %>% 
    
    group_by(Department, JobRole, Attrition) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    
    group_by(Department, JobRole) %>% 
    mutate(pct = n / sum(n)) %>% 
    ungroup() %>% 
     
    filter(Attrition %in% 'Yes')


# 1C. Measure the Drivers ----

# Collect Information on Employees Attrition: on going

# Develop KPI's: Industry KPI's 8.8%

dept_job_role_tbl %>% 
    
    group_by(Department, JobRole, Attrition) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    
    group_by(Department, JobRole) %>% 
    mutate(pct = n / sum(n)) %>% 
    ungroup() %>% 
    
    filter(Attrition %in% 'Yes') %>% 
    arrange(desc(pct)) %>% 
    
    mutate(above_industry_avg = case_when(
        pct > 0.088 ~ 'Yes',
        TRUE ~ 'No'
    ))
 
# 1.D Uncover Problems & Opportunities ----

calculate_attrition_cost <- function(
    
    # Employees
    n                         = 1,
    salary                    = 80000,
    
    # Direct Cost
    separation_cost           = 500,
    vacancy_cost              = 10000,
    acquisition_cost          = 4900,
    placement_cost            = 3500,
    
    # Productivity Cost
    net_revenue_per_employee  = 250000,
    workdays_per_year         = 240,
    workdays_position_open    = 40,
    workdays_onboarding       = 60,
    onboarding_efficiency     = 0.5
    
) {
    
    # Direct Costs
    direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
    
    # Lost Productivity Cost
    productivity_cost <- net_revenue_per_employee / workdays_per_year * (workdays_position_open + workdays_onboarding * onboarding_efficiency)
        
    # Savings of Salary & Benefits (Cost Reduction)
    salary_benefit_reduction <- salary / workdays_per_year *workdays_position_open
    
    # Estimated Turnover Per Employee
    cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
    
    # Total cost of Employee Turnover
    total_cost <- n * cost_per_employee
    
    return(total_cost)
 
} 

# g
calculate_attrition_cost()
calculate_attrition_cost(n=200)



