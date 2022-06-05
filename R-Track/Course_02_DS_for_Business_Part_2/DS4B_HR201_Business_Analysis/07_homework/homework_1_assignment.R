# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)


# Source Scripts ----
source("./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_scripts/assess_attrition.R")

# Data ----
path_train <- './R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/telco_train.xlsx'

train_raw_tbl <- read_excel(path_train, sheet = 1)


dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----
productivity_cost_by_role_tbl <- read_excel('./R-Track/Course_02_DS_for_Business_Part_2/DS4B_HR201_Business_Analysis/00_data/productivity_cost_by_role.xlsx')

productivity_cost_by_role_tbl


# Q1: Which Job Role has the highest total cost of attrition? ----
dept_jobrole_productivity_tbl <- dept_jobrole_tbl %>% 
    count(Department, JobRole, Attrition) %>% 
    count_to_pct(Department, JobRole) %>% 
    assess_attrition(Attrition, 'Yes', 
                     baseline_pct = kpi_industry_turnover_pct) %>% 
    
    # left join
    left_join(productivity_cost_by_role_tbl, by = c('Department', 'JobRole')) %>% 
    
    # attrition cost
    mutate(attrition_cost = calculate_attrition_cost(n = n,
                                                     salary = Salary_Average,
                                                     net_revenue_per_employee = Revenue_Average))

dept_jobrole_productivity_tbl %>% 
    plot_attrition(Department, JobRole, .value = attrition_cost)

## Answer: Sales: Sales Executive - $4.27 M


# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----

dept_jobrole_productivity_tbl %>% 
    plot_attrition(Department, JobRole, .value = attrition_cost, units = 'M')

## Answer: Research & Development: Research Scientist - $2.28 M

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----
dept_jobrole_productivity_tbl %>% 
    
    # data manipulation 
    arrange(desc(attrition_cost)) %>% 
    mutate(row_num  = row_number()) %>% 
    mutate(is_top_4 = case_when(
        row_num <= 4 ~ 'Yes',
        TRUE         ~ 'No'
    )) %>% 
    
    # summarize cost by top-4
    group_by(is_top_4) %>% 
    summarise(total_attrition_cost = sum(attrition_cost)) %>% 
    ungroup() %>% 
    
    # calculate % of total
    mutate(total_attrition_pct = total_attrition_cost / sum(total_attrition_cost))
    

# ALTERNATIVE
dept_jobrole_productivity_tbl %>% 
    mutate(percent_attrition = attrition_cost / sum(attrition_cost)) %>% 
    slice_max(attrition_cost, n = 4) %>% 
    mutate(cumulative_pct_sum = cumsum(percent_attrition)) %>% 
    select(Department, JobRole, attrition_cost, cumulative_pct_sum)


# Q4. Which Department has the highest total cost of attrition? ----



# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----


