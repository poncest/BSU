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
    mutate_if(is.factor, as.numeric) %>%
    mutate(
        professional_development_strategy = case_when(
            
            # Ready For Rotation: YearsInCurrentRole, JobSatisfaction (LOW)
            
            
            # Ready For Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
            
            
            # Ready For Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
            
            
            # Ready For Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
            
            
            # Ready For Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
            
            
            # Incentivize Specialization: YearsInCurrentRole, JobSatisfaction, PerformanceRating
            
            
            # Catch All
            TRUE ~ "Retain and Maintain"
        )
    ) %>%
    pull(professional_development_strategy) %>%
    table()

tidy(recipe_obj, number = 3) %>%
    filter(str_detect(terms, "YearsInCurrentRole"))