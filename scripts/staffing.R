# Script to pull and prep VDOE Staffing and Vacancy data for Virginia public 
# schools for the 2021-22 though 2024-25 school years. 
# Homepage: https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/positions-and-exits-collection

# Libraries ----
library(janitor)
library(tidyverse)

# Administrative Positions ----
# This data is available to download using the VDOE Build-a-Table functionality: 
# https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=320 

## Download ----
# Build-a table-criteria: State, Region, School
# - School years: All (2024-25, 2023-24, 2022-23, and 2021-22)
# - Report level: State, Division, and School
# - Position Category: Personnel
# - Position Type: Administration
# - Position Description: School Counselor (Elem., Middle, High), Licensed Behavior 
#   Analysts, Licensed Assistant Behavior Analysts, Other Licensed Health and 
#   Behavior Analysis 
# - English Learners: Yes
# - Adult Education: No 
# - Special Education: Yes

## Tidy ----
admin <- read_csv("data/raw/staffing_admin.csv") %>%
  clean_names()
