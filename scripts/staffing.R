# Script to pull and prep VDOE Staffing and Vacancy data for Virginia public 
# schools for the 2021-22 though 2024-25 school years. 
# Homepage: https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/positions-and-exits-collection

# Libraries ----
library(janitor)
library(tidyverse)

# To standardize on region names across datasets:
regions <- read_csv("data/vdoe_regions.csv")

# This data is available to download using the VDOE Build-a-Table functionality: 
# https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=320 

# Administrative Positions ----
## Download ----
# Build-a table-criteria: STATE, DIVISION, SCHOOL
# - School years: All (2024-25, 2023-24, 2022-23, and 2021-22)
# - Report level: State, Division, and School
# - Position Category: Personnel
# - Position Type: Administration
# - Position Description: School Counselor (Elem., Middle, High), Licensed Behavior 
#   Analysts, Licensed Assistant Behavior Analysts, Other Licensed Health and 
#   Behavior Analysis 
# - English Learners: All
# - Adult Education: No 
# - Special Education: All

# Build-a table-criteria: REGION
# - School years: All (2024-25, 2023-24, 2022-23, and 2021-22)
# - Report level: State, Division, and School
# - Position Category: Personnel
# - Position Type: Administration
# - Position Description: School Counselor (Elem., Middle, High), Licensed Behavior 
#   Analysts, Licensed Assistant Behavior Analysts, Other Licensed Health and 
#   Behavior Positions
# - English Learners: All
# - Adult Education: No 
# - Special Education: All

## Tidy ----
admin <- read_csv("data/raw/staffing_admin.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = tolower(level),
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled))) %>%
  select(-level, -adult_ed)

admin_reg <- read_csv("data/raw/staffing_admin_region.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = "region",
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    region_number = as.numeric(str_extract(region_number, "\\d+"))) %>%
  left_join(regions %>% select(-division_name), by = "region_number") %>%
  select(-adult_ed) %>%
  distinct()

# Non-Instructional Positions ----
## Download ----
# Build-a table-criteria: STATE, DIVISION, SCHOOL
# - School years: All (2024-25, 2023-24, 2022-23, and 2021-22)
# - Report level: State, Division, and School
# - Position Category: Personnel
# - Position Type: Non-Instructional Personnel
# - Position Description: Student Support Services, Psychologists, and School Social Workers
# - English Learners: All
# - Adult Education: No 
# - Special Education: All

# Build-a table-criteria: REGION
# - School years: All (2024-25, 2023-24, 2022-23, and 2021-22)
# - Report level: State, Division, and School
# - Position Category: Personnel
# - Position Type: Non-Instructional Personnel
# - Position Description: Student Support Services, Psychologists, and School Social Workers
# - English Learners: All
# - Adult Education: No 
# - Special Education: All

## Tidy ----
noninstr <- read_csv("data/raw/staffing_noninstr.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = tolower(level),
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled))) %>%
  select(-level, -adult_ed)

noninstr_reg <- read_csv("data/raw/staffing_noninstr_region.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = "region",
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    region_number = as.numeric(str_extract(region_number, "\\d+"))) %>%
  left_join(regions %>% select(-division_name), by = "region_number") %>%
  select(-adult_ed) %>%
  distinct()

# Combine and Save ----  

staffing <- bind_rows(admin, admin_reg, noninstr, noninstr_reg)
write_csv(staffing, "data/staffing.csv")
