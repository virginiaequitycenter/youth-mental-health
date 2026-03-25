# Script to pull and prep VDOE Staffing and Vacancy data for Virginia public 
# schools for the 2021-22 though 2024-25 school years. 
# Homepage: https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/positions-and-exits-collection

# Libraries ----
library(janitor)
library(tidyverse)

# For consistency on regions across datasets, we're using the regions data from:
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories/virginia-public-school-listing-by-region
regions <- read_csv("data/regions.csv")

# And for consistency across school names we're using school key from fall_membership.R:
new_school_names <-  read_csv("data/new_school_names.csv")

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
### State, division, and school ----
admin <- read_csv("data/raw/staffing_admin.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = tolower(level),
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    division_number = case_when(
      locality_grouping %in% c("division", "school") ~ str_pad(as.character(division_number), 3, pad = "0"),
      locality_grouping == "state" ~ NA),
    school_number = case_when(
      locality_grouping == "school" ~ str_pad(as.character(school_number), 4, pad = "0"),
      TRUE ~ NA),
    sch_id = case_when(
      locality_grouping == "school" ~paste0(division_number, school_number),
      TRUE ~ NA)) %>%
  select(-level, -adult_ed, -position_type, -school_number, -division_name) 

# Join with regions to standardize on region and division names: 
admin <- admin %>%
  left_join(regions, by = "division_number")

# Spot check missing values: 
na_div <- admin %>% filter(if_any(c(division_number, division_name, region_number, region_name), is.na)) #state

# Join with new_school_names to standardize on school names:
admin <- admin %>%
  left_join(new_school_names, by = "sch_id")

# If the school name isn't listed in the school_key, then just use the name from admin:
admin <- admin %>%
  mutate(
    school_name.y = coalesce(school_name.y, school_name.x)) %>%
  rename(school_name = school_name.y) %>%
  select(-school_name.x)

# Spot check missing values:
nas_sch <- admin %>% filter(
  locality_grouping == "school" &
  if_any(c(school_name), is.na)) #0

### Region ----
admin_reg <- read_csv("data/raw/staffing_admin_region.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = "region",
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    region_number = as.numeric(str_extract(region_number, "\\d+"))) %>%
  left_join(regions %>% select(-division_name, -division_number) %>% distinct()) %>%
  select(-adult_ed, -position_type)

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
### State, division, and school ----
noninstr <- read_csv("data/raw/staffing_noninstr.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = tolower(level),
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    division_number = case_when(
      locality_grouping %in% c("division", "school") ~ str_pad(as.character(division_number), 3, pad = "0"),
      locality_grouping == "state" ~ NA),
    school_number = case_when(
      locality_grouping == "school" ~ str_pad(as.character(school_number), 4, pad = "0"),
      TRUE ~ NA),
    sch_id = case_when(
      locality_grouping == "school" ~paste0(division_number, school_number),
      TRUE ~ NA)) %>%
  select(-level, -adult_ed, -position_type, -school_number, -division_name)

# Join with regions to standardize on region and division names:
noninstr <- noninstr %>%
  left_join(regions, by = "division_number")

# Spot check missing values:
na_div <- noninstr %>% filter(if_any(c(division_number, division_name, region_number, region_name), is.na)) #state

# Join with new_school_names to standardize on school names:
noninstr <- noninstr %>%
  left_join(new_school_names, by = "sch_id")
  
# If the school name isn't listed in the school_key, then just use the name from noninstr:
noninstr <- noninstr %>%
  mutate(
    school_name.y = coalesce(school_name.y, school_name.x)) %>%
  rename(school_name = school_name.y) %>%
  select(-school_name.x)
  
# Spot check missing values:
nas_sch <- noninstr %>% filter(
  locality_grouping == "school" &
    if_any(c(school_name), is.na)) #0

### Region ----
noninstr_reg <- read_csv("data/raw/staffing_noninstr_region.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = "region",
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    region_number = as.numeric(str_extract(region_number, "\\d+"))) %>%
  left_join(regions %>% select(-division_name, -division_number) %>% distinct()) %>%
  select(-adult_ed)

# Combine and Save ----  

staffing <- bind_rows(admin, admin_reg, noninstr, noninstr_reg) %>%
  select(-position_type)

write_csv(staffing, "data/staffing.csv")
