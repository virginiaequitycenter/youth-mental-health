# Script to pull and prep VDOE Staffing and Vacancy data for Virginia public 
# schools for the 2021-22 though 2024-25 school years. 
# Homepage: https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/positions-and-exits-collection

# Libraries ----
library(janitor)
library(tidyverse)

# To standardize on region names across datasets:
regions <- read_csv("data/vdoe_regions_divisions.csv")

# And for consistency across school names we're using the school key from fall_membership.R:
school_key <- read_csv("data/school_key.csv")

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
# State, division, and school levels 
admin <- read_csv("data/raw/staffing_admin.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = tolower(level),
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    division_number = case_when(
      locality_grouping %in% c("division", "school") ~ str_pad(as.character(division_number), 3, pad = "0"),
      locality_grouping == "state" ~ NA),
    division_name = case_when(
      division_number == "003" ~ "Alleghany County",
      TRUE ~ division_name),
    school_number = case_when(
      locality_grouping == "school" ~ str_pad(as.character(school_number), 4, pad = "0"),
      TRUE ~ NA),
    sch_id = case_when(
      locality_grouping == "school" ~paste0(division_number, school_number),
      TRUE ~ NA)
  ) %>%
  select(-level, -adult_ed)

# Join with school key 
admin <- admin %>%
  left_join(school_key)

# Region levels
admin_reg <- read_csv("data/raw/staffing_admin_region.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = "region",
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    region_number = as.numeric(str_extract(region_number, "\\d+"))) %>%
  left_join(regions %>% select(-division_name) %>% distinct()) %>%
  select(-adult_ed)

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
# State, division, and school levels
noninstr <- read_csv("data/raw/staffing_noninstr.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = tolower(level),
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    division_number = case_when(
      locality_grouping %in% c("division", "school") ~ str_pad(as.character(division_number), 3, pad = "0"),
      locality_grouping == "state" ~ NA),
    division_name = case_when(
      division_number == "003" ~ "Alleghany County",
      TRUE ~ division_name),
    school_number = case_when(
      locality_grouping == "school" ~ str_pad(as.character(school_number), 4, pad = "0"),
      TRUE ~ NA),
    sch_id = case_when(
      locality_grouping == "school" ~paste0(division_number, school_number),
      TRUE ~ NA)
  ) %>%
  select(-level, -adult_ed)

# Join with school key
noninstr <- noninstr %>%
  left_join(school_key)

# Region levels
noninstr_reg <- read_csv("data/raw/staffing_noninstr_region.csv") %>%
  clean_names() %>%
  mutate(
    locality_grouping = "region",
    percent_unfilled = as.numeric(gsub("%", "", percent_unfilled)),
    region_number = as.numeric(str_extract(region_number, "\\d+"))) %>%
  left_join(regions %>% select(-division_name) %>% distinct()) %>%
  select(-adult_ed)


# Combine and Save ----  

staffing <- bind_rows(admin, admin_reg, noninstr, noninstr_reg)
write_csv(staffing, "data/staffing.csv")
