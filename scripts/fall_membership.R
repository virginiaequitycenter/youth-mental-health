# Script to pull and prep Fall Membership (Enrollment) data from the Virginia
# Department of Education from the 2022 to 2024 school years. 
# Homepage: https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304

# Because this data cannot be downloaded programatically, we need to use the VDOE 
# build-a-table functionality to create and download the data as CSVs.  

# Libraries ----
library(tidyverse)
library(janitor)

# Division ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: Division
#    - Divisions: All
# - Student Characteristics: All 
# - Reporting Categories: All

enroll_div <- read_csv("data/raw/fall_membership_statistics_division.csv") %>%
  clean_names()

vdoe_regions <- read_csv("data/vdoe_regions.csv")

# Join with regions (to get regional summaries)
enroll_div <- enroll_div %>%
  left_join(vdoe_regions, by = join_by(division_name == district_name))

# Deal with districts that have missing regions
missing <- enroll_div %>%
  filter(is.na(region_name))

enroll_div <- enroll_div %>%
  mutate(
    region_name = case_when(
      division_name %in% c("Alleghany Highlands", "Alleghany County") ~ "Western Virginia",
      division_name %in% c("Colonial Beach", "West Point") ~ "Northern Neck",
      division_name == "Williamsburg-James City County" ~ "Tidewater", 
      TRUE ~ region_name),
    region_number = case_when(
      division_name %in% c("Alleghany Highlands", "Alleghany County") ~ 6,
      division_name %in% c("Colonial Beach", "West Point") ~ 3,
      division_name == "Williamsburg-James City County" ~ 2,
      TRUE ~ region_number),
    GEOID = case_when(
      division_name %in% c("Alleghany Highlands", "Alleghany County") ~ 51005,
      division_name == "Colonial Beach" ~ 51193,
      division_name == "West Point" ~ 51101,
      division_name == "Williamsburg-James City County" ~ 51830, #VDOE defaults to Williamsburg 
      TRUE ~ GEOID),
    locality_grouping = "division")

# Region ----
enroll_region <- enroll_div %>%
  group_by(school_year, region_name, region_number) %>%
  summarise(total_count = sum(total_count, na.rm = T),
            ft_count = sum(ft_count, na.rm = T),
            pt_count = sum(pt_count, na.rm = T)) %>%
  mutate(locality_grouping = "region")

# School ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: School
#    - Divisions: All
#    - Schools: All
# - Student Characteristics: All 
# - Reporting Categories: All

enroll_school <- read_csv("data/raw/fall_membership_statistics_school.csv") %>%
  clean_names() %>%
  left_join(vdoe_regions, by = join_by(division_name == district_name)) %>%
  mutate(locality_grouping = "school")

# State ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: State
# - Student Characteristics: All 
# - Reporting Categories: All

enroll_state <- read_csv("data/raw/fall_membership_statistics_state.csv") %>%
  clean_names() %>%
  mutate(locality_grouping = "state")

# Combine & Save ----
fall_membership <- bind_rows(enroll_div, enroll_region, enroll_school, enroll_state)

write_csv(fall_membership, "data/fall_membership.csv")
