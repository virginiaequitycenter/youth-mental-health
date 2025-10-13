# Script to pull and prep Fall Membership (Enrollment) data from the Virginia
# Department of Education from the 2022 to 2024 school years. 
# Homepage: https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304

# Because this data cannot be downloaded programatically, we need to use the VDOE 
# build-a-table functionality to create and download the data as CSVs.  

# Libraries ----
library(tidyverse)
library(janitor)

# For consistency on regions across datasets, we're using the regions data from:
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories/virginia-public-school-listing-by-region

regions <- read_csv("data/vdoe_regions_divisions.csv")

# Division ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: Division
#    - Divisions: All
# - Student Characteristics: All 
# - Reporting Categories: All

enroll_div <- read_csv("data/raw/fall_membership_statistics_division.csv") %>%
  clean_names() %>%
  left_join(regions, by = c("division_number", "division_name"))

# Deal with districts that have missing regions due to combined city/county districts 
missing <- enroll_div %>%
  filter(is.na(region_name)) # Alleghany Highlands and Covington City 

enroll_div <- enroll_div %>%
  mutate(
    region_name = case_when(
      division_name %in% c("Alleghany Highlands", "Covington City") ~ "Western Virginia",
      TRUE ~ region_name),
    region_number = case_when(
      division_name %in% c("Alleghany Highlands", "Alleghany County") ~ 6,
      TRUE ~ region_number),
    locality_grouping = "division")

# Region ----
enroll_region <- enroll_div %>%
  group_by(school_year, region_name) %>%
  summarise(total_count = sum(total_count),
            ft_count = sum(ft_count, na.rm = T),
            pt_count = sum(pt_count, na.rm = T),
            region_number = max(region_number, na.rm = T)) %>%
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
  clean_names() %>% left_join(regions, by = c("division_name", "division_number")) %>%
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
