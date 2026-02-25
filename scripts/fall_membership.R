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
## All Students ----
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

## Economically Disadvantaged Students ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: Division
#    - Divisions: All
# - Student Characteristics: All 
# - Reporting Categories: 
#     - Disadvantaged: Yes

disadv_div <- read_csv("data/raw/disadv_div_raw.csv") %>%
  clean_names() %>%
  left_join(regions, by = c("division_number", "division_name"))

# Deal with districts that have missing regions due to combined city/county districts 
missing <- disadv_div %>%
  filter(is.na(region_name)) # Alleghany Highlands and Covington City 

disadv_div <- disadv_div %>%
  mutate(
    region_name = case_when(
      division_name %in% c("Alleghany Highlands", "Covington City") ~ "Western Virginia",
      TRUE ~ region_name),
    region_number = case_when(
      division_name %in% c("Alleghany Highlands", "Alleghany County") ~ 6,
      TRUE ~ region_number),
    locality_grouping = "division") %>%
  select(-disadvantaged, -ft_count, -pt_count, n_disadv_students = total_count)

# Join
enroll_div <- left_join(enroll_div, disadv_div)

# Region ----
# Not available for download, so manually calculated:
enroll_reg <- enroll_div %>%
  group_by(school_year, region_name) %>%
  summarise(total_count = sum(total_count),
            ft_count = sum(ft_count, na.rm = T),
            pt_count = sum(pt_count, na.rm = T),
            n_disadv_students = sum(n_disadv_students, na.rm = T),
            region_number = max(region_number, na.rm = T)) %>%
  mutate(locality_grouping = "region")

# School ----
## All Students ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: School
#    - Divisions: All
#    - Schools: All
# - Student Characteristics: All 
# - Reporting Categories: All

enroll_sch <- read_csv("data/raw/fall_membership_statistics_school.csv") %>%
  clean_names() %>% left_join(regions, by = c("division_name", "division_number")) %>%
  mutate(locality_grouping = "school")

## Economically Disadvantaged ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: School
#    - Divisions: All
#    - Schools: All
# - Student Characteristics: All 
# - Reporting Categories: 
#     - Disadvantaged: Yes

disadv_sch <- read_csv("data/raw/disadv_sch_raw.csv") %>%
  clean_names() %>% left_join(regions, by = c("division_name", "division_number")) %>%
  mutate(locality_grouping = "school") %>%
  select(-disadvantaged, -ft_count, -pt_count, n_disadv_students = total_count)

enroll_sch <- left_join(enroll_sch, disadv_sch) %>%
  mutate(n_disadv_students = as.numeric(ifelse(grepl(">", n_disadv_students), 
                                               NA, n_disadv_students)))

# State ----
## All Students ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: State
# - Student Characteristics: All 
# - Reporting Categories: All

enroll_state <- read_csv("data/raw/fall_membership_statistics_state.csv") %>%
  clean_names() %>%
  mutate(locality_grouping = "state")

## Economically Disadvantaged ----
# Build-a-table criteria:
# - School Years: 2024-2025, 2023-2024, 2022-2023, 2021-2022
# - Report Level: State
# - Student Characteristics: All 
# - Reporting Categories: 
#     - Disadvantaged: Yes

disadv_state <- read_csv("data/raw/disadv_state_raw.csv") %>%
  clean_names() %>%
  mutate(locality_grouping = "state") %>%
  select(-disadvantaged, -ft_count, -pt_count, n_disadv_students = total_count)

enroll_state <- left_join(enroll_state, disadv_state)

# Combine & Save ----
fall_membership <- bind_rows(enroll_div, enroll_reg, enroll_sch, enroll_state)

write_csv(fall_membership, "data/fall_membership.csv")

# School Key ----
# Create a key of division names/numbers, school names/numbers, and additional information 
# To use as key when joining multiple datasets where school names may be different 
# Downloaded from: School List with Principal Contact Information CSV on
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories 

school_key_raw <- read_csv("data/raw/Public_School_report.csv", 
                       col_types = cols(`NCES School Num` = col_character(), 
                                        `School  Num` = col_character()), 
                       skip = 2) %>%
  janitor::clean_names() %>%
  select(-division_description, -schedule, -principal, -address1, -address2, -state, -phone_number) %>%
  mutate(division_name = str_trim(str_remove(division_name, "Public Schools")))

vdoe_regions <- read_csv("data/vdoe_regions.csv")

school_key <- school_key_raw %>% 
  left_join(vdoe_regions)

write_csv(school_key, "data/school_key.csv")
