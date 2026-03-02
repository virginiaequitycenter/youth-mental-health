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

# School Key ----
# Create a key of division names/numbers, school names/numbers, and additional information 
# To use as key when joining multiple datasets where school names may be different 
# Downloaded from: School List with Principal Contact Information CSV on
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories 

school_key <- read_csv("data/raw/Public_School_report.csv", 
                           col_types = cols(`NCES School Num` = col_character(), 
                                            `School  Num` = col_character()), 
                           skip = 2) %>%
  janitor::clean_names() %>%
  select(-division_description, -schedule, -principal, -address1, -address2, -state, -phone_number) %>%
  mutate(division_name = str_trim(str_remove(division_name, "Public Schools")),
         sch_id = paste0(division_num, school_num)) %>% 
  left_join(regions) %>%
  select(-division_num)

write_csv(school_key, "data/school_key.csv")

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
  left_join(regions, by = c("division_number", "division_name")) %>%
  mutate(division_number = str_pad(as.character(division_number), 3, pad = "0"),
         division_name = case_when(
           division_number == "003" ~ "Alleghany County",
           TRUE ~ division_name),
         region_name = case_when(
           division_name %in% c("Covington City", "Alleghany County") ~ "Western Virginia",
           TRUE ~ region_name),
         region_number = case_when(
           division_name %in% c("Covington City", "Alleghany County") ~ 6,
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
  left_join(regions, by = c("division_number", "division_name")) %>%
  mutate(division_number = str_pad(as.character(division_number), 3, pad = "0"),
         division_name = case_when(
           division_number == "003" ~ "Alleghany County",
           TRUE ~ division_name),
         region_name = case_when(
           division_name %in% c("Covington City", "Alleghany County") ~ "Western Virginia",
           TRUE ~ region_name),
         region_number = case_when(
           division_name %in% c("Covington City", "Alleghany County") ~ 6,
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
  clean_names() %>%
  mutate(
    division_number = str_pad(as.character(division_number), 3, pad = "0"),
    school_number = str_pad(as.character(school_number), 4, pad = "0"),
    sch_id = paste0(division_number, school_number),
    locality_grouping = "school") %>%
  select(-division_number, -division_name, -school_name) %>%
  left_join(school_key, by = "sch_id") %>%
  select(-school_number, -school_num)

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
  clean_names() %>%
  mutate(
    division_number = str_pad(as.character(division_number), 3, pad = "0"),
    school_number = str_pad(as.character(school_number), 4, pad = "0"),
    sch_id = paste0(division_number, school_number),
    locality_grouping = "school",
    n_disadv_students = as.numeric(ifelse(grepl(">", total_count), 
                                          NA, total_count))) %>%
  select(-division_number, -division_name, -school_name, -ft_count, -pt_count, -total_count, -disadvantaged) %>%
  left_join(school_key, by = "sch_id") %>%
  select(-school_number, -school_num)

enroll_sch <- enroll_sch %>%
  left_join(disadv_sch)

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