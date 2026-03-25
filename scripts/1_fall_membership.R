# Script to pull and prep Fall Membership (Enrollment) data from the Virginia
# Department of Education from the 2022 to 2024 school years. 
# Homepage: https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304

# Because this data cannot be downloaded programatically, we need to use the VDOE 
# build-a-table functionality to create and download the data as CSVs.  

# Libraries ----
library(tidyverse)
library(janitor)

# Names ----
# For consistency on region names and division names across datasets, we're using the regions data from:
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories/virginia-public-school-listing-by-region

regions <- read_csv("data/raw/vdoe_regions_divisions.csv") %>%
  filter(division_name != "James City County") %>% # duplicated as "Williamsburg-James City County"
  add_row(region_name = "Western Virginia", region_number = 6, # add Covington City
          division_name = "Covington City", division_number = "107")
  
write_csv(regions, "data/regions.csv")

# For consistency on school names (eg. Johnson Elementary --> Tall Oaks) were using the most 
# recent school names data from: School List with Principal Contact Information CSV on
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories 

school_key <- read_csv("data/raw/Public_School_report.csv", 
                           col_types = cols(`NCES School Num` = col_character(), 
                                            `School  Num` = col_character()), 
                           skip = 2) %>%
  janitor::clean_names() %>%
  select(-division_description, -schedule, -principal, -address1, -address2, -state, 
         -phone_number, -division_name, division_number = division_num) %>%
  mutate(sch_id = paste0(division_number, school_num)) %>%
  left_join(regions, by = "division_number")

write_csv(school_key, "data/school_key.csv")

# Smaller, for fixing old school names
new_school_names <- school_key %>%
  select(school_name, sch_id) %>%
  add_row(sch_id = "1070350", school_name = "Covington High") %>%
  add_row(sch_id = "1070260", school_name = "Edgemont Primary") %>%
  add_row(sch_id = "1070360", school_name = "Jeter-Watson Intermediate")

write_csv(new_school_names, "data/new_school_names.csv")

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
  select(-division_name) %>%
  mutate(division_number = str_pad(as.character(division_number), 3, pad = "0"),
         locality_grouping = "division") %>% 
  left_join(regions, by = "division_number") %>%
  mutate(
    division_name = case_when(
      division_number == 107 ~ "Covington City",
      TRUE ~ division_name),
    region_name = case_when(
      division_number == 107 ~ "Western Virginia",
      TRUE ~ region_name),
    region_number = case_when(
      division_number == 107 ~ 6,
      TRUE ~ region_number))
  
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
  select(-division_name) %>%
  mutate(division_number = str_pad(as.character(division_number), 3, pad = "0"),
         locality_grouping = "division") %>%
  left_join(regions, by = "division_number") %>%
  select(-disadvantaged, -ft_count, -pt_count, n_disadv_students = total_count) %>%
  mutate(
    division_name = case_when(
      division_number == 107 ~ "Covington City",
      TRUE ~ division_name),
    region_name = case_when(
      division_number == 107 ~ "Western Virginia",
      TRUE ~ region_name),
    region_number = case_when(
      division_number == 107 ~ 6,
      TRUE ~ region_number))

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
  select(-division_name) %>% # fix division names
  left_join(regions, by = "division_number") %>%
  mutate( # division edge cases
    division_name = case_when(
      division_number == 107 ~ "Covington City",
      TRUE ~ division_name),
    region_name = case_when(
      division_number == 107 ~ "Western Virginia",
      TRUE ~ region_name),
    region_number = case_when(
      division_number == 107 ~ 6,
      TRUE ~ region_number)) %>%
  select(-school_name) %>% # fix school names
  left_join(new_school_names, by = "sch_id")

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
    across(c(ft_count, pt_count, total_count), ~parse_number(.)),
    n_disadv_students = total_count) %>%
  select(-division_name) %>% # fix region names
  left_join(regions, by = "division_number") %>%
  mutate( # fix edge cases
    division_name = case_when(
      division_number == 107 ~ "Covington City",
      TRUE ~ division_name),
    region_name = case_when(
      division_number == 107 ~ "Western Virginia",
      TRUE ~ region_name),
    region_number = case_when(
      division_number == 107 ~ 6,
      TRUE ~ region_number)) %>%
  select(-school_name) %>% # fix school names
  left_join(new_school_names, by = "sch_id") %>%
  select(-ft_count, -pt_count, -total_count, -disadvantaged)

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
fall_membership <- bind_rows(enroll_div, enroll_reg, enroll_sch, enroll_state) %>%
  rename(total_enrolled = total_count) %>%
  select(-school_number)

write_csv(fall_membership, "data/fall_membership.csv")
