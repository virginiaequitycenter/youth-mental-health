# Download and clean the 2022 and 2023 School Survey of Climate and Working Conditions
# Homepage: https://www.dcjs.virginia.gov/virginia-center-school-and-campus-safety/programs/virginia-school-survey-climate-and-working

# Libraries ----
library(boxr)
#library(janitor)
library(readxl)
library(tidyverse)

# To standardize on region names across datasets:
regions <- read_csv("data/vdoe_regions_divisions.csv")

# Note that some of the regions provided are not used in other places (EX. 9-12, 999)
# so they come up as having NA region names 

# And for consistency across school names we're using the school key from fall_membership.R:
school_key <- read_csv("data/school_key.csv")

school_names_new <- school_key %>%
  select(school_name, sch_id)

# Instructions for setting up Box developer account and connecting it to RStudio: 
# https://r-box.github.io/boxr/articles/boxr-app-interactive.html#create
box_auth()

# Download ----
## 2023: 
box_fetch(dir_id = 320086019601, local_dir = "data/raw/school_climate/2023/") # takes ~3 minutes

# Read student data:
files_23 <- list.files("data/raw/school_climate/2023", pattern = "\\.xlsx", full.names = TRUE)
climate_23_raw <- map_dfr(files_23, ~read_excel(.x, sheet = "Data_student"))

# Prep for renaming variables:
climate_23 <- climate_23_raw %>%
  distinct() %>%
  rename_with(tolower) %>%
  select(-state_id) %>%
  mutate(
    across(
    .cols = matches("q") | matches("id"),
    .fns = ~ as.numeric(gsub("%", "", .))),
    s_num = as.numeric(s_num),
    yr = 2023)

## 2022:
box_fetch(dir_id = 320085027632, local_dir = "data/raw/school_climate/2022/") # takes ~3 minutes

# Read student data:
files_22 <- list.files("data/raw/school_climate/2022", pattern = "\\.xlsx", full.names = TRUE)
climate_22_raw <- map_dfr(files_22, ~read_excel(.x, sheet = "Data_student"))

# Prep for renaming variables:
climate_22 <- climate_22_raw %>%
  distinct() %>%
  rename_with(tolower) %>%
  select(-state_id, -region_name) %>%
  rename(s_num = student_num) %>%
  mutate(
    across(
      .cols = matches("stu") | matches("id"),
      .fns = ~ as.numeric(gsub("%", "", .))),
    s_num = as.numeric(s_num),
    yr = 2022)

# Fix Variable Names ----

# Create key for student variables of interest listed here:
# https://docs.google.com/spreadsheets/d/1PnVFH4DsR9IBsQnsbyemPaoPpTkJfjiPt0ilWQ85yik/edit?usp=sharing

# Removing variables that either aren't available for both the 22/23 school years 
# or that are calculated differently (eg. percents vs. averages, total anxiety scale vs.
# individual anxiety indicators)
col_key <- read_csv("data/raw/school_climate/climate_key.csv") %>%
  mutate(across(4:5, tolower)) %>%
  drop_na()

# Build mappings:
rename_map_22 <- setNames(col_key$var_name, col_key$id_22)
rename_map_23 <- setNames(col_key$var_name, col_key$id_23)

# Function to rename: 
rename_climate_data <- function(df, mapping) {
  df %>%
    select(matches("id|name"), s_num, yr, any_of(names(mapping))) %>%
    rename_with(~ mapping[.x], .cols = any_of(names(mapping)))
}

# Rename all data:
climate_23 <- rename_climate_data(climate_23, rename_map_23)
climate_22 <- rename_climate_data(climate_22, rename_map_22)

# missing <- climate_23 %>%
#   filter(is.na(region_id))

# Tidy ----
## 2023 ----
climate_23 <- climate_23 %>%
  mutate(
    locality_grouping = case_when(
      school_name == "State Average" ~ "state",
      school_name == "Division Average" ~ "division",
      TRUE ~ "school"),
    division_number = case_when(
      locality_grouping %in% c("division", "school") ~ str_pad(as.character(district_id), 3, pad = "0"),
      locality_grouping == "state" ~ NA),
    division_name = case_when(
      division_number == "003" ~ "Alleghany County",
      TRUE ~ division_name),
    school_number = case_when(
      locality_grouping == "school" ~ str_pad(as.character(school_id), 4, pad = "0"),
      TRUE ~ NA),
    sch_id = case_when(
      locality_grouping == "school" ~paste0(division_number, school_number),
      TRUE ~ NA)) %>%
  rename(region_number = region_id) %>%
  select(-state_name, -district_id, -school_id) %>%
  distinct()

# State 
climate_23_state <- climate_23 %>%
  filter(school_name == "State Average") %>%
  mutate(division_name = NA, 
         school_name = NA)

# Division
climate_23_div <- climate_23 %>%
  filter(school_name == "Division Average") %>%
  mutate(school_name = NA) %>%
  select(-region_number, -division_name) %>%
  left_join(regions, by = "division_number")

# Region 
# Calculated for 2023 (averages are provided in the 2022 data, but not in the 2023 summary)
climate_23_reg <- climate_23_div %>%
  group_by(region_number, region_name) %>%
  summarise(
    s_num = sum(s_num, na.rm = T),
    across(
      .cols = starts_with("pct") | starts_with("avg"),
      .fns = ~mean(.x, na.rm = T)
    )) %>%
  filter(!is.na(region_number)) %>%
  mutate(locality_grouping = "region",
         yr = 2023)
  
# School 
climate_23_sch <- climate_23 %>%
  filter(locality_grouping == "school")

# Join with regions to standardize on division names and get region names
climate_23_sch <- climate_23_sch %>%
  select(-region_number, -division_name) %>%
  left_join(regions, by = "division_number")

# Then join with school_names_new to standardize on school names
climate_23_sch <- climate_23_sch %>%
  select(-school_name) %>%
  left_join(school_names_new) %>%
  filter(!is.na(school_name))

## 2022 ----
climate_22 <- climate_22 %>%
  mutate(
    locality_grouping = case_when(
      school_name == "State Average" ~ "state",
      school_name == "Division Average" ~ "division",
      school_name == "Region Average" ~ "region",
      TRUE ~ "school"),
    division_number = case_when(
      locality_grouping %in% c("division", "school") ~ str_pad(as.character(district_id), 3, pad = "0"),
      locality_grouping == "state" ~ NA),
    division_name = case_when(
      division_number == "003" ~ "Alleghany County",
      TRUE ~ division_name),
    school_number = case_when(
      locality_grouping == "school" ~ str_pad(as.character(school_id), 4, pad = "0"),
      TRUE ~ NA),
    sch_id = case_when(
      locality_grouping == "school" ~paste0(division_number, school_number),
      TRUE ~ NA)) %>%
  rename(region_number = region_id) %>%
  select(-state_name, -district_id, -school_id) %>%
  distinct()

# State 
climate_22_state <- climate_22 %>%
  filter(school_name == "State Average") %>%
  mutate(division_name = NA, 
         school_name = NA)

# Division
climate_22_div <- climate_22 %>%
  filter(school_name == "Division Average") %>%
  mutate(school_name = NA) %>%
  select(-region_number, -division_name) %>% # Covington?
  left_join(regions, by = "division_number")

# Region 
climate_22_reg <- climate_22 %>%
  filter(school_name == "Region Average") %>%
  mutate(division_name = NA, 
         school_name = NA, 
         sch_id = NA) %>%
  left_join(regions %>% select(region_name, region_number) %>% distinct()) %>%
  filter(!is.na(region_name))

# School
climate_22_sch <- climate_22 %>%
  filter(locality_grouping == "school")

# Join with regions to standardize on division names and get region names
climate_22_sch <- climate_22_sch %>%
  select(-region_number, -division_name) %>%
  left_join(regions, by = "division_number")

# Join with school_names_new to standardize on school names 
climate_22_sch <- climate_22_sch %>%
  select(-school_name) %>%
  left_join(school_names_new) %>%
  filter(!is.na(school_name))

# Combine & Save ----
climate <- bind_rows(
  climate_22_state, climate_23_state, climate_22_reg, climate_23_reg, 
  climate_22_div, climate_23_div, climate_22_sch, climate_23_sch
)

write_csv(climate, "data/climate.csv")
