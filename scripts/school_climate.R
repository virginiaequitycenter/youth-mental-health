# Download and clean the 2022 and 2023 School Survey of Climate and Working Conditions
# Homepage: https://www.dcjs.virginia.gov/virginia-center-school-and-campus-safety/programs/virginia-school-survey-climate-and-working

# TODO: confirm why there are no regional summaries for 2023 

# Libraries ----
library(boxr)
library(janitor)
library(readxl)
library(tidyverse)

# Instructions for setting up Box developer account and connecting it to RStudio: 
# https://r-box.github.io/boxr/articles/boxr-app-interactive.html#create
box_auth()

# Download ----
## 2023: 
box_fetch(dir_id = 320086019601, local_dir = "data/raw/school_climate/2023/") # takes ~3 minutes

# Read student data:
files_23 <- list.files("data/raw/school_climate/2023", pattern = "\\.xlsx", full.names = TRUE)
climate_23_raw <- map_dfr(files_23, ~read_excel(.x, sheet = "Data_student"))

# Tidy:
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

# Tidy:
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

missing <- climate_23 %>%
  filter(is.na(region_id))

# Calculate region averages for 2023 (this is provided in the 2022 data, but not in the 2023 summary)
reg_23 <- climate_23 %>%
  group_by(region_id) %>%
  summarise(
    s_num = sum(s_num, na.rm = T),
    across(
      .cols = starts_with("pct") | starts_with("avg"),
      .fns = ~mean(.x, na.rm = T)
    )) %>%
  filter(!is.na(region_id)) %>%
  mutate(locality_grouping = "region")

# Combine & Save ----
climate <- bind_rows(climate_22, climate_23, reg_23)

# Add region name:
vdoe_regions <- read_csv("data/vdoe_regions_divisions.csv") %>%
  select(region_name, region_number) %>%
  distinct()

# Add locality grouping (for easier filtering):
climate <- climate %>%
  left_join(vdoe_regions, by = join_by(region_id == region_number)) %>%
  mutate(locality_grouping = case_when(
    division_name == "Region Average" ~ "region",
    division_name == "State Average" ~ "state",
    school_name == "Division Average" ~ "division",
    !is.na(school_id) ~ "school")) %>%
    distinct()

# Note that some of the regions provided are not used in other places (EX. 9-12, 999)
# so they come up as having NA region names 

# Save:
write_csv(climate, "data/climate.csv")
