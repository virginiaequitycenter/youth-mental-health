# Download and clean the 2022 and 2023 School Survey of Climate and Working Conditions
# Homepage: https://www.dcjs.virginia.gov/virginia-center-school-and-campus-safety/programs/virginia-school-survey-climate-and-working

# Libraries ----
library(boxr)
library(janitor)
library(readxl)
library(tidyverse)

# Instructions for setting up Box developer account and connecting it to RStudio: 
# https://r-box.github.io/boxr/articles/boxr-app-interactive.html#create
box_auth()

# 2023 ----
# Download: 
box_fetch(dir_id = 320086019601, local_dir = "data/raw/school_climate/2023/")

# Read student data:
files_23 <- list.files("data/raw/school_climate/2023", pattern = "\\.xlsx", full.names = TRUE)
climate_23_raw <- map_dfr(files_23, ~read_excel(.x, sheet = "Data_student"))

# Tidy:
climate_23 <- climate_23_raw %>%
  distinct() %>%
  clean_names() %>%
  select(-state_id, -state_name) %>%
  mutate(
    across(
    .cols = matches("q") | matches("id"),
    .fns = ~ as.numeric(gsub("%", "", .))),
    s_num = as.numeric(s_num),
    yr = 2023)

# Create state average dataframe:
climate_state_23 <- climate_23 %>%
  filter(division_name == "State Average") %>%
  distinct(division_name, .keep_all = T) %>%
  select(-matches("id|name"))

# Create division average dataframe:
climate_div_23 <- climate_23 %>%
  filter(school_name == "Division Average") %>%
  select(-matches("school"))

# Create school-level dataframe:
climate_sch_23 <- climate_23 %>%
  filter(!school_name %in% c("State Average", "Division Average"))

# 2022 ----
# Download: 
box_fetch(dir_id = 320085027632, local_dir = "data/raw/school_climate/2022/")

# Read student data:
files_22 <- list.files("data/raw/school_climate/2022", pattern = "\\.xlsx", full.names = TRUE)
climate_22_raw <- map_dfr(files_22, ~read_excel(.x, sheet = "Data_student"))

# Tidy:
climate_22 <- climate_22_raw %>%
  distinct() %>%
  clean_names() %>%
  select(-state_id, -state_name) %>%
  rename(s_num = student_num) %>%
  mutate(
    across(
      .cols = matches("stu") | matches("id"),
      .fns = ~ as.numeric(gsub("%", "", .))),
    s_num = as.numeric(s_num),
    yr = 2022,
    region_name = gsub(".*-\\s*", "", region_name))

# Create state average dataframe:
climate_state_22 <- climate_22 %>%
  filter(region_name == "State Average") %>%
  select(-matches("id|name"))

# Create division average dataframe:
climate_div_22 <- climate_22 %>%
  filter(school_name == "Division Average") %>%
  select(-matches("school"))

# Create region average dataframe:
climate_reg_22 <- climate_22 %>%
  filter(school_name == "Region Average") %>%
  select(-matches("district|division|school"))

# Create school-level dataframe:
climate_sch_22 <- climate_22 %>%
  filter(!grepl("Average", school_name))

# Variables ----

# Create key for student variables of interest listed here:
# https://docs.google.com/spreadsheets/d/1PnVFH4DsR9IBsQnsbyemPaoPpTkJfjiPt0ilWQ85yik/edit?usp=sharing

