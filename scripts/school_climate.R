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
    .cols = matches("q"),
    .fns = ~ as.numeric(gsub("%", "", .))),
    s_num = as.numeric(s_num),
    yr = 2023)

# Drop state average and save as separate dataframe:
climate_state_23 <- climate_23 %>%
  filter(region_id == "State Average") %>%
  distinct(region_id, .keep_all = T)

# Drop district averages and save and separate dataframe 



