# Format prototype_data for use across prototype apps (app2_glance, app3_explore, and app4_compare)
# Data generated and defined in prototype_data_dictionary.Rmd

# Source this script to standardize on data structures and metadata across prototypes

# Setup ----
library(tidyverse)

# Read ----

# For school grade levels, unique ids, etc. 
school_levels <- read_csv("school_key.csv") %>%
  select(grade_standard, sch_id)

# Data generated and defined in prototype_data_dictionary.Rmd
prototype_data <- read_csv("prototype_data.csv") %>%
  left_join(school_levels, by = "sch_id") %>%
  filter(locality_grouping != "school" | grade_standard %in% c("High", "Middle"))

# Create lookup table for schools that have duplicate names 
# (eg. Central High - King and Queen, Central High - Lunenburg County, etc. )
duplicate_school_names <- prototype_data %>%
  filter(locality_grouping == "school") %>%
  distinct(sch_id, school_name) %>%
  count(school_name) %>%
  filter(n > 1) %>%
  pull(school_name)

# Reduce to most recent values ----

## bully rates (sbar) --> 2024
## climate --> 2022 (high) and 2023 (middle)
## staff and enrollment --> 2025

## Bully information as of 23/24 ----
bully_recent <- prototype_data %>%
  filter(school_year == "2023-2024") %>%
  select(school_year:division_name, sch_id:school_name, n_bullying_incidents:bully_rate_per_1k) %>%
  mutate(
    label = case_when(
      locality_grouping == "state" ~ "State Average",
      locality_grouping == "region" ~ region_name, 
      locality_grouping == "division" ~ division_name,
      locality_grouping == "school" & school_name %in% duplicate_school_names ~
        paste(school_name, division_name, sep = " — "),
      locality_grouping == "school" ~ school_name),
    # If bully rate is NA make 0 
    bully_rate_per_1k = case_when(
      is.na(bully_rate_per_1k) ~ 0,
      TRUE ~ bully_rate_per_1k))

# Check for dupes:
# bully_recent %>% count(label) %>% filter(n > 1) # uh oh
# 
# duplicate_labels <- bully_recent %>%
#   count(label) %>%
#   filter(n > 1) %>%
#   pull(label)


## Climate data is more complicated ----
# - years are 21/22 (high) AND 22/23 (middle)
# - different state averages calculated (middle school, high school, combined)
climate <- prototype_data %>%
  filter(str_detect(school_year, "2022")) %>%
  select(school_year:division_name, sch_id:school_name, n_students_surveyed:avg_bully_prob, grade_standard)

# School:
# we only care about school type here 
climate_recent_sch <- climate %>% 
  filter(locality_grouping == "school",
         !is.na(n_students_surveyed)) %>%
  group_by(school_name) %>%
  filter(n() == 1 | school_year == "2022-2023") %>%
  ungroup()

# Division:
# High schools in 21-22 and middle schools in 22-23
# Calculate division averages across years
climate_div_avg <- climate %>%
  filter(locality_grouping == "division") %>%
  group_by(division_number, division_name) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "division")

# Calculate region averages across years 
reg_avg <- climate %>%
  filter(locality_grouping == "region") %>%
  group_by(region_number, region_name) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "region")

# State:
# Calculate state average across years
state_avg <- climate %>%
  filter(locality_grouping == "state") %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "state",
    region_number = NA_real_)

# Combine all climate data 
climate_recent <- bind_rows(state_avg, reg_avg, climate_div_avg, climate_recent_sch) %>%
  mutate(label = case_when(
    locality_grouping == "school" & school_name %in% duplicate_school_names ~
      paste(school_name, division_name, sep = " — "),
    locality_grouping == "school" ~ school_name,
    locality_grouping == "division" ~ division_name,
    locality_grouping == "region" ~ region_name,
    TRUE ~ "State Average"))

## Staffing and disadv as of 24/25 ----
staff_disadv_recent <- prototype_data %>%
  filter(school_year == "2024-2025") %>%
  select(school_year:pct_disadv, total_positions:staff_per_1k_students) %>%
  mutate(label = case_when(
    locality_grouping == "state" ~ "State Average",
    locality_grouping == "region" ~ region_name, 
    locality_grouping == "division" ~ division_name,
    locality_grouping == "school" & school_name %in% duplicate_school_names ~
      paste(school_name, division_name, sep = " — "),
    locality_grouping == "school" ~ school_name))

# Pivot everything longer  
staff_disad_long <- staff_disadv_recent %>%
  select(label, locality_grouping, pct_disadv, staff_per_1k_students) %>%
  pivot_longer(cols = c(pct_disadv, staff_per_1k_students))

climate_long <- climate_recent %>%
  select(label, locality_grouping, pct_mental_health_training:avg_bully_prob) %>%
  pivot_longer(cols = c(pct_mental_health_training:avg_bully_prob))

bully_long <- bully_recent %>%
  select(label, locality_grouping, bully_rate_per_1k) %>%
  pivot_longer(cols = bully_rate_per_1k)

# And combine:
plt_dat <- rbind(staff_disad_long, climate_long, bully_long) 

# Get change data ----
## Measures that have multiple years: bully_rate_per_1k, pct_disadv, staff_per_1k_students
change_raw <- prototype_data %>%
  select(school_year, locality_grouping, region_name, division_name, school_name, pct_disadv, bully_rate_per_1k, staff_per_1k_students) %>%
  mutate(
    school_year = as.numeric(str_sub(school_year, 6, 9)),
    label = case_when(
      locality_grouping == "state" ~ "State Average",
      locality_grouping == "region" ~ region_name,
      locality_grouping == "division" ~ division_name,
      locality_grouping == "school" & school_name %in% duplicate_school_names ~
        paste(school_name, division_name, sep = " — "),
      locality_grouping == "school" ~ school_name),
    # If bullying incidents are not available, we assume there were no reports made
    # (this is different from staffing and disadv)
    bully_rate_per_1k = case_when(
      is.na(bully_rate_per_1k) ~ 0,
      TRUE ~ bully_rate_per_1k))

change_dat <- change_raw %>%
  select(label, locality_grouping, pct_disadv:staff_per_1k_students, school_year) %>%
  pivot_longer(cols = c(pct_disadv:staff_per_1k_students))

# Standardize on measure metadata ----
# Measures where higher values are better: avg_peer_rel, avg_adult_rel, 
# pct_mental_health_training, staff_per_1k_students
measure_key <- read_csv("measure_key.csv")

# Save to different app directories ----
folders <- c("../app2_glance", "../app3_explore", "../app4_compare")

## School key---- 
school_key_deduped <- read_csv("school_key.csv") %>%
  mutate(school_name = ifelse(school_name %in% duplicate_school_names,
                              paste(school_name, division_name, sep = " — "), school_name))

walk(folders, ~write_csv(school_key_deduped, file.path(.x, "school_key_deduped.csv")))

## Measure key ----
walk(folders, ~write_csv(measure_key, file.path(.x, "measure_key.csv")))

## Plot data ----
walk(folders, ~write_csv(plt_dat, file.path(.x, "plt_dat.csv")))

## Change data ----
walk(folders, ~write_csv(change_dat, file.path(.x, "change_dat.csv")))


# Debug --------------------
# Duplicates
# n_dupes <- plt_dat %>%
#   count(locality_grouping, label, name, sort = TRUE) %>%
#   filter(n > 1)
# 
# duped_labels <- unique(n_dupes$label)
# 
# dupes <- plt_dat %>% filter(label %in% duped_labels)
