# Script to pull and prep Student Behavior and Administrative Response data for 
# Virginia public schools from the 2021-22 to 2023-24 school years 
# Homepage: https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education

# Libraries ----
library(here)
library(httr)
library(janitor)
library(readxl)
library(tidyverse)

# For consistency on regions across datasets, we're using the regions data from:
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories/virginia-public-school-listing-by-region
regions <- read_csv("data/regions.csv")

# And for consistency across school names we're using school key from fall_membership.R:
new_school_names <-  read_csv("data/new_school_names.csv")

# Student Behavior ----
## Download data ----
# Data pulled from https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education

# Create vectors of urls:
urls_div <- c(
  "https://www.doe.virginia.gov/home/showpublisheddocument/57664/638629474066500000", # 2023-2024
  "https://www.doe.virginia.gov/home/showpublisheddocument/50647/638345340433400000", # 2022-2023
  "https://www.doe.virginia.gov/home/showpublisheddocument/50645/638345340429030000") # 2021-2022

urls_state <- c(
  "https://www.doe.virginia.gov/home/showpublisheddocument/57668/638629474076070000", # 2023-2024
  "https://www.doe.virginia.gov/home/showpublisheddocument/50655/638345340450130000", # 2022-2023
  "https://www.doe.virginia.gov/home/showpublisheddocument/50653/638345340446230000") # 2021-2022

urls_sch <- c(
  "https://www.doe.virginia.gov/home/showpublisheddocument/57666/638629474072370000", # 2023-2024
  "https://www.doe.virginia.gov/home/showpublisheddocument/50651/638345340442630000", # 2022-2023
  "https://www.doe.virginia.gov/home/showpublisheddocument/50649/638345340437930000") # 2021-2022


# Create vectors of destination file names:
dest_div <- paste0("data/raw/sbar_div_", c(2022:2024), ".xlsx")
dest_state <- paste0("data/raw/sbar_state_", c(2022:2024), ".xlsx")
dest_sch <- paste0("data/raw/sbar_sch_", c(2022:2024), ".xlsx")

if (!dir.exists(here("data/raw"))) {
  dir.create(here("data/raw"))
}

# Use headers to masquerade as a browser by manually supplying your user-agent,
# otherwise you'll get a Error 403: Forbidden. 
# You'll need to do this every time you update one of your browsers. 

# To get your user agent: 
# 1. Open url above (in Chrome): https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education
# 2. Right click anywhere on the page and select INSPECT
# 3. Navigate to NETWORK tab 
# 4. Resubmit the api request by selecting one of the school download links as an example 
# 5. Click on the request (it will start with image.aspx?...)
# 6. Scroll down to REQUEST HEADERS
# 7. Copy the text after USER-AGENT and paste it into field below

headers = c(
  'user-agent' = 'Mozilla/5.0 (iPhone; CPU iPhone OS 18_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.5 Mobile/15E148 Safari/604.1'
)

custom_dl_func = function(file, dest) {
  res <- GET(url = file, add_headers(.headers = headers))
  bin <- content(res, "raw")
  writeBin(bin, dest)
}

walk2(urls_div, dest_div, custom_dl_func)
walk2(urls_state, dest_state, custom_dl_func)
walk2(urls_sch, dest_sch, custom_dl_func)

## Read ----
files_div <- list.files("data/raw", pattern = "^sbar_div", full.names = TRUE)
sbar_div <- map_dfr(files_div, ~read_excel(.x, sheet = "Events by Behavior"))

files_state <- list.files("data/raw", pattern = "^sbar_state", full.names = TRUE)
sbar_state <- map_dfr(files_state, ~read_excel(.x, sheet = "Events by Behavior"))

files_sch <- list.files("data/raw", pattern = "^sbar_sch", full.names = TRUE)
sbar_sch_raw <- map_dfr(files_sch, ~read_excel(.x, sheet = "Events by Behavior"))

## Tidy ----
### Division ----
sbar_div <- sbar_div %>%
  clean_names() %>%
  rename(region_number = region, n_events = number_of_events) %>%
  mutate(division_number = str_pad(as.character(division_number), 3, pad = "0"),
         division_name = case_when(
           division_number == "003" ~ "Alleghany County",
           TRUE ~ division_name)) %>%
  left_join(regions, by = c("division_number", "division_name", "region_number")) %>%
  mutate(region_name = case_when(
    division_name == "Covington City" ~ "Western Virginia",
    TRUE ~ region_name
  ))

# Alternative, technical, or adult education schools typically have region as NA,
# so let's drop them for now 
nas <- sbar_div %>% filter(if_any(everything(), is.na))

sbar_div <- sbar_div %>%
  filter(!is.na(region_number)) %>%
  mutate(locality_grouping = "division")

### Region ----
# Region-level summaries are not available for download, so we create those ourselves 
sbar_reg <- sbar_div %>%
  group_by(school_year, region_name, region_number, behavior_code, behavior_category, 
           behavior_category_code, behavior) %>%
  summarise(n_events = sum(n_events, na.rm = T))%>%
  mutate(locality_grouping = "region")

### State ----
sbar_state <- sbar_state %>%
  clean_names() %>%
  rename(n_events = number_of_events) %>%
  mutate(locality_grouping = "state")
  
### School ----
sbar_sch <- sbar_sch_raw %>%
  clean_names() %>%
  rename(region_number = region,
         n_events = number_of_events) %>%
  mutate(division_number = str_pad(as.character(division_number), 3, pad = "0"),
         school_number = str_pad(as.character(school_number), 4, pad = "0"),
         sch_id = paste0(division_number, school_number)) %>%
  select(-division_name, -region_number, -school_number)

# Join with regions to get region names:
sbar_sch <- sbar_sch %>%
  left_join(regions, by = "division_number")
  
# Join with new_school_names to get school names:
sbar_sch <- sbar_sch %>%
  left_join(new_school_names, by = "sch_id") %>%
  mutate(locality_grouping = "school")

# Spot check missing values
nas <- sbar_sch %>% filter(if_any(c(region_number, region_name, division_number, 
                                    division_name, school_name.x, school_name.y), is.na))

# If the school name isn't listed in the school_key, then just use the name from sbar:
sbar_sch <- sbar_sch %>%
  mutate(
    school_name.y = coalesce(school_name.y, school_name.x)) %>%
  rename(school_name = school_name.y) %>%
  select(-school_name.x)

# Confirm
nas <- sbar_sch %>% filter(if_any(c(region_number, region_name, division_number, 
                                    division_name, school_name, school_name), is.na))

## Combine & Save ----
sbar <- bind_rows(sbar_div, sbar_reg, sbar_sch, sbar_state)
write_csv(sbar, "data/sbar_behavior.csv")

# Behavior Codes ----

beh_url <- c("https://www.doe.virginia.gov/home/showpublisheddocument/57882/638862802164530000")

dest_beh <- "data/raw/sbar_behavior_codes.xlsx"

walk2(beh_url, dest_beh, custom_dl_func)

beh <- map_dfr(dest_beh, 
               ~read_excel(.x, sheet = "Behavior Codes", col_names = TRUE,
                           skip = 2))
beh <- beh %>% 
  clean_names() %>%
  mutate(behavior_group = case_when(
    str_detect(behavior_code, "BAP") ~ "BAP: Behaviors that Impede the Academic Progress (BAP) of the student or of other students",
    str_detect(behavior_code, "BSO") ~ "BSO: Behaviors related to School Operations (BSO) interfere with the daily operation of school procedures",
    str_detect(behavior_code, "RB") ~ "RB: Relationship Behaviors (RB) create a negative relationship between two or more members of the school community (No physical harm is done.)",
    str_detect(behavior_code, "BSC") ~ "BSC: Behaviors of a Safety Concern (BSC) create unsafe conditions for students, staff, and/or visitors to the school.",
    str_detect(behavior_code, "BESO") ~ "BESO: Behaviors that Endanger Self or Others (BESO) These behaviors endanger the health, safety, or welfare of either the student or others in the school community.",
    str_detect(behavior_code, "PD") ~ "PD: Behaviors described in the Virginia’s Unsafe School Choice Option Policy required by the federal Every Student Succeeds Act of 2015."
  )) %>%
  filter(!is.na(description) & !is.na(behavior_code))

write_csv(beh, "data/sbar_behavior_codes.csv")

# Administrative Response ----
## Download ----

# In the Fall of 2025, the SBAR Administrative Response data became downloadable via their Build-A-Table tool:
# https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=351

# Table criteria:
# - School years: All (2023-2024, 2022-2023, and 2021-2022)
# - Report level: All (State, Division, and School)
# - SBAR Report: Sanctions Report
# - SBAR Behavior Categories/Types: All Sanction Types 

## Tidy ----
response <- read_csv("data/raw/sbar_statistics.csv") %>%
  clean_names() %>%
  mutate(locality_grouping = tolower(level)) %>%
  rename(n_sanctions = number_of_sanctions, n_students_sanctioned = number_of_students) %>%
  select(-level) %>%
  left_join(regions, by = join_by(division_name, division_number))

# Calculate region-level summaries 
res_region <- response %>%
  group_by(school_year, sanction_code, sanction_description, region_name, region_number) %>%
  summarise(
    n_sanctions = sum(n_sanctions, na.rm = T),
    n_students_sanctioned = sum(n_students_sanctioned, na.rm = T)) %>%
  mutate(locality_grouping = "region") %>%
  drop_na(region_name) # remove alt schools

## Combine & Save ----

response <- bind_rows(response, res_region)
write_csv(response, "data/sbar_response.csv")
            