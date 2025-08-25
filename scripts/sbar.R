# Script to pull and prep Student Behavior and Administrative Response data for 
# Virginia public schools from the 2022 to 2024 school years 
# Homepage: https://www.doe.virginia.gov/data-policy-funding/data-reports/data-collection/special-education

# Libraries ----
library(here)
library(httr)
library(janitor)
library(readxl)
library(sf)
library(tidycensus)
library(tidyverse)

# Download data ----
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
  'user-agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36'
)

custom_dl_func = function(file, dest) {
  res <- GET(url = file, add_headers(.headers = headers))
  bin <- content(res, "raw")
  writeBin(bin, dest)
}

walk2(urls_div, dest_div, custom_dl_func)
walk2(urls_state, dest_state, custom_dl_func)
walk2(urls_sch, dest_sch, custom_dl_func)

# Read ----
files_div <- list.files("data/raw", pattern = "^sbar_div", full.names = TRUE)
sbar_div <- map_dfr(files_div, ~read_excel(.x, sheet = "Events by Behavior"))

files_state <- list.files("data/raw", pattern = "^sbar_state", full.names = TRUE)
sbar_state <- map_dfr(files_state, ~read_excel(.x, sheet = "Events by Behavior"))

files_sch <- list.files("data/raw", pattern = "^sbar_sch", full.names = TRUE)
sbar_sch <- map_dfr(files_sch, ~read_excel(.x, sheet = "Events by Behavior"))

# Tidy & Save ----

regions <- read_csv("data/vdoe_regions.csv")

## Division ----

# Deal with missing, renamed, etc. school districts 
sbar_div <- sbar_div %>%
  clean_names() %>%
  rename(region_number = region) %>%
  mutate(division_name = case_when(
    grepl("Alleghany", division_name) ~ "Alleghany Highlands County",
    grepl("Colonial Beach", division_name) ~ "Westmoreland County",
    grepl("Enterprise", division_name) ~ "Newport News City",
    grepl("West Point", division_name) ~ "King William County",
    TRUE ~ division_name))
  
# Join with regions to get GEOID 
sbar_div <- sbar_div %>%
  select(-region_number) %>%
  left_join(regions, by = join_by(division_name == district_name))

# Fix combined Williamsburg/James City County District 
sbar_div <- sbar_div %>%
  mutate(
    region_name = case_when(
      division_name == "Williamsburg-James City County" ~ "Tidewater",
      TRUE ~ region_name),
    region_number = case_when(
      division_name == "Williamsburg-James City County" ~ 2,
      TRUE ~ region_number),
    GEOID = case_when(
      division_name == "Williamsburg-James City County" ~ 51830,
      TRUE ~ GEOID))

write_csv(sbar_div, "data/sbar_division.csv")

# State:
sbar_state <- sbar_state %>%
  clean_names() 

write_csv(sbar_state, "data/sbar_state.csv")

# School:
sbar_sch <- sbar_sch %>%
  clean_names() %>%
  rename(region_number = region) %>%
  left_join(regions)

write_csv(sbar_sch, "data/sbar_school.csv")

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