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

regions <- tibble(
  region_number = c(1:8),
  region_name = c("Central Virginia", "Tidewater", "Northern Neck", "Northern Virginia",
             "Valley", "Western Virginia", "Southwest", "Southside"))

# Division: 
sbar_div <- sbar_div %>%
  clean_names() %>%
  rename(region_number = region) %>%
  left_join(regions)

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

# Explore -----

# Filter to firearm-related incidents:

# Behaviors of interest: 
# "Assault with Firearm or Weapon", "Illegal Possession of Rifle or Shotgun", 
# "Illegal Possession of Handgun", "Illegal Possession of Any Other Projectile Weapon",
# "Illegal Possession of Other Firearms:..."
sbar_firearm <- sbar %>%
  filter(grepl("Firearm or Weapon|Rifle|Handgun|Other Projectile Weapon|Other Firearms", behavior)) %>%
  group_by(division_name, school_year) %>%
  summarize(total_incidents = sum(number_of_events))



# Join with school enrollment numbers to calculate rates ----
# ACS school enrollment 
raw <- get_acs(geography = "county",
               variables = c("B01003_001", "B14001_005", "B14001_006", "B14001_007"),
               state = "VA",
               survey = "acs5",
               year = 2023,
               geometry = TRUE,
               output = "wide")

acs <- raw %>%
  mutate(county = gsub('.{10}$', "", NAME)) %>%
  select(-ends_with("M"), -NAME) %>%
  rename(pop_est = B01003_001E,
         grade1to4 = B14001_005E,
         grade5to8 = B14001_006E,
         grade9to12 = B14001_007E) %>%
  group_by(county) %>%
  mutate(enrolled = sum(grade1to4, grade5to8, grade9to12)) %>%
  ungroup() %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

sbar_firearms <- acs %>%
  left_join(sbar, by = join_by(county == division_name)) %>%
  mutate(incident_rate = (total_incidents / enrolled))

# Quickly visualize to confirm
plot(sbar_firearms["incident_rate"])
plot(sbar_firearms["total_incidents"])

sbar_firearms <- sbar_firearms %>%
  drop_na(incident_rate) %>%
  select(district = county, school_year, total_pop = pop_est, enrolled, total_incidents, 
         incident_rate, geometry)

# Save ----
saveRDS(sbar_firarms, "data/sbar_firearms.RDS")