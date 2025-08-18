# Script to pull and prep American Community Survey data on enrollment numbers and 
# other youth demographics in the United States. 
# Explorer: https://data.census.gov/table/ACSDT1Y2023.C14002?q=Type+of+School&t=Age+and+Sex:Populations+and+People&g=040XX00US51

# Note on terminology: 
# District = school district, approximated by county 
# Region = group of school districts/counties, 8 total 

# Libraries ----
library(sf)
library(tidycensus)
library(tidyverse)

# County ----
##  School Enrollment (2023) ----
raw_div <- get_acs(geography = "county",
                   variables = c("B01003_001", "B14001_005", "B14001_006", "B14001_007"),
                   state = "VA",
                   survey = "acs5",
                   year = 2023,
                   geometry = TRUE,
                   output = "wide")

acs_div <- raw_div %>%
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

#saveRDS(acs_div, "data/acs_division.RDS") # Keep geometries

# Region ----

# Create regions dataframe from
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories/virginia-public-school-listing-by-region

regions <- read_csv("data/vdoe_regions.csv", col_types = cols(GEOID = col_character()))

acs_regions <- acs_div %>%
  left_join(regions)

# Add up totals and create new geometries 
acs_regions <- acs_regions %>%
  group_by(region_name, region_number) %>%
  summarise(pop_est = sum(pop_est, na.rm = TRUE),
            grade1to4 = sum(grade1to4, na.rm = TRUE),
            grade5to8 = sum(grade5to8, na.rm = TRUE),
            grade9to12 = sum(grade9to12, na.rm = TRUE),
            enrolled = sum(enrolled, na.rm = TRUE),
            geometry = st_union(geometry)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(acs_regions, "data/acs_region.RDS")

# State ----
## School Enrollment (2023) ----
raw_state <- get_acs(geography = "state",
                   variables = c("B01003_001", "B14001_005", "B14001_006", "B14001_007"),
                   state = "VA",
                   survey = "acs5",
                   year = 2023,
                   geometry = TRUE,
                   output = "wide")

acs_state <- raw_state %>%
  mutate(county = gsub('.{10}$', "", NAME)) %>%
  select(-ends_with("M"), -NAME) %>%
  rename(pop_est = B01003_001E,
         grade1to4 = B14001_005E,
         grade5to8 = B14001_006E,
         grade9to12 = B14001_007E) %>%
  mutate(enrolled = sum(grade1to4, grade5to8, grade9to12)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

saveRDS(acs_state, "data/acs_state.RDS")
