# Script to pull and prep American Community Survey data across different 
# geographical groupings in Virginia. 
# Explorer: https://data.census.gov/table/ACSDT1Y2023.C14002?q=Type+of+School&t=Age+and+Sex:Populations+and+People&g=040XX00US51

# Note on terminology: 
# District = school district, approximated by county 
# Region = group of school districts/counties, 8 total 


# Libraries ----
library(sf)
library(tidycensus)
library(tidyverse)

# County ----
raw_div <- get_acs(geography = "county",
                   variables = c("B01003_001"),
                   state = "VA",
                   survey = "acs5",
                   year = 2023,
                   geometry = TRUE,
                   output = "wide")

acs_div <- raw_div %>%
  mutate(county = gsub('.{10}$', "", NAME),
         county = gsub("city", "City", county)) %>%
  select(-ends_with("M"), -NAME) %>%
  rename(pop_est = B01003_001E) %>%
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
            geometry = st_union(geometry)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326)

#saveRDS(acs_regions, "data/acs_region.RDS")

# State ----
raw_state <- get_acs(geography = "state",
                   variables = c("B01003_001"),
                   state = "VA",
                   survey = "acs5",
                   year = 2023,
                   geometry = TRUE,
                   output = "wide")

acs_state <- raw_state %>%
  select(-B01003_001M) %>%
  rename(pop_est = B01003_001E) %>% 
  st_as_sf() %>%
  st_transform(crs = 4326)

#saveRDS(acs_state, "data/acs_state.RDS")
