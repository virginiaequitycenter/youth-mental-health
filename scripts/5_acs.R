# Script to pull and prep American Community Survey geometry data across different 
# geographical groupings in Virginia. 
# Explorer: https://data.census.gov/table/ACSDT1Y2023.C14002?q=Type+of+School&t=Age+and+Sex:Populations+and+People&g=040XX00US51

# Note on terminology: 
# District = school district, approximated by county 
# Region = group of school districts/counties, 8 total 

# Libraries ----
library(sf)
library(tidycensus)
library(tidyverse)

# For consistency on regions across datasets, we're using the regions data from:
# https://www.doe.virginia.gov/about-vdoe/virginia-school-directories/virginia-public-school-listing-by-region

regions <- read_csv("data/vdoe_regions_divisions.csv")

# County/Division ----
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
  rename(pop_est = B01003_001E,
         geometry = geometry) %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  left_join(regions, by = join_by(county == division_name))

# Fix combined city/county districts: 
# - Covington City --> Alleghany County District
# - Fairfax City --> Fairfax County District
# - Emporia City --> Greensville County District
# - Williamsburg City --> Williamsburg-James City County

acs_div <- acs_div %>%
  mutate(division_name = case_when(
    county == "Covington City" ~ "Alleghany County",
    county == "Fairfax City" ~ "Fairfax County",
    county == "Emporia City" ~ "Greensville County",
    county %in% c("Williamsburg City", "James City County") ~ "Williamsburg-James City County",
    TRUE ~ county
  )) %>%
  group_by(division_name) %>%
  summarise(
    pop_est = sum(pop_est),
    geometry = st_union(geometry),
    region_name = max(region_name, na.rm = T),
    region_number = max(region_number, na.rm = T),
    division_number = max(division_number, na.rm = T)) %>%
  mutate(locality_grouping = "division")

# Region ----
# Add up totals and create new geometries:  
acs_regions <- acs_div %>%
  group_by(region_name, region_number) %>%
  summarise(pop_est = sum(pop_est, na.rm = TRUE),
            geometry = st_union(geometry)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  mutate(locality_grouping = "region")

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
  st_transform(crs = 4326) %>%
  mutate(locality_grouping = "state") %>%
  select(-GEOID, -NAME)

# Combine & Save ----

acs <- bind_rows(acs_div, acs_regions, acs_state)
saveRDS(acs, "data/acs.RDS")
