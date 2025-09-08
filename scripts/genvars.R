# Generate variables to explore

library(tidyverse)

# Data ----
# SBAR:
sbar_behavior_codes <- read_csv("data/sbar_behavior_codes.csv")
sbar_div <- read_csv("data/sbar_division.csv") %>% 
  mutate(GEOID = as.character(GEOID))
sbar_reg <- read_csv("data/sbar_region.csv")
sbar_state <- read_csv("data/sbar_state.csv")
sbar_sch <- read_csv("data/sbar_school.csv")

# VDOE enrollment counts:
enroll_div <- read_csv("data/enroll_division.csv") %>% 
  mutate(GEOID = as.character(GEOID))
enroll_reg <- read_csv("data/enroll_region.csv")
enroll_sch <- read_csv("data/enroll_school.csv")
enroll_state <- read_csv("data/enroll_state.csv")

# Codes ----
bullying_codes <- c("RB1", "RB2", "BSC6", "BSC7")
violence_codes <- c(paste0("PD", seq(1,10)), 
                    paste0("PD", seq(11,15)),
                    paste0("BESO", seq(1,4)))
hostile_environ_codes <- c(paste0("BSC", seq(1,4)),
                           paste0("BSC", seq(14,22)),
                           paste0("BSC", seq(26,27)),
                           paste0("BESO", seq(5,18)),
                           "PD16")

# Measures ----
## bullying ----
### state ----
bully_state <- sbar_state %>% 
  filter(behavior_code %in% bullying_codes) %>% 
  group_by(school_year) %>% 
  summarize(n_events = sum(number_of_events, na.rm = TRUE)) %>% 
  left_join(enroll_state %>% 
              select(school_year, enrolled = total_count)) %>% 
  mutate(bullying_rate = (n_events / enrolled) * 1000)

### region ----
bully_region <- sbar_reg %>% 
  filter(behavior_code %in% bullying_codes,
         !is.na(region_name)) %>% 
  group_by(school_year, region_name) %>% 
  summarize(n_events = sum(total_events, na.rm = TRUE)) %>% 
  left_join(enroll_reg %>% 
              select(school_year, region_name, region_number, enrolled = total_count)) %>% 
  mutate(bullying_rate = (n_events / enrolled) * 1000)

### division ----
bully_division <- sbar_div %>%
  filter(behavior_code %in% bullying_codes,
         !is.na(GEOID)) %>% 
  group_by(school_year, region_number, division_number, GEOID) %>% 
  summarise(n_events = sum(number_of_events, na.rm = TRUE)) %>%
  left_join(enroll_div %>% 
              select(school_year, region_number, region_name, division_number, division_name, enrolled = total_count),
            by = c("school_year", "region_number", "division_number")) %>% 
  mutate(bullying_rate = (n_events / enrolled) * 1000)

### school ----
bully_school <- sbar_sch %>%
  filter(behavior_code %in% bullying_codes) %>% 
  group_by(school_year, division_number, division_name, school_number, school_name) %>% 
  summarise(n_events = sum(number_of_events, na.rm = TRUE)) %>%
  # switched to right join to incorporate schools with 0 of these events
  right_join(enroll_sch %>% 
              select(school_year, division_number, school_number, school_name, enrolled = total_count),
            by = c("school_year", "division_number", "school_number")) %>% 
  mutate(n_events = ifelse(is.na(n_events), 0, n_events),
         bullying_rate = (n_events / enrolled) * 1000,
         school_level = case_when(
           grepl("Elem", school_name.x) ~ "Elementary",
           grepl("Middle", school_name.x) ~ "Middle",
           grepl("High", school_name.x) ~ "High",
           TRUE ~ "Unknown"
         ))

## violence ----
### state ----
violence_state <- sbar_state %>% 
  filter(behavior_code %in% violence_codes) %>% 
  group_by(school_year) %>% 
  summarize(n_events = sum(number_of_events, na.rm = TRUE)) %>% 
  left_join(enroll_state %>% 
              select(school_year, enrolled = total_count)) %>% 
  mutate(violence_rate = (n_events / enrolled) * 1000)

### region ----
violence_region <- sbar_reg %>% 
  filter(behavior_code %in% violence_codes,
         !is.na(region_name)) %>% 
  group_by(school_year, region_name) %>% 
  summarize(n_events = sum(total_events, na.rm = TRUE)) %>% 
  left_join(enroll_reg %>% 
              select(school_year, region_name, region_number, enrolled = total_count)) %>% 
  mutate(violence_rate = (n_events / enrolled) * 1000)

### division ----
violence_division <- sbar_div %>%
  filter(behavior_code %in% violence_codes,
         !is.na(GEOID)) %>% 
  group_by(school_year, region_number, division_number, GEOID) %>% 
  summarise(n_events = sum(number_of_events, na.rm = TRUE)) %>%
  left_join(enroll_div %>% 
              select(school_year, region_number, region_name, division_number, division_name, enrolled = total_count),
            by = c("school_year", "region_number", "division_number")) %>% 
  mutate(violence_rate = (n_events / enrolled) * 1000)

### school ----
violence_school <- sbar_sch %>%
  filter(behavior_code %in% violence_codes) %>% 
  group_by(school_year, division_number, division_name, school_number, school_name) %>% 
  summarise(n_events = sum(number_of_events, na.rm = TRUE)) %>%
  # switched to right join to incorporate schools with 0 of these events
  right_join(enroll_sch %>% 
               select(school_year, division_number, school_number, school_name, enrolled = total_count),
             by = c("school_year", "division_number", "school_number")) %>% 
  mutate(n_events = ifelse(is.na(n_events), 0, n_events),
         violence_rate = (n_events / enrolled) * 1000,
         school_level = case_when(
           grepl("Elem", school_name.x) ~ "Elementary",
           grepl("Middle", school_name.x) ~ "Middle",
           grepl("High", school_name.x) ~ "High",
           TRUE ~ "Unknown"
         ))

## hostile environment ----
### state ----
hostenv_state <- sbar_state %>% 
  filter(behavior_code %in% hostile_environ_codes) %>% 
  group_by(school_year) %>% 
  summarize(n_events = sum(number_of_events, na.rm = TRUE)) %>% 
  left_join(enroll_state %>% 
              select(school_year, enrolled = total_count)) %>% 
  mutate(hostenv_rate = (n_events / enrolled) * 1000)

### region ----
hostenv_region <- sbar_reg %>% 
  filter(behavior_code %in% hostile_environ_codes,
         !is.na(region_name)) %>% 
  group_by(school_year, region_name) %>% 
  summarize(n_events = sum(total_events, na.rm = TRUE)) %>% 
  left_join(enroll_reg %>% 
              select(school_year, region_name, region_number, enrolled = total_count)) %>% 
  mutate(hostenv_rate = (n_events / enrolled) * 1000)

### division ----
hostenv_division <- sbar_div %>%
  filter(behavior_code %in% hostile_environ_codes,
         !is.na(GEOID)) %>% 
  group_by(school_year, region_number, division_number, GEOID) %>% 
  summarise(n_events = sum(number_of_events, na.rm = TRUE)) %>%
  left_join(enroll_div %>% 
              select(school_year, region_number, region_name, division_number, division_name, enrolled = total_count),
            by = c("school_year", "region_number", "division_number")) %>% 
  mutate(hostenv_rate = (n_events / enrolled) * 1000)

### school ----
hostenv_school <- sbar_sch %>%
  filter(behavior_code %in% hostile_environ_codes) %>% 
  group_by(school_year, division_number, division_name, school_number, school_name) %>% 
  summarise(n_events = sum(number_of_events, na.rm = TRUE)) %>%
  # switched to right join to incorporate schools with 0 of these events
  right_join(enroll_sch %>% 
               select(school_year, division_number, school_number, school_name, enrolled = total_count),
             by = c("school_year", "division_number", "school_number")) %>% 
  mutate(n_events = ifelse(is.na(n_events), 0, n_events),
         hostenv_rate = (n_events / enrolled) * 1000,
         school_level = case_when(
           grepl("Elem", school_name.x) ~ "Elementary",
           grepl("Middle", school_name.x) ~ "Middle",
           grepl("High", school_name.x) ~ "High",
           TRUE ~ "Unknown"
         ))

# Combine ----
state <- bully_state %>% 
  left_join(violence_state, by = c("school_year", "enrolled")) %>% 
  left_join(hostenv_state, by = c("school_year", "enrolled")) %>% 
  select(school_year, enrolled, everything())

region <- bully_region %>% 
  left_join(violence_region, by = c("school_year", "region_name", "region_number", "enrolled")) %>% 
  left_join(hostenv_region, by = c("school_year", "region_name", "region_number", "enrolled")) %>% 
  select(school_year, region_name, region_number, enrolled, everything())

division <- bully_division %>% 
  left_join(violence_division, by = c("school_year", "region_number", "region_name",
                                      "division_number", "division_name", "GEOID", 
                                      "enrolled")) %>% 
  left_join(hostenv_division, by = c("school_year", "region_number", "region_name",
                                     "division_number", "division_name", "GEOID", 
                                     "enrolled")) %>% 
  select(school_year, region_number, region_name, division_number, division_name, 
         GEOID, enrolled, everything())
  
school <- bully_school %>% 
  left_join(violence_school, by = c("school_year", "division_number",
                                    "division_name", "school_number", 
                                    "school_name.x", "school_name.y",
                                    "enrolled", "school_level")) %>% 
  left_join(hostenv_school, by = c("school_year", "division_number",
                                   "division_name", "school_number", 
                                   "school_name.x", "school_name.y",
                                   "enrolled", "school_level")) %>% 
  select(school_year, division_number, division_name, school_number,
         school_name.x, school_name.y, school_level, enrolled, everything())

# save ----
save(state, region, division, school, file = "data/sbar_levels.Rdata")
