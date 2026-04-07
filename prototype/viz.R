# Explore visualizations for prototype 

library(tidyverse)

prototype_data <- read_csv("prototype/prototype_data.csv")

# Reduce to most recent values ----
# Bully information as of 23/24:
bully_recent <- prototype_data %>%
  filter(school_year == "2023-2024") %>%
  select(school_year:division_name, sch_id:school_name, n_bullying_incidents:bully_rate_per_1k) %>%
  mutate(label = case_when(
    locality_grouping == "state" ~ "State Average",
    locality_grouping == "region" ~ region_name, 
    locality_grouping == "division" ~ division_name,
    TRUE ~ school_name))

# Climate data is more complicated: 21/22 AND 22/23
climate <- prototype_data %>%
  filter(str_detect(school_year, "2022")) %>%
  select(school_year:division_name, sch_id:school_name, n_students_surveyed:avg_bully_prob)

climate_recent_sch <- climate %>% 
  filter(locality_grouping == "school",
         !is.na(n_students_surveyed)) %>%
  group_by(school_name) %>%
  filter(n() == 1 | school_year == "2022-2023") %>%
  ungroup()

# High schools in 21-22 and middle schools in 22-23
climate_recent_div <- climate %>%
  filter(locality_grouping == "division") %>%
  mutate(division_name_ext = case_when(
    school_year == "2022-2023" ~ paste0(division_name, " Middle Schools"),
    TRUE ~ paste0(division_name, " High Schools")))

climate_recent_reg <- climate %>%
  filter(locality_grouping == "region") %>%
  mutate(region_name_ext = case_when(
    school_year == "2022-2023" ~ paste0(region_name, " Middle Schools"),
    TRUE ~ paste0(region_name, " High Schools")))

climate_recent_state <- climate %>%
  filter(locality_grouping == "state") %>%
  mutate(state_name_ext = case_when(
    school_year == "2022-2023" ~ "State Average - Middle Schools",
    TRUE ~ "State Average - High Schools"))

climate_recent <- bind_rows(climate_recent_state, climate_recent_reg, climate_recent_div, climate_recent_sch) %>%
  mutate(label = case_when(
    !is.na(state_name_ext) ~ state_name_ext,
    !is.na(region_name_ext) ~ region_name_ext, 
    !is.na(division_name_ext) ~ division_name_ext,
    TRUE ~ school_name))

# Staffing and disadvantage 24/25
staff_disadv_recent <- prototype_data %>%
  filter(school_year == "2024-2025") %>%
  select(school_year:pct_disadv, total_positions:staff_per_1k_students) %>%
  mutate(label = case_when(
    locality_grouping == "state" ~ "State Average",
    locality_grouping == "region" ~ region_name, 
    locality_grouping == "division" ~ division_name,
    TRUE ~ school_name))

# Visualize All ----
# Pivot everything longer:
staff_disad_long <- staff_disadv_recent %>%
  select(label, locality_grouping, pct_disadv, staff_per_1k_students) %>%
  pivot_longer(cols = c(pct_disadv, staff_per_1k_students))

climate_long <- climate_recent %>%
  select(label, locality_grouping, pct_mental_health_training:avg_bully_prob) %>%
  pivot_longer(cols = c(pct_mental_health_training:avg_bully_prob))

bully_long <- bully_recent %>%
  select(label, locality_grouping, bully_rate_per_1k) %>%
  pivot_longer(cols = bully_rate_per_1k)

# Combine:
plt_dat <- rbind(staff_disad_long, climate_long, bully_long)

# Plot Example Schools:
plt_state <- plt_dat %>% filter(locality_grouping == "state")

# plt_dat %>%
#   count(label, sort = TRUE) %>%
#   slice(1) # Liberty Middle 

plt_highlight <- plt_dat %>% filter(label == "Liberty Middle")

plt_dat %>%
  filter(locality_grouping == "school", 
         !label %in% c("Amelia Street Special Education")) %>%
  ggplot(aes(x = value, y = name)) +
  geom_jitter(height = 0.1, alpha = 0.03) +
  geom_point(data = plt_state, color = "red") +
  geom_point(data = plt_highlight, color = "yellow") +
  theme_minimal() +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  theme(strip.text.x = element_blank())

plt_highlight_div <- plt_dat %>% filter(label == "Albemarle County")

plt_dat %>%
  filter(locality_grouping == "division") %>%
  ggplot(aes(x = value, y = name)) +
  geom_jitter(height = 0.1, alpha = 0.03) +
  geom_point(data = plt_state, color = "red") +
  geom_point(data = plt_highlight_div, color = "green") +
  #theme_minimal() +
  facet_wrap(~ name, ncol = 1, scales = "free") +
  theme(strip.text.x = element_blank())



