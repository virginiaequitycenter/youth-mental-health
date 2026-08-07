# Explore visualizations for prototype 

library(tidyverse)

prototype_data <- read_csv("prototype_data.csv")

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

# Rank ----

# AHS bullying rate

bully_dat <- prototype_data %>%
  filter(locality_grouping == "school",
         division_name == "Albemarle County", 
         school_year == "2023-2024", 
         !str_detect(school_name, "Elementary")) %>%
  select(school_name, bully_rate_per_1k) %>%
  mutate(bully_rate_per_1k = replace_na(bully_rate_per_1k, 0))

bully_highlight <- bully_dat %>%
  filter(school_name == "Albemarle High")

state_highlight <- prototype_data %>%
  filter(locality_grouping == "state" ,
         school_year == "2023-2024") %>%
  select(school_name, bully_rate_per_1k) %>%
  mutate(
    school_name = case_when(
      is.na(school_name) ~ "State Average",
      TRUE ~ school_name))

bully_lolli <- bind_rows(bully_dat, state_highlight)
  

bully_lolli %>%
  ggplot(aes(x = bully_rate_per_1k, y = fct_reorder(school_name, bully_rate_per_1k))) +
  geom_segment(aes(x = 0, xend = bully_rate_per_1k)) +
  geom_segment(data = bully_highlight, aes(x = 0, xend = bully_rate_per_1k), color = "#1B9E77FF", linewidth = 1) +
  geom_segment(data = state_highlight, aes(x = 0, xend = bully_rate_per_1k), color = "#D95F02FF", linewidth = 1) +
  geom_point(size = 3) +
  geom_point(data = bully_highlight, aes(x = bully_rate_per_1k), color = "#1B9E77FF", size = 3) +
  geom_point(data = state_highlight, aes(x = bully_rate_per_1k), color = "#D95F02FF", size = 3) +
  labs(x = "Incidents per 1000 Students", 
       y = NULL,
       title = "Bullying Rates in Albemarle County, 2023-2024") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )

# Quintile ----

bully_quintiles <- prototype_data %>%
  filter(locality_grouping == "school",
         school_year == "2023-2024", 
         !str_detect(school_name, "Elementary")) %>%
  select(school_name, bully_rate_per_1k) %>%
  mutate(
    bully_rate_per_1k = replace_na(bully_rate_per_1k, 0),
    quintile = ntile(bully_rate_per_1k, 5))

##Boxplot ----
ggplot(bully_quintiles, aes(x = factor(quintile), y = bully_rate_per_1k)) +
  geom_boxplot(fill = "#a6cee3") +
  labs(
    x = "Quintile (1 = lowest bullying rate)",
    y = "Incidents per 1000 Students",
    title = "Bullying Rates by Quintile"
  ) +
  theme_minimal()

##Scatter ----
ggplot(bully_quintiles, aes(x = factor(quintile), y = bully_rate_per_1k)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_point(
    data = subset(bully_quintiles, school_name %in% c("State Average", "Albemarle High")),
    aes(color = school_name),
    size = 3
  ) +
  scale_color_manual(values = c(
    "State Average" = "#D95F02FF",
    "Albemarle High" = "#1B9E77FF"
  )) +
  theme_minimal()

## *Bar plot ----

quintile_summary <- bully_quintiles %>%
  group_by(quintile) %>%
  summarise(
    mean_rate = mean(bully_rate_per_1k, na.rm = TRUE),
    n = n()) %>%
  mutate(highlight = quintile)

ggplot(quintile_summary, aes(x = factor(quintile), y = mean_rate, fill = highlight)) +
  geom_hline(data = state_highlight,
    aes(yintercept = bully_rate_per_1k),
    linetype = "dashed",
    color = "#D95F02FF",
    linewidth = 1
  ) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#1B9E77FF", "FALSE" = "#303234"), guide = "none") +
  geom_text(aes(label = round(mean_rate, 1)), vjust = -0.5, size = 3.5) +
  geom_text(aes(label = paste0("n = ", n)), vjust = -2, size = 3, color = "gray30") +
  geom_text(data = subset(quintile_summary, highlight), aes(label = "Albemarle High"), 
            vjust = -3, fontface = "bold", color = "#1B9E77FF", size = 4) +
  labs(
    x = "Quintile (1 = lowest)",
    y = "Average Incidents per 1000",
    title = "Average Bullying Rate by Quintile, 2023-2024",
    subtitle = "Across all schools") +
  theme_minimal() +
  expand_limits(y = max(quintile_summary$mean_rate) * 1.2)

# Distance from mean


## Ntile ----
ntiles <- prototype_data %>%
  filter(locality_grouping == "school",
         school_year == "2023-2024", 
         !str_detect(school_name, "Elementary")) %>%
  select(school_name, bully_rate_per_1k) %>%
  mutate(bully_rate_per_1k = replace_na(bully_rate_per_1k, 0),
         percentile = percent_rank(bully_rate_per_1k))

# Change ----

# Line chart of change in bully rates over time for AHS

bully_change <- prototype_data %>%
  filter(locality_grouping == "school",
         division_name == "Albemarle County",
         !str_detect(school_name, "Elementary"),
         school_year != "2024-2025",
         !school_name %in% c("Journey Middle", "Lakeside Middle", "Virtual High School", "Community Lab School")) %>%
  select(school_year, school_name, bully_rate_per_1k) %>%
  mutate(bully_rate_per_1k = replace_na(bully_rate_per_1k, 0))

alb_change <- bully_change %>%
  filter(school_name == "Albemarle High")

va_change <- prototype_data %>%
  filter(locality_grouping == "state",
         school_year != "2024-2025") %>%
  select(school_year, school_name, bully_rate_per_1k) %>%
  mutate(school_name == "State Average")

bully_change %>%
  ggplot(aes(x = school_year, y = bully_rate_per_1k)) +
  geom_point(color = "darkgrey") +
  geom_line(aes(group = school_name), color = "darkgrey") +
  geom_point(data = va_change, 
             aes(color = "State Average"),
             size = 3, shape = 17) +
  geom_line(data = va_change, 
            aes(group = school_name, color = "State Average"),
            linewidth = 1.5) +
  geom_point(data = alb_change, 
             aes(color = "Albemarle High"),
             size = 3, shape = 15) +
  geom_line(data = alb_change, 
            aes(group = school_name, color = "Albemarle High"),
            linewidth = 1.5) +
  theme_minimal() +
  labs(title = "Change in Bullying Rates for Albemarle High",
       x = NULL,
       y = "Bullying Rate",
       color = NULL) +
  scale_x_discrete(expand = c(0.10, 0.10)) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), 
        legend.position = "top") +
  scale_color_manual(values = c("State Average" = "#D95F02FF",
                                "Albemarle High" = "#1B9E77FF"))

# Scatter
sc <- prototype_data %>%
  filter(locality_grouping == "school",
         division_name == "Albemarle County",
         !str_detect(school_name, "Elementary"),
         school_year == "2023-2024",
         !school_name == "Albemarle High") %>%
  select(school_name, total_enrolled, staff_per_1k_students, bully_rate_per_1k) %>%
  mutate(bully_rate_per_1k = replace_na(bully_rate_per_1k, 0))

sc_alb <- prototype_data %>%
  filter(locality_grouping == "school",
         school_year == "2023-2024",
         school_name == "Albemarle High") %>%
  select(school_name, total_enrolled, staff_per_1k_students, bully_rate_per_1k) %>%
  mutate(bully_rate_per_1k = replace_na(bully_rate_per_1k, 0))

sc_va <- prototype_data %>%
  filter(locality_grouping == "state",
         school_year == "2023-2024") %>%
  select(school_name, total_enrolled, staff_per_1k_students, bully_rate_per_1k) %>%
  mutate(school_name = "State Average")
  
sc %>%
  ggplot(aes(x = staff_per_1k_students, y = bully_rate_per_1k)) +
  geom_point(aes(size = total_enrolled)) +
  geom_point(data = sc_alb, 
             aes(x = staff_per_1k_students, y = bully_rate_per_1k, 
                 color = "Albemarle High", size = total_enrolled, shape = "Albemarle High")) +
  geom_point(data = sc_va,
             aes(x = staff_per_1k_students, y = bully_rate_per_1k,
                 color = "State Average", size = 1000, shape = "State Average")) +
  theme_minimal() +
  labs(title = "Bullying Rates compared to Mental Health Staffing Rates",
       subtitle = "Albemarle County Schools",
       x = "Mental Health Staffing Rate",
       y = "Bullying Rate",
       color = NULL,
       shape = NULL,
       size = NULL) +
  guides(size = "none") +
  theme(legend.position = "top") +
  scale_color_manual(values = c("State Average" = "#D95F02FF",
                                "Albemarle High" = "#1B9E77FF")) +
  scale_shape_manual(values = c(
    "State Average" = 17,
    "Albemarle High" = 15
  ))
  
  
