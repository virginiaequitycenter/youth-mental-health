# Explore possible data visualistions

library(tidyverse)
library(scales)
library(GGally)

load("data/sbar_levels.Rdata")

# 1. example of all divisions on one variable
# with albemarle highlighted
state <- state %>% 
  mutate(highlight = "background",
         region_name = "State")

division %>% 
  mutate(highlight = if_else(division_number == 2, "highlight", "background")) %>% 
  
  ggplot(aes(x = hostenv_rate, y = school_year, color = highlight, alpha = highlight)) +
  geom_jitter(aes(size = enrolled), height = 0.1, show_guides = FALSE) +
  geom_point(data = state, color = "black", alpha = 1, size = 3) +
  scale_color_manual(values = c("grey50", "red")) +
  scale_alpha_discrete(c(1/3, 1)) +
  labs(x = "Incidents per 1000 Students", y = "", 
       title = "Reported Incidents Contributing to a Hostile or Unsafe Environment",
       subtitle = "Albemarle County Schools relative to All Divisions and State") +
  theme_minimal()

# 2. example of all schools within a division ranked on one variable
# chesterfield, 2023-2024 (state-wide reference line)
pal_sch_type <- c("Elementary" = "#66c2a5", "Middle" = "#beaed4", "High" = "#fdc086")

school %>% 
  filter(division_number == 21, school_year == "2023-2024",
         !is.na(school_name.x)) %>% 
  ggplot(aes(x = hostenv_rate, y = fct_reorder(school_name.x, hostenv_rate))) +
  geom_segment(aes(x = 0, xend = hostenv_rate), color = "grey50") +
  geom_point(aes(color = school_level), size = 3) +
  geom_vline(xintercept = state$hostenv_rate[3], color = "grey30") +
  scale_color_manual(values = pal_sch_type) +
  labs(x = "Incidents per 1000 Students", y = "",
       title = "Reported Incidents Contributing to a Hostile or Unsafe Environment, 2023-2024",
       subtitle = "By Schools in Chesterfield County Schools",
       color = "School Level") +
  theme_minimal()

# 3. example compaing change in regions over time
region %>% 
  ggplot(aes(x = school_year, y = violence_rate, color = region_name, group = region_name)) +
  geom_line(data = state, color = "grey50", size = 2) +
  geom_point(aes(size = enrolled), show_guides = FALSE) +
  geom_line() +
  labs(x = "", y = "Incidents per 1000 Students",
       title = "Reported Incidents of Violence", 
       subtitle = "By Region, 2021-22 through 2023-24") +
  theme_minimal()

# 4. example of comparing change over time for schools within division
# notice the challenge of changing school names...
school %>% 
  filter(division_number == 2, school_year %in% c("2022-2023", "2023-2024"), !is.na(school_name.x)) %>% 
  ggplot(aes(x = hostenv_rate, y = school_name.x, color = school_year, group = school_name.x)) +
  geom_line(color = "grey50") +
  geom_point() +
  labs(x = "Incidents per 1000 Students", y = "",
       title = "Reported Incidents Contributing to a Hostile or Unsafe Environment",
       subtitle = "Albemarle County Schools, 2022-23 and 2023-24",
       caption = "Note issue of missingness for schools with name changes") +
  theme_minimal()

# 5. example of heat map of all three variables?
# comparing regions (could work for larger n as well)
# standardize rates within type
region %>% 
  bind_rows(state) %>% 
  filter(school_year == "2023-2024") %>% 
  mutate(scaled_bullying_rate = bullying_rate/max(bullying_rate),
         scaled_violence_rate = violence_rate/max(violence_rate),
         scaled_hostenv_rate = hostenv_rate/max(hostenv_rate)) %>% 
  pivot_longer(cols = c("scaled_bullying_rate", "scaled_violence_rate", "scaled_hostenv_rate"), 
               names_to = "incident_type", names_prefix = "scaled_", values_to = "incident_rate") %>% 
  mutate(region_name = factor(region_name),
         region_name = fct_relevel(region_name, "State", after = 0L)) %>% 
  ggplot(aes(x = incident_type, y = fct_rev(region_name), fill = incident_rate)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="darkorchid4") +
  labs(x = "", y = "", title = "(Scaled) Incidents Rates", subtitle = "By Region")


# 6a. example of parallel coordinates?
# Divisions within region - highlighting Charlottesville
division %>% 
  filter(region_number == 5, school_year == "2023-2024") %>% 
  mutate(highlight = if_else(division_number == 104, "highlight", "background")) %>% 
  ggparcoord(columns = c(9,11,13),
             groupColumn = 14,
             showPoints = TRUE, alphaLines = .5,
             scale = "uniminmax") +
  scale_color_manual(values = c("grey", "red"), guide = FALSE) +
  labs(x = "", y = "Scaled Incidence Rate",
       title = "(Scaled) Incident Rates", subtitle = "School Divisions in Valley Region") +
  theme_minimal()

# 6b. alternative, by hand, to add division labels on end
# make long
division_long <- division %>% ungroup() %>% 
  filter(school_year == "2023-2024") %>% 
  mutate(scaled_bullying_rate = bullying_rate/max(bullying_rate),
         scaled_violence_rate = violence_rate/max(violence_rate, na.rm = TRUE),
         scaled_hostenv_rate = hostenv_rate/max(hostenv_rate)) %>% 
  filter(region_number == 5) %>% 
  pivot_longer(cols = c("scaled_bullying_rate", "scaled_violence_rate", "scaled_hostenv_rate"), 
               names_to = "incident_type", names_prefix = "scaled_", values_to = "incident_rate") 
  
# create labels
div_labels <- division_long %>% 
  filter(incident_type == "violence_rate") %>% 
  mutate(highlight = if_else(division_number == 104, "highlight", "background"))
  
# plot
division_long %>% 
  mutate(highlight = if_else(division_number == 104, "highlight", "background")) %>% 
  ggplot(aes(x = incident_type, y = incident_rate, group = division_name, color = highlight)) +
  geom_line() + 
  geom_point() +
  geom_text(data = div_labels, aes(label = division_name), nudge_x = 0.33, size = 3) +
  scale_color_manual(values = c("grey70", "red"), guide = FALSE) +
  theme_minimal()

