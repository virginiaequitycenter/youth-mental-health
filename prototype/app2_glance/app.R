# Create standalone app to show all youth mental health variables at a glance with a particular region highlighted 
# This is equivalent to just the At-A-Glance tab on app1_all.R 

# Setup ----
library(shiny)
library(bslib)
library(ggbeeswarm)
library(plotly)
library(tidyverse)

# For school levels
school_levels <- read_csv("school_key.csv") %>%
  select(grade_standard, sch_id)

prototype_data <- read_csv("prototype_data.csv") %>%
  left_join(school_levels, by = "sch_id") %>%
  filter(
    locality_grouping != "school" |
      grade_standard %in% c("High", "Middle"))

# Dataprep ----
## Reduce to most recent values ----
# Bully information as of 23/24:
bully_recent <- prototype_data %>%
  filter(school_year == "2023-2024") %>%
  select(school_year:division_name, sch_id:school_name, n_bullying_incidents:bully_rate_per_1k) %>%
  mutate(
    label = case_when(
      locality_grouping == "state" ~ "State Average",
      locality_grouping == "region" ~ region_name, 
      locality_grouping == "division" ~ division_name,
      TRUE ~ school_name),
    # If bully rate is NA make 0 
    bully_rate_per_1k = case_when(
      is.na(bully_rate_per_1k) ~ 0,
      TRUE ~ bully_rate_per_1k))

# Climate data is more complicated: 
# - years are 21/22 AND 22/23
# - different state averages calculated (middle school, high school, combined)
climate <- prototype_data %>%
  filter(str_detect(school_year, "2022")) %>%
  select(school_year:division_name, sch_id:school_name, n_students_surveyed:avg_bully_prob, grade_standard)

# School:
# we only care about school type here 
climate_recent_sch <- climate %>% 
  filter(locality_grouping == "school",
         !is.na(n_students_surveyed)) %>%
  group_by(school_name) %>%
  filter(n() == 1 | school_year == "2022-2023") %>%
  ungroup()

# Division:
# High schools in 21-22 and middle schools in 22-23

# Calculate division averages across years
climate_div_avg <- climate %>%
  filter(locality_grouping == "division") %>%
  group_by(division_number, division_name) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "division")

# Region:
# Calculate region averages across years 
reg_avg <- climate %>%
  filter(locality_grouping == "region") %>%
  group_by(region_number, region_name) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "region")

# State:
# Calculate state average across years
state_avg <- climate %>%
  filter(locality_grouping == "state") %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "state",
    region_number = NA_real_)

# Combine all climate data: 
climate_recent <- bind_rows(state_avg, reg_avg, climate_div_avg, climate_recent_sch) %>%
  mutate(label = case_when(
    locality_grouping == "school" ~ school_name,
    locality_grouping == "division" ~ division_name,
    locality_grouping == "region" ~ region_name,
    TRUE ~ "State Average"
  ))

# Staffing and disadvantage 24/25
staff_disadv_recent <- prototype_data %>%
  filter(school_year == "2024-2025") %>%
  select(school_year:pct_disadv, total_positions:staff_per_1k_students) %>%
  mutate(label = case_when(
    locality_grouping == "state" ~ "State Average",
    locality_grouping == "region" ~ region_name, 
    locality_grouping == "division" ~ division_name,
    TRUE ~ school_name))

## Standardize on measure names ----
measure_key <- tibble::tribble(
  ~label,                         ~name,                         ~year,          ~units,
  "Bullying Rate",                "bully_rate_per_1k",           "2024",         "Incidents per 1,000 Students",
  "Avg. Bullying Problem",        "avg_bully_prob",              "2022/2023",    "Average Rating",
  "Relationships with Peers",     "avg_peer_rel",                "2022/2023",    "Average Rating",
  "Relationships with Adults",    "avg_adult_rel",               "2022/2023",    "Average Rating",
  "Mental Health Training (%)",   "pct_mental_health_training",  "2022/2023",    "Percent of Students",
  "Economically Disadv. (%)",     "pct_disadv",                  "2025",         "Percent of Students",
  "Mental Health Staff Rate",     "staff_per_1k_students",       "2025",         "Staff per 1,000 Students"
)


## Pivot longer & then combine 
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
plt_dat <- rbind(staff_disad_long, climate_long, bully_long) %>%
  left_join(measure_key %>% select(name, year, units), by = "name")


# UI ----
ui <- 
  page_sidebar(
    theme = bs_theme(version = 5),
    title = "At a Glance - Youth Mental Health",
    sidebar = sidebar(
      title = "Select Measures",
      
      # Locality Grouping:
      selectInput(
        "locality",
        "Grouped By:",
        choices = c("school", "division", "region"),
        selected = "school"
      ),
      
      # Highlight Value:
      selectizeInput(
        "highlight",
        "Type to select your area:",
        choices = NULL,   
        multiple = FALSE,
        options = list(placeholder = "Start typing")
      ),
    ),
    card(
      textOutput("all_values_header"),
      plotlyOutput("all_values_plt")
    )
  )

# Server ----
server <- function(input, output, session) {
  
  # Dynamically update highlight options based on locality:
  observeEvent(input$locality, {
    
    # Filter plt_dat based on selected locality
    choices <- plt_dat %>%
      filter(locality_grouping == input$locality) %>%
      pull(label) %>%
      unique() %>%
      sort()
    
    # Update the selectizeInput choices:
    updateSelectizeInput(
      session,
      "highlight",
      choices = choices,
      server = TRUE
    )
  })
  
  # Compare All ----
  # Reactive data for plotting main data:
  plot_data <- reactive({
    req(input$locality)  
    
    # Filter the main dataset based on locality:
    main_dat <- plt_dat %>%
      filter(locality_grouping == input$locality)
    
    # Drop outliers:
    if (input$locality == "school") {
      main_dat <- main_dat %>%
        filter(!label %in% c("Amelia Street Special Education", 
                             "Richmond Career Education and Employment Charter School"))
    }
    
    main_dat %>%
      mutate(tooltip = paste0(label,
                              "<br>", round(value, 2), " ", units,
                              "<br>", year))
  })
  
  # Reactive data for the selected highlighted point:
  highlight_data <- reactive({
    req(input$highlight)  
    plt_dat %>%
      filter(label == input$highlight) %>%
      mutate(tooltip = paste0(label,
                              "<br>", round(value, 2), " ", units,
                              "<br>", year))
  })
  
  # Reactive data for state-level points:
  state_data <- reactive({
    plt_dat %>%
      filter(locality_grouping == "state") %>%
      mutate(tooltip = paste0(label,
                              "<br>", round(value, 2), " ", units,
                              "<br>", year))
  })
  
  # Beeswarm ----
  output$all_values_plt <- renderPlotly({
    
    req(input$highlight)
    highlight_label <- as.character(input$highlight)
    
    highlight_df <- bind_rows(
      mutate(state_data(), type = "State average"),
      mutate(highlight_data(), type = highlight_label))
    
    color_vals <- c("State average" = "#D95F02FF")
    color_vals[highlight_label] = "#1B9E77FF"
    
    shape_vals <- c("State average" = 17)
    shape_vals[highlight_label] = 15
    
    p <- ggplot(plot_data(), aes(x = value, y = name)) +
      geom_beeswarm(alpha = 0.10, color = "black", show.legend = FALSE, size = 1, aes(text = tooltip)) +
      geom_point(data = highlight_df, aes(color = type, shape = type, text = tooltip), size = 3) +
      theme_bw(base_size = 12) +
      facet_wrap(~ name, ncol = 1, scales = "free") +
      theme(
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 10)) +
      labs(x = NULL, y = NULL, color = NULL, shape = NULL) +
      scale_color_manual(values = color_vals ) +
      scale_shape_manual(values = shape_vals) +
      scale_y_discrete(labels = c(
        bully_rate_per_1k = "Bullying Rate",
        avg_bully_prob = "Avg. Bullying\nProblem",
        avg_peer_rel = "Relationships\nwith Peers",
        avg_adult_rel = "Relationships\nwith Adults",
        pct_mental_health_training = "Mental Health\nTraining (%)",
        pct_disadv = "Economically\nDisadv. (%)",
        staff_per_1k_students = "Mental Health\nStaff Rate"
      ))
    
    ggplotly(p, 
             tooltip = "text") %>%
      config(displayModeBar = FALSE, displaylogo = FALSE)
  })
  
  # Header for plot ----
  output$all_values_header <- renderText({
    highlight <- input$highlight
    grouping <- input$locality
    
    # Handle NULL / empty cases safely:
    if (is.null(highlight) || highlight == "") {
      highlight <- "your value"
      grouping <- "other group"
    }
    
    paste0("See ", highlight, " across all indicators")
  })
  
}

shinyApp(ui, server)
