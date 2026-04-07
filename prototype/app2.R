# Explore student mental health across different datasets 

# Setup ----
library(shiny)
library(bslib)
library(ggbeeswarm)
library(tidyverse)

prototype_data <- readr::read_csv("prototype_data.csv")

# Dataprep ----
## Reduce to most recent values 
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

## Pivot longer & combine 
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



# UI ----
ui <- 
  page_sidebar(
    theme = bs_theme(version = 5),
    title = "Prototype v2: Exploring Youth Mental Health in Virginia",
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
    navset_card_tab(
      nav_panel(title = "At a Glance", 
                textOutput("all_values_header"),
                plotOutput("all_values_plt")),
      nav_panel(title = "Explore"),
      nav_panel(title = "Compare")
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
    # Update the selectizeInput choices
    updateSelectizeInput(
      session,
      "highlight",
      choices = choices,
      server = TRUE
    )
  })
  
  # Compare all values plot ----
  # Reactive data for plotting main data
  plot_data <- reactive({
    req(input$locality)  
    
    # Filter the main dataset based on locality
    main_dat <- plt_dat %>%
      filter(locality_grouping == input$locality)
    
    # Drop outlier
    if (input$locality == "school") {
      main_dat <- main_dat %>%
        filter(!label %in% c("Amelia Street Special Education"))
    }
    
    main_dat
  })
  
  # Reactive data for the selected highlighted point
  highlight_data <- reactive({
    req(input$highlight)  
    plt_dat %>%
      filter(label == input$highlight)
  })
  
  # Reactive data for state-level points
  state_data <- reactive({
    plt_dat %>%
      filter(locality_grouping == "state")
  })
  
  # Render the plot:
  output$all_values_plt <- renderPlot({
    ggplot(plot_data(), aes(x = value, y = name)) +
      geom_beeswarm(alpha = 0.1, color = "black", show.legend = FALSE) +
      geom_point(
        data = dplyr::bind_rows(
          dplyr::mutate(state_data(), type = "State"),
          dplyr::mutate(highlight_data(), type = "Highlighted")),
        aes(color = type, shape = type), size = 4) +
      theme_bw() +
      facet_wrap(~ name, ncol = 1, scales = "free") +
      theme(
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11)) +
      labs(x = NULL, y = NULL, color = NULL, shape = NULL) +
      scale_color_manual(
        values = c("State" = "blue", "Highlighted" = "red"),
        labels = c("State" = "State Average", "Highlighted" = "Selected Area")) +
      scale_shape_manual(
        values = c("State" = 17, "Highlighted" = 15),
        labels = c("State" = "State Average", "Highlighted" = "Selected Area")) +
      scale_y_discrete(labels = c("bully_rate_per_1k" = "Bullying Rate",
                                  "avg_bully_prob" = "Average\nBullying Problem",
                                  "avg_peer_rel" = "Average Relationship\nwith Peers",
                                  "avg_adult_rel" = "Average Relationship\nwith Adults",
                                  "pct_mental_health_training" = "Percent that Received\nMental Health Training",
                                  "pct_disadv" = "Percent\nEconomically\nDisadvantaged",
                                  "staff_per_1k_students" = "Rate of Mental\nHealth Staff"))
  })
  
  
  # Header for plot:
  output$all_values_header <- renderText({
    highlight <- input$highlight
    grouping <- input$locality

    # Handle NULL / empty cases safely
    if (is.null(highlight) || highlight == "") {
      highlight <- "your value"
      grouping <- "other group"
    }

    paste0("Compare ", highlight, " to all ", grouping, "s")
  })

  # Show selected values
  # output$selected <- renderPrint({
  #   list(
  #     locality = input$locality,
  #     highlight = input$highlight
  #   )
  # })
}

shinyApp(ui, server)

