# Explore student mental health across different datasets 

# Setup ----
library(shiny)
library(bslib)
library(ggbeeswarm)
library(plotly)
library(tidyverse)

# For school levels
school_levels <- read_csv("../data/school_key.csv") %>%
  select(grade_standard, sch_id)

prototype_data <- readr::read_csv("prototype_data.csv") %>%
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

# climate_recent_div <- climate %>%
#   filter(locality_grouping == "division") %>%
#   mutate(division_name_ext = case_when(
#     school_year == "2022-2023" ~ paste0(division_name, " Middle Schools"),
#     TRUE ~ paste0(division_name, " High Schools")))

# Calculate division averages across years
climate_div_avg <- climate %>%
  filter(locality_grouping == "division") %>%
  group_by(division_number, division_name) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "division")

# Combine
#climate_recent_div <- bind_rows(climate_recent_div, div_avg)

# Region:
# climate_recent_reg <- climate %>%
#   filter(locality_grouping == "region") %>%
#   mutate(region_name_ext = case_when(
#     school_year == "2022-2023" ~ paste0(region_name, " Middle Schools"),
#     TRUE ~ paste0(region_name, " High Schools")))

# Calculate region averages across years 
reg_avg <- climate %>%
  filter(locality_grouping == "region") %>%
  group_by(region_number, region_name) %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "region")

# Combine
#climate_recent_reg <- bind_rows(climate_recent_reg, reg_avg)

# State:
# climate_recent_state <- climate %>%
#   filter(locality_grouping == "state") %>%
#   mutate(state_name_ext = case_when(
#     school_year == "2022-2023" ~ "State Average - Middle Schools",
#     TRUE ~ "State Average - High Schools"))

# Calculate state average across years
state_avg <- climate %>%
  filter(locality_grouping == "state") %>%
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    school_year = "2021-2023",
    locality_grouping = "state",
    region_number = NA_real_)

# Combine climate state 
# climate_recent_state <-bind_rows(climate_recent_state, state_avg_row)

# Combine all climate data 

climate_recent <- bind_rows(state_avg, reg_avg, div_avg, climate_recent_sch) %>%
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
plt_dat <- rbind(staff_disad_long, climate_long, bully_long) 



# UI ----
ui <- 
  page_sidebar(
    theme = bs_theme(version = 5),
    title = "Prototype: Exploring Youth Mental Health in Virginia",
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
                plotlyOutput("all_values_plt")),
      nav_panel(title = "Explore",
                selectInput("measure", "Select measure to explore:", choices = c(
                  "Bullying Rate",
                  "Avg. Bullying Problem",
                  "Relationships with Peers",
                  "Relationships with Adults",
                  "Mental Health Training (%)",
                  "EconomicallyDisadv. (%)",
                  "Mental Health Staff Rate"
                )),
                layout_columns(
                  col_widths = c(6, 6),
                  row_heights = c(2, 2),
                  card(card_header("Rank"), "Lollipop plot"),
                  card(card_header("Severity"), "Quintiles plot"),
                  card(card_header("Change"), "Line plot"),
                  card(card_header("Locality"), "Map")
                )),
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
  output$all_values_plt <- renderPlotly({
    
    req(input$highlight)
    highlight_label <- as.character(input$highlight)
    
   
    highlight_df <- dplyr::bind_rows(
      dplyr::mutate(state_data(), type = "State average"),
      dplyr::mutate(highlight_data(), type = highlight_label))
    
    color_vals <- c("State average" = "#D95F02FF")
    color_vals[highlight_label] = "#1B9E77FF"
    
    shape_vals <- c("State average" = 17)
    shape_vals[highlight_label] = 15
    
    p <- ggplot(plot_data(), aes(x = value, y = name)) +
      geom_beeswarm(alpha = 0.10, color = "black", show.legend = FALSE, size = 1,
                    aes(text = paste0(label,": ", round(value)))) +
      geom_point(
        data = highlight_df, 
        aes(color = type, shape = type, 
            text = paste0(label,": ", round(value))), 
        size = 3) +
      #theme_minimal() +
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
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE, displaylogo = FALSE)
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

    paste0("See ", highlight, " across all indicators")
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

