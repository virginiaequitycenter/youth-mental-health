# Create standalone app to dive deeper into specific variables for a particular region, division, or school

# This is equivalent to just the Explore tab on app1_all.R 

# Setup ----
library(shiny)
library(bslib)
library(ggbeeswarm)
library(plotly)
library(tidyverse)

# Read ----
# For school levels
school_key <- read_csv("../school_key.csv")

school_levels <-  school_key %>%
  select(grade_standard, sch_id)

prototype_data <- readr::read_csv("../prototype_data.csv") %>%
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

## Standardize on measure names ----
measure_key <- tibble::tribble(
  ~label,                         ~name,                     ~units,
  "Bullying Rate",                "bully_rate_per_1k",        "Incidents per 1,000 Students",
  "Avg. Bullying Problem",        "avg_bully_prob",        "Average Rating",
  "Relationships with Peers",     "avg_peer_rel",             "Average Rating",
  "Relationships with Adults",    "avg_adult_rel",            "Average Rating",
  "Mental Health Training (%)",   "pct_mental_health_training",          "Percent",
  "Economically Disadv. (%)",     "pct_disadv",               "Percent",
  "Mental Health Staff Rate",     "staff_per_1k_students",    "Staff per 1,000 Students"
)

# UI ----
ui <- 
  page_sidebar(
    theme = bs_theme(version = 5),
    title = "Explore - Youth Mental Health",
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
    
    # Main body (variable to explore): 
    selectInput(
      "measure", 
      "Select measure to explore:", 
      choices = measure_key$label),
    
    layout_columns(
      col_widths = c(6, 6),
      row_heights = c(2, 2),
      card(
        full_screen = TRUE,
        card_header(uiOutput("lollipop_title")), 
        plotlyOutput("lollipop")),
      card(
        full_screen = TRUE,
        card_header(uiOutput("severity_title")), 
        plotlyOutput("severity")),
      card(
        card_header("Change"), 
        plotlyOutput("line")),
      card(
        card_header("Locality"), 
        plotlyOutput("map"))
    )
  )

# Server ----
server <- function(input, output, session) {
  
  selected_measure <- reactive({
    req(input$measure)
    measure_key %>%
      filter(label == input$measure)
  })
  
  # Update highlight choices based on selected locality ----
  observeEvent(input$locality, {
    
    choices <- plt_dat %>%
      filter(locality_grouping == input$locality) %>%
      pull(label) %>%
      unique() %>%
      sort()
    
    updateSelectizeInput(
      session,
      "highlight",
      choices = choices,
      selected = NULL,
      server = TRUE
    )
  })
  
  # Identify the selected area's hierarchy ----
  selected_area <- reactive({
    
    req(input$highlight)
    
    if (input$locality == "school") {
      
      school_key %>%
        filter(school_name == input$highlight) %>%
        select(
          school_name,
          division_name,
          region_name
        ) %>%
        slice(1)
      
    } else if (input$locality == "division") {
      
      school_key %>%
        filter(division_name == input$highlight) %>%
        select(
          division_name,
          region_name
        ) %>%
        distinct() %>%
        slice(1)
      
    } else if (input$locality == "region") {
      
      tibble(
        region_name = input$highlight
      )
    }
  })
  
  # Lollipop ----
  ## Get plot data based on selection logic ---- 
  plot_data <- reactive({
    
    req(
      input$locality,
      input$measure,
      input$highlight
    )
    
    selected <- selected_area()
    
    # Start with selected measure and geography level
    dat <- plt_dat %>%
      filter(
        locality_grouping == input$locality,
        name == selected_measure()$name
      )
    
    # Region: all regions
    if (input$locality == "region") {
      
      dat
      
      # Division: only divisions in selected region
    } else if (input$locality == "division") {
      
      division_region <- school_key %>%
        select(
          division_name,
          region_name
        ) %>%
        distinct()
      
      dat %>%
        left_join(
          division_region,
          by = c("label" = "division_name")
        ) %>%
        filter(
          region_name == selected$region_name
        ) %>%
        select(
          -region_name
        )
      
      # School: only schools in selected division
    } else if (input$locality == "school") {
      
      school_division <- school_key %>%
        select(
          school_name,
          division_name
        ) %>%
        distinct()
      
      dat %>%
        left_join(
          school_division,
          by = c("label" = "school_name")
        ) %>%
        filter(
          division_name == selected$division_name
        ) %>%
        select(
          -division_name
        )
    }
  })
  
  ## Get highlighted data based on logic ----
  
  highlight_data <- reactive({
    
    plt_dat %>%
      filter(
        locality_grouping == input$locality,
        name == selected_measure()$name,
        label == input$highlight
      )
  })
  
  ## Get state average based on logic ----
  
  state_data <- reactive({
    
    plt_dat %>%
      filter(
        locality_grouping == "state",
        name == selected_measure()$name,
        label == "State Average"
      )
  })
  
  ## Generate plot ----
  
  # Plot title:
  plot_title <- reactive({
    
    req(
      input$measure,
      input$highlight,
      input$locality)
    
    selected <- selected_area()
    
    # Get internal measure name and display label
    measure_info <- measure_key %>%
      filter(label == input$measure)
    
    req(nrow(measure_info) == 1)
    
    measure_name <- measure_info$name
    measure_label <- measure_info$label
    
    # Determine year based on internal measure name
    year <- case_when(
      measure_name == "bully_rate_per_1k" ~ "2023-2024",
      measure_name %in% c(
        "avg_bully_prob",
        "avg_peer_rel",
        "avg_adult_rel",
        "pct_mental_health_training"
      ) ~ "2021-2023",
      measure_name %in% c(
        "pct_disadv",
        "staff_per_1k_students"
      ) ~ "2024-2025",
      TRUE ~ "Unknown year"
    )
    
    # Determine geography wording
    location <- case_when(
      input$locality == "school" ~ paste0(
        selected$division_name, " division"),
      input$locality == "division" ~ paste0(
        selected$region_name, " region"),
      input$locality == "region" ~ "state by region")
    
    paste0(
      measure_label,
      " in the ", location, ", ", year)
    
  })
  
  ### Plot output:
  output$lollipop <- renderPlotly({
    
    req(input$highlight)
    
    plot_dat <- plot_data() %>%
      filter(!is.na(value)) %>%
      mutate(label = forcats::fct_reorder(label, value))
    
    p <- ggplot(
      plot_dat,
      aes(
        x = value,
        y = label,
        text = paste0("<b>", label, "</b><br>", input$measure, ": ", round(value, 2)))) +
      geom_segment(
        aes(x = 0, xend = value, yend = label), color = "grey75") +
      geom_segment(data = highlight_data(),
        aes(x = 0, xend = value, yend = label), color = "#1B9E77FF", linewidth = 1) +
      geom_segment(data = state_data(),
        aes(x = 0, xend = value, yend = label), color = "#D95F02FF", linewidth = 1) +
      geom_point(size = 2) +
      geom_point(data = highlight_data(), color = "#1B9E77FF", size = 2) +
      geom_point(data = state_data(), color = "#D95F02FF", size = 2) +
      labs(
        title = plot_title(),
        x = input$measure,
        y = NULL) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = -.9)
      )
    
    ggplotly(
      p,
      tooltip = "text") %>%
      config(
        displayModeBar = FALSE,
        displaylogo = FALSE
      )
    
  })
  
  ## Card title ----
  output$lollipop_title <- renderUI({
    
    req(input$highlight, input$locality)
    selected <- selected_area()
    
    if (input$locality == "school") {
      paste0("Ranking ", input$highlight, " among other schools in the ", selected$division_name, " division")
      
    } else if (input$locality == "division") {
      paste0("Ranking ", input$highlight, " among other divisions in the ", selected$region_name, " region")
      
    } else if (input$locality == "region") {
      paste0("Ranking ", input$highlight, " among other regions")
      
    }
    
  }) 
  
  # Percentile ----
  
  ## Get data ----
  severity_data <- reactive({
    
    req(input$highlight)
    
    # All statewide data for this measure and locality type
    dat <- plt_dat %>%
      filter(
        locality_grouping == input$locality,
        name == selected_measure()$name
      ) %>%
      filter(!is.na(value)) %>%
      mutate(value = as.numeric(value))
    
    # Highlighted area's value
    highlight_value <- dat %>%
      filter(label == input$highlight) %>%
      pull(value)
    
    req(length(highlight_value) == 1)
    
    # Calculate statewide percentile
    percentile <- mean(dat$value <= as.numeric(highlight_value)) * 100
    
    # Measures where higher values are better
    good_measures <- c(
      "avg_peer_rel",
      "avg_adult_rel",
      "pct_mental_health_training",
      "staff_per_1k_students"
    )
    
    # Reverse so higher always means more severe
    if (selected_measure()$name %in% good_measures) {
      percentile <- 100 - percentile
    }
    
    tibble(
      value = highlight_value,
      percentile = percentile
    )
    
  })
  
  ## Generate plot ----
  output$severity <- renderPlotly({
  
    sev <- severity_data()
    percentile <- sev$percentile
    
    # Label of extremeness: 
    # severity_label <- case_when(
    #   percentile < 20 ~ "Very Low Severity",
    #   percentile < 40 ~ "Low Severity",
    #   percentile < 60 ~ "Moderate Severity",
    #   percentile < 80 ~ "High Severity",
    #   TRUE ~ "Very High Severity"
    # )
    
    # Helper function for dealing with ordinals in text (1st, 4th, etc.):
    ordinal <- function(x) {
      ifelse(
        x %% 100 %in% c(11, 12, 13),
        paste0(x, "th"),
        paste0(x, c("th","st","nd","rd","th","th","th","th","th","th")[x %% 10 + 1])
      )
    }
    
    p <- ggplot(
      tibble(x = c(0, 100), y = 1),
      aes(x = x, y = y)) +
      geom_segment(
        aes(x = 0, xend = 100, y = 1, yend = 1), linewidth = 1, color = "grey40") +
      geom_segment(
        data = tibble(x = seq(0, 100, 20)),
        aes(x = x, xend = x, y = .9, yend = 1.1), linewidth = .8, color = "grey40") +
      geom_point(
        aes(
          x = percentile,
          y = 1,
          text = paste0(
            "<b>", input$highlight, "</b>",
            "<br>",
            input$measure,
            ": ",
            round(highlight_data()$value, 2)
          )
        ),
        shape = 17,
        size = 5,
        color = "#1B9E77"
      ) +
      annotate(
        "text",
        x = percentile,
        y = 1.25,
        label = paste0(
          input$highlight,
          "<br>",
          input$measure, ": ", round(highlight_data()$value, 2), 
          "<br>",
          ordinal(round(percentile)),
          " percentile statewide"
        ),
        size = 4
      ) +
      annotate("text", x = 0, y = .65, label = "Very Low", hjust = 0, size = 4) +
      annotate("text", x = 100, y = .65, label = "Very High", hjust = 1,size = 4) +
      scale_x_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
      scale_y_continuous(limits = c(.5, 1.5)) +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_void()
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      config(
        displayModeBar = FALSE,
        displaylogo = FALSE
      )
    
  })
  
  ## Card title ----
  output$severity_title <- renderUI({
    
    req(input$highlight)
    
    sev <- severity_data()
    
    paste0(
      "Severity: ",
      round(sev$percentile),
      "% of statewide ",
      input$locality,
      "s scored less than or equal to ",
      input$highlight
    )
    
  })
  
}

shinyApp(ui, server)


