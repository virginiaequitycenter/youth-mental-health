# Create standalone app to compare specific measures for a particular region, division, or school

# This is equivalent to just the Compare tab on app1_all.R 

# Setup ----
library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(forcats)
library(ggplot2)

# Read ----
# For school levels
school_key <- read_csv("school_key.csv")

school_levels <-  school_key %>%
  select(grade_standard, sch_id)

prototype_data <- read_csv("prototype_data.csv") %>%
  left_join(school_levels, by = "sch_id") %>%
  filter(locality_grouping != "school" | grade_standard %in% c("High", "Middle"))

# Dataprep ----
## Reduce to most recent values ----
# TODO: eventually move to dataprep script

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

# Combine all climate data 
climate_recent <- bind_rows(state_avg, reg_avg, climate_div_avg, climate_recent_sch) %>%
  mutate(label = case_when(
    locality_grouping == "school" ~ school_name,
    locality_grouping == "division" ~ division_name,
    locality_grouping == "region" ~ region_name,
    TRUE ~ "State Average"))

# Staffing and disadvantage 24/25
staff_disadv_recent <- prototype_data %>%
  filter(school_year == "2024-2025") %>%
  select(school_year:pct_disadv, total_positions:staff_per_1k_students) %>%
  mutate(label = case_when(
    locality_grouping == "state" ~ "State Average",
    locality_grouping == "region" ~ region_name, 
    locality_grouping == "division" ~ division_name,
    TRUE ~ school_name))

# Pivot everything longer  
staff_disad_long <- staff_disadv_recent %>%
  select(label, locality_grouping, pct_disadv, staff_per_1k_students) %>%
  pivot_longer(cols = c(pct_disadv, staff_per_1k_students))

climate_long <- climate_recent %>%
  select(label, locality_grouping, pct_mental_health_training:avg_bully_prob) %>%
  pivot_longer(cols = c(pct_mental_health_training:avg_bully_prob))

bully_long <- bully_recent %>%
  select(label, locality_grouping, bully_rate_per_1k) %>%
  pivot_longer(cols = bully_rate_per_1k)

# And combine:
plt_dat <- rbind(staff_disad_long, climate_long, bully_long) 

## Get change data ----
# # Measures that have multiple years: bully_rate_per_1k, pct_disadv, staff_per_1k_students
change_raw <- prototype_data %>%
  select(school_year, locality_grouping, region_name, division_name, school_name, pct_disadv, bully_rate_per_1k, staff_per_1k_students) %>%
  mutate(
    school_year = as.numeric(str_sub(school_year, 6, 9)),
    label = case_when(
      locality_grouping == "state" ~ "State Average",
      locality_grouping == "region" ~ region_name,
      locality_grouping == "division" ~ division_name,
      TRUE ~ school_name),
    # If bullying incidents are not available, we assume there were no reports made
    # (this is different from staffing and disadv)
    bully_rate_per_1k = case_when(
      is.na(bully_rate_per_1k) ~ 0,
      TRUE ~ bully_rate_per_1k))

change_dat <- change_raw %>%
  select(label, locality_grouping, pct_disadv:staff_per_1k_students, school_year) %>%
  pivot_longer(cols = c(pct_disadv:staff_per_1k_students))

## Standardize on measure names ----
measure_key <- tibble::tribble(
  ~label,                         ~name,                     ~units,
  "Bullying Rate",                "bully_rate_per_1k",        "Incidents per 1,000 Students",
  "Avg. Bullying Problem",        "avg_bully_prob",           "Average Rating",
  "Relationships with Peers",     "avg_peer_rel",             "Average Rating",
  "Relationships with Adults",    "avg_adult_rel",            "Average Rating",
  "Mental Health Training (%)",   "pct_mental_health_training",          "Percent of Students",
  "Economically Disadv. (%)",     "pct_disadv",               "Percent of Students",
  "Mental Health Staff Rate",     "staff_per_1k_students",    "Staff per 1,000 Students")

# Measures where higher values are better:
good_measures <- c(
  "avg_peer_rel",
  "avg_adult_rel",
  "pct_mental_health_training",
  "staff_per_1k_students")

# UI ----
ui <- 
  page_sidebar(
    theme = bs_theme(version = 5),
    title = "Compare - Youth Mental Health",
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
    
    # Main body: 
    card(
      card_title("Compare how two indicators intersect"),
      layout_columns(
        selectInput(
          "measure1", 
          "Select value for x-axis:", 
          choices = measure_key$label,
          selected = "Economically Disadv. (%)"
        ),
        selectInput(
          "measure2", 
          "Select value for y-axis:", 
          choices = measure_key$label,
          selected = "Bullying Rate"
        )
      ),
      plotlyOutput("compare_plt")
    )
  )

# Server ----
server <- function(input, output, session) {
  
  # Reactive UI ----
  # Update highlight choices based on selected locality in UI: 
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

  # Scatterplot ----
  
  ## Plot data ----
  
  # Get the measure metadata for the two selected measures:
  x_measure <- reactive({
    req(input$measure1)
    measure_key %>%
      filter(label == input$measure1)
  })
  
  y_measure <- reactive({
    req(input$measure2)
    measure_key %>%
      filter(label == input$measure2)
  })
  
  # Scatter data, tooltips, etc:
  scatter_data <- reactive({
    req(input$locality, input$highlight)
    req(x_measure(), y_measure())
  
    x_name <- x_measure()$name
    y_name <- y_measure()$name
    
    x_units <- x_measure()$units
    y_units <- y_measure()$units
    
    x_dat <- plt_dat %>%
      filter(
        locality_grouping == input$locality,
        name == x_name) %>%
      select(label, x = value)
    
    y_dat <- plt_dat %>%
      filter(
        locality_grouping == input$locality,
        name == y_name) %>%
      select(label,y = value)
    
    comparison <- x_dat %>%
      inner_join(y_dat, by = "label") %>%
      mutate(
        point_type = case_when(
          label == input$highlight ~ "Selected",
          TRUE ~ "Other"),
        tooltip = paste0("<b>", label, "</b>",
                         "<br>", input$measure1," (", x_year(), "): ", round(x, 2), " ", x_measure()$units,
                         "<br>", input$measure2," (", y_year(), "): ", round(y, 2), " ", y_measure()$units))
    
    state_x <- plt_dat %>%
      filter(
        locality_grouping == "state",
        name == x_name) %>%
      pull(value)
    
    state_y <- plt_dat %>%
      filter(
        locality_grouping == "state",
        name == y_name) %>%
      pull(value)
    
    state <- tibble(
      label = "State Average",
      x = state_x[1],
      y = state_y[1],
      point_type = "State Average",
      tooltip = paste0("<b>State Average</b>",
                       "<br>", input$measure1, " (", x_year(), "): ", round(state_x[1], 2), " ", x_measure()$units,
                       "<br>", input$measure2, " (", y_year(), "): ", round(state_y[1], 2), " ", y_measure()$units))
    
    bind_rows(comparison, state)
  })
  
  ## Reactive plot title ----
  plot_title <- reactive({
    locality_text <- case_when(
      input$locality == "school"   ~ "schools",
      input$locality == "division" ~ "divisions",
      input$locality == "region"   ~ "regions")
    
    paste(input$measure1, "compared to", input$measure2, "for all", locality_text, "in Virginia")
  })
  
  ## Reactive years ----
  x_year <- reactive({
    req(x_measure())
    case_when(
      x_measure()$name == "bully_rate_per_1k" ~ "2024",
      x_measure()$name %in% c("avg_bully_prob", "avg_peer_rel", "avg_adult_rel", 
                              "pct_mental_health_training") ~ "2022/2023",
      x_measure()$name %in% c("pct_disadv","staff_per_1k_students") ~ "2025",
      TRUE ~ "Unknown year")
  })
  
  y_year <- reactive({
    req(y_measure())
    case_when(
      y_measure()$name == "bully_rate_per_1k" ~ "2024",
      y_measure()$name %in% c("avg_bully_prob", "avg_peer_rel", "avg_adult_rel", 
                              "pct_mental_health_training") ~ "2022/2023",
      y_measure()$name %in% c("pct_disadv","staff_per_1k_students") ~ "2025",
      TRUE ~ "Unknown year")
  })
  
  ## Render scatterplot ----
  output$compare_plt <- renderPlotly({
    d <- scatter_data()
    req(nrow(d) > 0)
    other <- d %>% filter(point_type == "Other")
    selected <- d %>% filter(point_type == "Selected")
    state <- d %>% filter(point_type == "State Average")
    selected_label <- as.character(selected$label[[1]])
    
    p <- ggplot() +
      geom_point(data = other, aes(x, y, text = tooltip), 
                 color = "black", alpha = 0.2) +
      geom_point(data = selected, aes(x, y, text = tooltip, color = "Selected"),
                 size = 4, shape = 15) +
      geom_point(data = state, aes(x, y, text = tooltip, color = "State Average"),
                 size = 4, shape = 17) +
      # geom_text_repel(data = selected, aes(x, y, label = paste0("<b>", selected$label, "</b>")),
      #   color = "#1B9E77FF") +
      # geom_text_repel(data = state, aes(x, y, label = paste0("<b>State Average</b>")),
      #   color = "#D55E00") +
      scale_color_manual(name = NULL, 
                         values = c("Selected" = "#1B9E77FF", "State Average" = "#D95F02FF")) +
      labs(
        title = plot_title(),
        x = paste0(input$measure1, " (", x_measure()$units, ")"),
        y = paste0(input$measure2, " (", y_measure()$units, ")")) +
      theme_minimal(base_size = 12) 
    
    # Convert to Plotly and deal with legend:
    plt <- ggplotly(p, tooltip = "text")
    plt$x$data[[2]]$name <- selected_label
    plt$x$data[[3]]$name <- "State Average"
    
    plt %>%
      config(
        displayModeBar = FALSE,
        displaylogo = FALSE
      )
  })
}

shinyApp(ui, server)
