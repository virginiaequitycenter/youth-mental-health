# Create standalone app to dive deeper into specific variables for a particular region, division, or school

# This is equivalent to just the Explore tab on app1_all.R 

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

acs <- readRDS("acs.RDS")

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
    
    # Main body: 
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
        full_screen = TRUE,
        card_header(uiOutput("change_title")), 
        plotlyOutput("line")),
      card(
        full_screen = TRUE,
        card_header(uiOutput("map_title")), 
        leafletOutput("map"),
        div(
          style = "font-size: 0.75rem; font-style: italic; color: #666;",
          "School-level geography is not available."
        ))
    )
  )

# Server ----
server <- function(input, output, session) {
  
  # Data logic ----
  
  # Get measure of interest:
  selected_measure <- reactive({
    req(input$measure)
    measure_key %>%
      filter(label == input$measure)
  })
  
  # Display year for measure of interest:
  measure_year <- reactive({
    req(selected_measure())
    case_when(
      selected_measure()$name == "bully_rate_per_1k" ~ "2024",
      selected_measure()$name %in% c("avg_bully_prob", "avg_peer_rel", "avg_adult_rel", 
                                     "pct_mental_health_training") ~ "2022/2023",
      selected_measure()$name %in% c("pct_disadv", "staff_per_1k_students") ~ "2025",
      TRUE ~ "Unknown year")
  })
  
  # Update highlight choices based on selected locality: 
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
  
  # Identify the selected area's hierarchy: 
  selected_area <- reactive({
    
    req(input$highlight)
    
    if (input$locality == "school") {
      school_key %>%
        filter(school_name == input$highlight) %>%
        select(school_name, division_name, region_name) %>%
        slice(1)
      
    } else if (input$locality == "division") {
      school_key %>%
        filter(division_name == input$highlight) %>%
        select(division_name, region_name) %>%
        distinct() %>%
        slice(1)
      
    } else if (input$locality == "region") {
      tibble(region_name = input$highlight)
    }
  })
  
  # Lollipop ----
  ## Get plot data based on selection logic ---- 
  plot_data <- reactive({
    
    req(input$locality, input$measure,input$highlight)
    selected <- selected_area()
    
    # Start with selected measure and geography level
    dat <- plt_dat %>%
      filter(
        locality_grouping == input$locality,
        name == selected_measure()$name)
    
    # Region: all regions
    if (input$locality == "region") {
      dat
      
      # Division: only divisions in selected region
    } else if (input$locality == "division") {
      division_region <- school_key %>%
        select(division_name, region_name) %>%
        distinct()
      
      dat %>%
        left_join(division_region, by = c("label" = "division_name")) %>%
        filter(region_name == selected$region_name) %>%
        select(-region_name)
      
      # School: only schools in selected division
    } else if (input$locality == "school") {
      school_division <- school_key %>%
        select(school_name, division_name) %>%
        distinct()
      
      dat %>%
        left_join(school_division, by = c("label" = "school_name")) %>%
        filter(division_name == selected$division_name) %>%
        select(-division_name)
    }
  })
  
  ## Get highlighted data based on logic ----
  highlight_data <- reactive({
    
    plt_dat %>%
      filter(
        locality_grouping == input$locality,
        name == selected_measure()$name,
        label == input$highlight)
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
  
  ## Line plot ----
  # Plot title:
  line_title <- reactive({
    req(input$measure, input$highlight, input$locality)
    selected <- selected_area()
    
    # Get internal measure name and display label:
    measure_info <- measure_key %>%
      filter(label == input$measure)
    
    # Determine measure name, label, and year:
    req(nrow(measure_info) == 1)
    measure_name <- measure_info$name
    measure_label <- measure_info$label
    year <- measure_year()
    
    # Determine geography wording:
    location <- case_when(
      input$locality == "school" ~ paste0(
        selected$division_name, " division"),
      input$locality == "division" ~ paste0(
        selected$region_name, " region"),
      input$locality == "region" ~ "state by region")
    
    paste0(measure_label, " in the ", location, ", ", year)
  })
  
  ### Plot output:
  output$lollipop <- renderPlotly({
    req(input$highlight)
    
    plot_dat <- plot_data() %>%
      filter(!is.na(value)) %>%
      mutate(label = forcats::fct_reorder(label, value))
    
    p <- ggplot(plot_dat,
      aes(x = value, y = label,
        text = paste0("<b>", label, "</b>", 
                      "<br>", measure_year(),
                      "<br>", input$measure, ": ", round(value, 2), " ", selected_measure()$units))) +
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
        title = line_title(),
        x = selected_measure()$units,
        y = NULL) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = -.9))
    
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
    
    # All statewide data for this measure and locality type:
    dat <- plt_dat %>%
      filter(
        locality_grouping == input$locality,
        name == selected_measure()$name,
        !is.na(value)) %>%
      mutate(value = as.numeric(value))
    
    # Highlighted area's value:
    highlight_value <- dat %>%
      filter(label == input$highlight) %>%
      pull(value)
    
    # Calculate statewide percentile:
    req(length(highlight_value) == 1)
    percentile <- mean(dat$value <= as.numeric(highlight_value)) * 100
    
    # Reverse so higher always means more severe:
    if (selected_measure()$name %in% good_measures) {
      percentile <- 100 - percentile
    }
    
    tibble(value = highlight_value,percentile = percentile)
    
  })
  
  ## Scale plot ----
  output$severity <- renderPlotly({
  
    sev <- severity_data()
    percentile <- sev$percentile
    
    # Label of extremeness: 
    # severity_label <- case_when(
    #   percentile < 20 ~ "Very Low Severity",
    #   percentile < 40 ~ "Low Severity",
    #   percentile < 60 ~ "Moderate Severity",
    #   percentile < 80 ~ "High Severity",
    #   TRUE ~ "Very High Severity")
    
    # Helper function for dealing with ordinals in text (1st, 4th, etc.):
    ordinal <- function(x) {
      ifelse(
        x %% 100 %in% c(11, 12, 13),
        paste0(x, "th"),
        paste0(x, c("th","st","nd","rd","th","th","th","th","th","th")[x %% 10 + 1]))
    }
    
    p <- ggplot(tibble(x = c(0, 100), y = 1),
      aes(x = x, y = y)) +
      geom_segment(
        aes(x = 0, xend = 100, y = 1, yend = 1), linewidth = 1, color = "grey40") +
      geom_segment(
        data = tibble(x = seq(0, 100, 20)),
        aes(x = x, xend = x, y = .9, yend = 1.1), linewidth = .8, color = "grey40") +
      geom_point(
        aes(x = percentile, y = 1,
            text = paste0("<b>", input$highlight, "</b>", 
                          "<br>", measure_year(), 
                          "<br>", input$measure,": ", round(highlight_data()$value, 2), " ", selected_measure()$units,
                          "<br>", "State Average: ", round(state_data()$value, 2), " ", selected_measure()$units, 
                          "<br>", ordinal(round(percentile)), " percentile statewide")), 
        shape = 17, size = 5,color = "#1B9E77") +
      annotate("text", x = percentile, y = 1.3,
        label = paste0(input$highlight, 
                       "<br>", measure_year(),
                       "<br>", input$measure, ": ", round(highlight_data()$value, 2), " ", selected_measure()$units,
                       "<br>", ordinal(round(percentile)), " percentile statewide"),
        size = 3.5) +
      annotate("text", x = 0, y = .65, label = "Low", hjust = 0, size = 4) +
      annotate("text", x = 100, y = .65, label = "High", hjust = 1,size = 4) +
      scale_x_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
      scale_y_continuous(limits = c(.5, 1.5)) +
      labs(x = NULL, y = NULL) +
      theme_void()
    
    ggplotly(
      p,
      tooltip = "text") %>%
      config(
        displayModeBar = FALSE,
        displaylogo = FALSE)
    
  })
  
  ## Card title ----
  output$severity_title <- renderUI({
    
    req(input$highlight)
    sev <- severity_data()
    
    paste0(
      "Severity: ", round(sev$percentile), "% of statewide ", input$locality,
      "s scored less than or equal to ", input$highlight)
  })
  
  # Change ----
  ## Get data ----
  state_change <- reactive({
    change_dat %>%
      filter(
        locality_grouping == "state",
        label == "State Average",
        name == selected_measure()$name,
        !(name == "bully_rate_per_1k" & school_year == 2025))
  })
  
  has_trend_data <- reactive({
    selected_measure()$name %in% c("pct_disadv", "bully_rate_per_1k", "staff_per_1k_students")
  })
  
  change_plot_data <- reactive({
    
    req(input$highlight)
    selected <- selected_area()
    
    dat <- change_dat %>%
      filter(
        locality_grouping == input$locality,
        name == selected_measure()$name,
        !(name == "bully_rate_per_1k" & school_year == 2025))
    
    if (input$locality == "region") {
      dat
      
    } else if (input$locality == "division") {
      division_region <- school_key %>%
        select(division_name, region_name) %>%
        distinct()
      
      dat %>%
        left_join(division_region,by = c("label" = "division_name")) %>%
        filter(region_name == selected$region_name) %>%
        select(-region_name)
      
    } else {
      school_division <- school_key %>%
        select(school_name, division_name) %>%
        distinct()
      
      dat %>%
        left_join(school_division,by = c("label" = "school_name")) %>%
        filter(division_name == selected$division_name) %>%
        select(-division_name)
    }
  })
  
  ## Line plot ----
  # Reactive title:
  change_title <- reactive({
    
    req(input$measure, input$highlight, input$locality)
    selected <- selected_area()
    
    # Get internal measure name and display label:
    measure_info <- measure_key %>%
      filter(label == input$measure)
    
    req(nrow(measure_info) == 1)
    measure_name <- measure_info$name
    measure_label <- measure_info$label
    
    # Determine geography wording
    location <- case_when(
      input$locality == "school" ~ paste0(
        selected$division_name, " division"),
      input$locality == "division" ~ paste0(
        selected$region_name, " region"),
      input$locality == "region" ~ "state by region")
    
    paste0(measure_label, " in the ", location)
  })
  
  output$line <- renderPlotly({
    
    validate(
      need(
        has_trend_data(),
        paste(
          "Change data is not available for this measure. Change data is available for Bullying Rates, Mental Health Staff Rates, and Economically Disadvantaged (%)."
        )
      )
    )
    
    highlight_line <- change_plot_data() %>%
      filter(label == input$highlight)
    
    p <- ggplot() +
      geom_line(data = change_plot_data(),
        aes(x = school_year, y = value, group = label), color = "grey80", linewidth = 0.5) +
      geom_point(data = change_plot_data(),
        aes(x = school_year, y = value,
            text = paste0("<b>", label, "</b><br>",school_year, "<br>",
                          input$measure, ": ", round(value, 2), " ", selected_measure()$units)), 
        color = "grey80", size = 1) +
      geom_line(data = state_change(),
        aes(x = school_year,y = value), color = "#D95F02", linewidth = 1) +
      geom_point(data = state_change(),
        aes(x = school_year, y = value,
            text = paste0("<b>State Average</b><br>", school_year, "<br>",
                          input$measure, ": ", round(value, 2), " ", selected_measure()$units)), 
        color = "#D95F02", size = 2) +
      geom_line(data = highlight_line,
        aes(x = school_year,y = value), color = "#1B9E77", linewidth = 1) +
      geom_point(data = highlight_line,
        aes(x = school_year, y = value,
            text = paste0("<b>", label, "</b><br>", school_year, "<br>",
                          input$measure, ": ", round(value, 2), " ", selected_measure()$units)), 
        color = "#1B9E77", size = 2) +
      scale_x_continuous(breaks = sort(unique(change_plot_data()$school_year))) +
      labs(
        title = change_title(),
        x = "School Year",
        y = selected_measure()$units) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 11, hjust = -.9))
    
    ggplotly(
      p,
      tooltip = "text") %>%
      config(
        displayModeBar = FALSE,
        displaylogo = FALSE)
    
  })
  
  ## Card title ----
  output$change_title <- renderUI({
    
    req(input$highlight, input$measure)
    paste0("Change in ", input$measure, " for ", input$highlight)
    
  })

  # Map ----
  ## Prepare geography data ----
  acs_map <- acs %>% 
    mutate(label = case_when(
      locality_grouping == "division" ~ division_name, 
      locality_grouping == "region" ~ region_name,
      locality_grouping == "state" ~ "State Average")) %>% 
    filter(!is.na(label)) %>%
    select(locality_grouping, label, geometry)
  
  # Get geography grouping logic: 
  map_locality <- reactive({ 
    
    if (input$locality == "region") { 
      "region" 
    } else { 
      "division" 
      } 
    })
  
  # Get highlight logic:
  map_highlight <- reactive({ 
    
    req(input$highlight) 
    selected <- selected_area() 
    
    if (input$locality == "school") { 
      selected$division_name 
      
    } else if (input$locality == "division") { 
      input$highlight 
    
    } else { 
      input$highlight 
      } 
    })
  
  # Get map data:
  map_data <- reactive({ 
    
    req(input$measure) 
    map_level <- map_locality() 
    
    values <- plt_dat %>% 
      filter(locality_grouping == map_level, 
             name == selected_measure()$name) %>% 
      select(locality_grouping, label, value) 

    acs_map %>% 
      filter(locality_grouping == map_level) %>% 
      left_join(values, by = c("locality_grouping", "label")) 
    })
  
  # Get map title:
  output$map_title <- renderUI({ 
    
    req(input$measure, input$locality) 
    
    if (input$locality == "school") { 
      paste0(input$measure, " by school division" ) 
    
    } else if (input$locality == "division") { 
      paste0(input$measure, " by school division" ) 
      
    } else { paste0(input$measure, " by region" ) 
      
    } 
    })
  
  ## Generate map ----

  output$map <- renderLeaflet({ 
    req(input$measure, input$highlight) 
    dat <- map_data() 
    req(nrow(dat) > 0) 
    selected_name <- map_highlight() 
    
    # Reverse palette for measures where higher is better 
    palette_colors <- if (
      selected_measure()$name %in% good_measures 
      ) { 
        rev(RColorBrewer::brewer.pal(9, "YlOrRd")) 
      } else { 
        RColorBrewer::brewer.pal(9, "YlOrRd") } 
    
    pal <- colorNumeric(palette = palette_colors, domain = dat$value, na.color = "#E5E5E5")
    
    dat <- dat %>% 
      mutate(selected = label == selected_name, 
             tooltip = paste0("<b>", label, "</b>",
                              "<br>", measure_year(),
                              "<br>", input$measure, ": ", ifelse(is.na(value), "Data not available", round(value, 2)), " ", selected_measure()$units, 
                              "<br>", "State Average: ", round(state_data()$value, 2), " ", selected_measure()$units)) 
    
    leaflet(dat) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(fillColor = ~pal(value), fillOpacity = 0.75, 
                  color = ~ifelse(selected, "#1B9E77", "white"), 
                  weight = ~ifelse(selected, 3, 1), 
                  opacity = 1, 
                  label = ~lapply(tooltip, htmltools::HTML), 
                  highlightOptions = highlightOptions(weight = 2, color = "#333333", 
                                                      fillOpacity = 0.85, bringToFront = TRUE )) %>% 
      addLegend(position = "bottomright", pal = pal, values = ~value, 
                title = selected_measure()$label, opacity = 0.8 ) 
    })
  
}

shinyApp(ui, server)

# Deployment note: 
# The leaflet package depends on the raster and terra package, and the most recent versions
# of these packages (as of 8/14/26) are not comparable with the shinyapps.io GDAL. A workaround
# per https://forum.posit.co/t/deployment-fails-on-shinyapps-io-because-of-terra/214331/19 is to
# force an older version of the terra package, using:
# 
# download.file(
#   "https://packagemanager.posit.co/cran/2026-01-15/bin/macosx/big-sur-arm64/contrib/4.3/terra_1.8-93.tgz",
#   destfile = "terra_1.8-93.tgz"
# )
# install.packages("terra_1.8-93.tgz", repos = NULL, type = "binary")
