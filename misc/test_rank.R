library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

df <- readr::read_csv("prototype_data.csv")

ui <- 
  page_sidebar(
    theme = bs_theme(version = 5),
    title = "Prototype: Exploring Youth Mental Health in Virginia",
    
    sidebar = sidebar(
      title = "Select Measures",
      
      # School year:
      selectInput(
        "year",
        "School Year:",
        choices = sort(unique(df$school_year)),
        selected = unique(df$school_year)[1]
      ),
      
      # Region selection
      selectInput(
        "region", "Region:",
        choices = c("Compare All Regions", sort(unique(df$region_name))),
        selected = "Compare All Regions"
      ),
      
      # Conditional Division selection: only visible if a specific region is selected
      uiOutput("division_ui"),
      
      # Indicator
      varSelectInput(
        "var", "Indicator",
        df[, c("pct_disadv", "bully_rate_per_1k", "avg_bully_prob", 
               "avg_adult_relationships", "avg_peer_relationships", 
               "pct_mental_health_training", "staff_per_1k_students")]
      )
    ),
    
    plotOutput("lollipop_plot", height = "500px")
  )

server <- function(input, output, session) {
  
  # Render Division input dynamically
  output$division_ui <- renderUI({
    if (input$region != "Compare All Regions") {
      divisions <- df %>%
        filter(region_name == input$region) %>%
        pull(division_name) %>%
        unique() %>%
        sort()
      
      selectInput(
        "division", "Division:",
        choices = c("Compare All Divisions in the Region", divisions),
        selected = "Compare All Divisions in the Region"
      )
    }
  })
  
  filtered_data <- reactive({
    data <- df %>% filter(school_year == input$year)
    
    # Determine what level to plot based on selection
    if (input$region == "Compare All Regions") {
      data <- data %>% filter(locality_grouping == "region")
    } else if (!is.null(input$division) && input$division != "Compare All Divisions in the Region") {
      # Specific division → show schools
      data <- data %>% filter(locality_grouping == "school", division_name == input$division)
    } else {
      # Specific region → show divisions
      data <- data %>% filter(locality_grouping == "division", region_name == input$region)
    }})
  
  output$lollipop_plot <- renderPlot({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    # Determine grouping column
    group_col <- if (!is.null(input$division) &&
                     input$division != "Compare All Divisions in the Region") {
      "school_name"
    } else if (input$region != "Compare All Regions") {
      "division_name"
    } else {
      "region_name"
    }
    
    # Remove NAs and sort descending
    data <- data %>%
      filter(!is.na(.data[[group_col]]), !is.na(.data[[input$var]])) %>%
      arrange(desc(.data[[input$var]]))
    req(nrow(data) > 0)
    
    # State value for reference line
    state_value <- df %>%
      filter(school_year == input$year, locality_grouping == "state") %>%
      pull(.data[[input$var]])
    
    ggplot(data, aes(
      x = .data[[input$var]],
      y = reorder(.data[[group_col]], .data[[input$var]])
    )) +
      geom_segment(
        aes(
          x = 0,
          xend = .data[[input$var]],
          y = reorder(.data[[group_col]], .data[[input$var]]),
          yend = reorder(.data[[group_col]], .data[[input$var]])
        ),
        linewidth = 1.2,
        color = "#2C3E50"
      ) +
      geom_point(
        size = 5,
        color = "#E74C3C"
      ) +
      geom_vline(
        xintercept = state_value,
        color = "blue",
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = state_value,
        y = 0.5,
        label = paste("State:", round(state_value, 2)),
        color = "blue",
        angle = 90,
        vjust = -0.5
      ) +
      labs(
        x = input$var,
        y = tools::toTitleCase(gsub("_name", "", group_col)),
        title = paste("Lollipop Plot of", input$var),
        subtitle = paste("School Year:", input$year)
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)

