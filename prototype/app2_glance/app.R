# Create standalone app to show all youth mental health variables at a glance with a particular region highlighted 
# This is equivalent to just the At-A-Glance tab on app1_all.R 

# Setup ----
library(shiny)
library(bslib)
library(ggbeeswarm)
library(plotly)
library(tidyverse)

# Read ----
plt_dat <- readr::read_csv("plt_dat.csv")
measure_key <- readr::read_csv("measure_key.csv")

# Add measure metadata
plt_dat <- plt_dat %>%
  left_join(measure_key %>% select(name, units, year),by = "name")

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
    
    paste("See all youth mental health indicators for ", highlight)
  })
  
}

shinyApp(ui, server)
