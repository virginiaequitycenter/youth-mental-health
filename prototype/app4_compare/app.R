# Create standalone app to compare specific measures for a particular region, division, or school

# Setup ----
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

# Read ----
plt_dat <- readr::read_csv("plt_dat.csv")
measure_key <- readr::read_csv("measure_key.csv")

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
      filter(label == input$measure1) %>%
      slice(1)
  })
  
  y_measure <- reactive({
    req(input$measure2)
    measure_key %>%
      filter(label == input$measure2) %>%
      slice(1)
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
                         "<br>", input$measure1, " (", x_measure()$year, "): ", round(x, 2), " ", x_measure()$units,
                         "<br>", input$measure2," (", y_measure()$year, "): ", round(y, 2), " ", y_measure()$units))
    
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
                       "<br>", input$measure1, " (", x_measure()$year, "): ", round(state_x[1], 2), " ", x_measure()$units,
                       "<br>", input$measure2, " (", y_measure()$year, "): ", round(state_y[1], 2), " ", y_measure()$units))
    
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
      scale_color_manual(name = NULL, 
                         values = c("Selected" = "#1B9E77FF", "State Average" = "#D95F02FF")) +
      labs(
        title = plot_title(),
        x = paste0(x_measure()$label, " (", x_measure()$units, ", ", x_measure()$year, ")"),
        y = paste0(y_measure()$label, " (", y_measure()$units, ", ", y_measure()$year,  ")")) +
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
