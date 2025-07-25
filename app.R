# app.R

# ---- 1. Load libraries and data ----
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)
library(shinythemes)
library(leaflet)

app_data <- readRDS("data/app_data.rds")
services_data <- read.csv("data/services.csv") # NEW: Load the services data

# NEW: Define our color palette
# Inspired by the BCCSU dashboard colors
color_palette <- list(
  primary_blue = "#007bff",
  highlight_red = "#dc3545",
  highlight_orange = "#fd7e14",
  highlight_purple = "#6f42c1",
  text_dark = "#343a40",
  background_light = "#f8f9fa"
)

# ---- 2. Define User Interface (UI) ----

# UPDATED: We'll prepare choices for ward instead of neighbourhood
ward_choices <- c("All Wards", sort(unique(na.omit(app_data$ward))))
gender_choices <- c("All Genders", "Male", "Female")

# NEW: We are replacing fluidPage with navbarPage to create a multi-tab layout
ui <- navbarPage(
  
  # Title for the entire application
  title = "Winnipeg Substance Use Dashboard",
  # This one line of code changes the whole look and feel!
  theme = shinytheme("cerulean"),
  
  # --- TAB 1: INCIDENT OVERVIEW ---
  tabPanel(
    title = "Incident Overview",
    
    # MOVED: Our entire sidebarLayout is now inside the first tabPanel
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        dateRangeInput(
          inputId = "date_range",
          label = "Filter by Incident Date:",
          start = min(app_data$dispatch_date, na.rm = TRUE),
          end = max(app_data$dispatch_date, na.rm = TRUE)
        ),
        # UPDATED: Replaced neighbourhood with ward
        selectInput(
          inputId = "ward_filter", # <-- Changed ID
          label = "Filter by Ward:", # <-- Changed label
          choices = ward_choices # <-- Using new choices
        ),
        radioButtons(
          inputId = "gender_filter",
          label = "Filter by Gender:",
          choices = c("All Genders", "Male", "Female")
        )
      ),
      # UPDATED: Main panel layout changes
      mainPanel(
        fluidRow(
          # Using our new background color
          column(width = 6, div(class = "value-box", style = paste0("background-color: ", color_palette$background_light, ";"), 
                                h4("Total Incidents"), uiOutput("total_incidents"))),
          column(width = 6, div(class = "value-box", style = paste0("background-color: ", color_palette$background_light, ";"), 
                                h4("Total Naloxone Doses"), uiOutput("total_naloxone")))
        ),
        
        # We removed the <hr/> lines for a tighter layout
        fluidRow(column(width = 12, h4("Incidents Over Time"), plotlyOutput("time_trend_plot"))),
        
        fluidRow(
          column(width = 6, h4("Incidents by Day of the Week"), plotlyOutput("weekday_plot")),
          column(width = 6, h4("Top Substances Involved"), plotlyOutput("substance_plot"))
        ),
        
        fluidRow(column(width = 12, h4("Demographic Breakdown by Age and Gender"), plotlyOutput("pyramid_plot")))
      )
    )
  ),
  
  
  # --- TAB 2: SERVICES MAP ---
  tabPanel(
    title = "Services Map",
    # This creates a map that fills the entire page
    leafletOutput("services_map", height = "80vh") 
  ),
  
  # --- TAB 3: ABOUT THE PROJECT ---
  tabPanel(
    title = "About",
    h2("About This Project"),
    p("This dashboard was created as a portfolio project to demonstrate skills in R, Shiny, data visualization, and data wrangling."),
    p("Data is sourced from the Winnipeg Open Data portal.")
  )
)

# ---- 3. Define Server Logic ----
server <- function(input, output) {
  
  # --- Reactive Data ---
  filtered_data <- reactive({
    data <- app_data
    # UPDATED: Filter by ward now
    if (input$ward_filter != "All Wards") { # <-- Changed input ID
      data <- data |>
        filter(ward == input$ward_filter) # <-- Changed column name
    }
    # 3. Filter by gender, if a specific one is selected
    if (input$gender_filter != "All Genders") {
      data <- data |>
        filter(gender == input$gender_filter) # <-- THIS IS THE CORRECTED LINE
    }
    data |>
      filter(
        dispatch_date >= input$date_range[1],
        dispatch_date <= input$date_range[2]
      )
  })
  
  # --- Value Boxes ---
  output$total_incidents <- renderUI({
    total <- n_distinct(filtered_data()$incident_number)
    tags$p(prettyNum(total, big.mark = ",")) 
  })
  
  output$total_naloxone <- renderUI({
    total <- sum(filtered_data()$naloxone_administrations, na.rm = TRUE)
    tags$p(prettyNum(total, big.mark = ","))
  })
  
  # --- Time Trend Plot ---
  output$time_trend_plot <- renderPlotly({
    trend_data <- filtered_data() |>
      filter(!is.na(dispatch_date)) |>
      mutate(year_month = floor_date(dispatch_date, "month")) |>
      count(year_month) |>
      mutate(tooltip_text = paste("Month:", format(year_month, "%b %Y"), "<br>Incidents:", n))
    
    plot_ly(data = trend_data, x = ~year_month, y = ~n, type = 'scatter', mode = 'lines', line = list(color = color_palette$primary_blue), hoverinfo = 'text', text = ~tooltip_text) |>
      layout(xaxis = list(title = "Month"), yaxis = list(title = "Number of Incidents"))
  })
  
  # --- Weekday Plot ---
  output$weekday_plot <- renderPlotly({
    plot_data <- filtered_data() |>
      mutate(weekday = wday(dispatch_date, label = TRUE, week_start = 1)) |>
      filter(!is.na(weekday)) |>
      count(weekday)
    
    p <- ggplot(plot_data, aes(x = weekday, y = n)) +
      geom_col(fill = color_palette$primary_blue) +
      labs(title = NULL, x = "Day of the Week", y = "Total Number of Incidents") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) 
    ggplotly(p)
  })
  
  # --- Substance Plot ---
  output$substance_plot <- renderPlotly({
    substance_counts <- filtered_data() |>
      filter(!is.na(substances_involved)) |>
      separate_rows(substances_involved, sep = "; ") |>
      count(substances_involved, sort = TRUE, name = "count") |>
      top_n(10, count)
    
    p <- ggplot(substance_counts, aes(x = count, y = reorder(substances_involved, count), text = paste("Count:", count))) +
      geom_col(fill = color_palette$highlight_red) +
      labs(x = "Total Count", y = NULL) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  # --- Pyramid Plot ---
  output$pyramid_plot <- renderPlotly({
    pyramid_data <- filtered_data() |>
      filter(gender %in% c("Male", "Female"), !is.na(age)) |>
      count(age, gender, name = "count") |>
      mutate(count = if_else(gender == "Male", -count, count))
    
    p <- ggplot(pyramid_data, aes(x = count, y = age, fill = gender, text = paste("Count:", abs(count)))) +
      geom_col() +
      scale_fill_manual(values = c("Male" = color_palette$primary_blue, "Female" = color_palette$highlight_orange)) +
      labs(x = "Number of Incidents", y = "Age Group") +
      scale_x_continuous(labels = function(x) abs(x), breaks = pretty(pyramid_data$count)) +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(p, tooltip = "text") |>
      layout(hovermode = "y unified")
  })
  
  # --- Services Map (NOW SEPARATE) ---
  output$services_map <- renderLeaflet({
    leaflet(data = services_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMarkers(
        lng = ~lng, 
        lat = ~lat,
        popup = ~paste("<b>", name, "</b>", "<br>", description)
      )
  })
  
}


# ---- 4. Run the Application ----
shinyApp(ui = ui, server = server)
