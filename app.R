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


# ---- 2. Define User Interface (UI) ----

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
        selectInput(
          inputId = "neighbourhood_filter",
          label = "Filter by Neighbourhood:",
          choices = c("All Neighbourhoods", sort(unique(na.omit(app_data$neighbourhood))))
        ),
        radioButtons(
          inputId = "gender_filter",
          label = "Filter by Gender:",
          choices = c("All Genders", "Male", "Female")
        )
      ),
      mainPanel(
        # The main panel content is the same as before
        fluidRow(
          column(width = 6, div(class = "value-box", h3("Total Incidents"), uiOutput("total_incidents"))),
          column(width = 6, div(class = "value-box", h3("Total Naloxone Doses"), uiOutput("total_naloxone")))
        ),
        hr(),
        fluidRow(column(width = 12, h3("Incidents Over Time"), plotlyOutput("time_trend_plot"))),
        hr(),
        fluidRow(
          column(width = 6, h3("Incidents by Day of the Week"), plotlyOutput("weekday_plot")),
          column(width = 6, h3("Top Substances Involved"), plotlyOutput("substance_plot"))
        ),
        hr(),
        fluidRow(column(width = 12, h3("Demographic Breakdown by Age and Gender"), plotlyOutput("pyramid_plot")))
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
    if (input$neighbourhood_filter != "All Neighbourhoods") {
      data <- data |> filter(neighbourhood == input$neighbourhood_filter)
    }
    if (input$gender_filter != "All Genders") {
      data <- data |> filter(gender == input_gender_filter)
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
    
    plot_ly(data = trend_data, x = ~year_month, y = ~n, type = 'scatter', mode = 'lines', line = list(color = '#007bff'), hoverinfo = 'text', text = ~tooltip_text) |>
      layout(xaxis = list(title = "Month"), yaxis = list(title = "Number of Incidents"))
  })
  
  # --- Weekday Plot ---
  output$weekday_plot <- renderPlotly({
    plot_data <- filtered_data() |>
      mutate(weekday = wday(dispatch_date, label = TRUE, week_start = 1)) |>
      filter(!is.na(weekday)) |>
      count(weekday)
    
    p <- ggplot(plot_data, aes(x = weekday, y = n)) +
      geom_col(fill = "#007bff") +
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
      geom_col(fill = "#007bff") +
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
      scale_fill_manual(values = c("Male" = "#007bff", "Female" = "#E13468")) +
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
# Note: The services map tab is currently a placeholder. We will add the map functionality in the next steps.