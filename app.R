# app.R

# ---- 1. Load libraries and data ----
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)

app_data <- readRDS("data/app_data.rds")


# ---- 2. Define User Interface (UI) ----
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .value-box {
        background-color: #f2f2f2;
        padding: 20px;
        border-radius: 5px;
        text-align: center;
      }
      .value-box h3 {
        margin-top: 0;
        color: #007bff;
      }
      .value-box p {
        font-size: 24px;
        font-weight: bold;
        margin-bottom: 0;
      }
    "))
  ),
  
  titlePanel("Winnipeg Substance Use Incident Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Filters"),
      dateRangeInput(
        inputId = "date_range",
        label = "Filter by Incident Date:",
        start = min(app_data$dispatch_date, na.rm = TRUE),
        end = max(app_data$dispatch_date, na.rm = TRUE),
        min = min(app_data$dispatch_date, na.rm = TRUE),
        max = max(app_data$dispatch_date, na.rm = TRUE)
      )
    ),
    
    mainPanel(
      fluidRow(
        column(width = 6, div(class = "value-box", h3("Total Incidents"), uiOutput("total_incidents"))),
        column(width = 6, div(class = "value-box", h3("Total Naloxone Doses"), uiOutput("total_naloxone")))
      ),
      hr(),
      fluidRow(
        column(width = 12,
               h3("Incidents Over Time"),
               plotOutput("time_trend_plot")
        )
      ),
      hr(),
      fluidRow(
        column(width = 12,
               h3("Incidents by Day of the Week"),
               plotOutput("weekday_plot")
        )
      )
    )
  ) # <-- THIS IS THE CORRECTED CLOSING PARENTHESIS FOR sidebarLayout
)

# ---- 3. Define Server Logic ----
server <- function(input, output) {
  
  # Create a reactive expression for filtered data
  filtered_data <- reactive({
    app_data |>
      filter(
        dispatch_date >= input$date_range[1],
        dispatch_date <= input$date_range[2]
      )
  })
  
  # --- Render Value Boxes ---
  output$total_incidents <- renderUI({
    total <- n_distinct(filtered_data()$incident_number)
    tags$p(prettyNum(total, big.mark = ",")) 
  })
  
  output$total_naloxone <- renderUI({
    total <- sum(filtered_data()$naloxone_administrations, na.rm = TRUE)
    tags$p(prettyNum(total, big.mark = ","))
  })
  
  # --- Render Plots (SEPARATELY) ---
  
  # This is the render function for the time trend plot
  output$time_trend_plot <- renderPlot({
    trend_data <- filtered_data() |>
      # Remove rows where dispatch_date is NA before processing
      filter(!is.na(dispatch_date)) |> 
      mutate(year_month = floor_date(dispatch_date, "month")) |>
      count(year_month)
    
    ggplot(trend_data, aes(x = year_month, y = n)) +
      geom_line(color = "#007bff", size = 1) +
      geom_point(color = "#007bff", size = 2) +
      labs(title = NULL, x = "Month", y = "Number of Incidents") +
      theme_minimal() +
      # THIS IS THE LINE TO CHANGE:
      scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(labels = scales::comma)
  })
  
  # This is the render function for the weekday plot
  output$weekday_plot <- renderPlot({
    plot_data <- filtered_data() |>
      mutate(weekday = wday(dispatch_date, label = TRUE, week_start = 1)) |>
      filter(!is.na(weekday)) |>
      count(weekday)
    
    ggplot(plot_data, aes(x = weekday, y = n)) +
      geom_col(fill = "#007bff") +
      labs(title = NULL, x = "Day of the Week", y = "Total Number of Incidents") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) 
  })
  
}

# ---- 4. Run the Application ----
shinyApp(ui = ui, server = server)