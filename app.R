# app.R

# ---- 1. Load libraries and data ----
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)

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
               plotlyOutput("time_trend_plot")
        )
      ),
      hr(),
      fluidRow(
        column(width = 12,
               h3("Incidents by Day of the Week"),
               plotlyOutput("weekday_plot")
        )
      )
    )
  )
)


# ---- 3. Define Server Logic ----
server <- function(input, output) {
  
  filtered_data <- reactive({
    app_data |>
      filter(
        dispatch_date >= input$date_range[1],
        dispatch_date <= input$date_range[2]
      )
  })
  
  output$total_incidents <- renderUI({
    total <- n_distinct(filtered_data()$incident_number)
    tags$p(prettyNum(total, big.mark = ",")) 
  })
  
  output$total_naloxone <- renderUI({
    total <- sum(filtered_data()$naloxone_administrations, na.rm = TRUE)
    tags$p(prettyNum(total, big.mark = ","))
  })
  
  # --- Correct definition for the time trend plot ---
  output$time_trend_plot <- renderPlotly({
    trend_data <- filtered_data() |>
      filter(!is.na(dispatch_date)) |>
      mutate(year_month = floor_date(dispatch_date, "month")) |>
      count(year_month) |>
      mutate(
        tooltip_text = paste(
          "Month:", format(year_month, "%b %Y"),
          "<br>Incidents:", n
        )
      )
    
    # NEW: Build the plot directly with plot_ly
    plot_ly(
      data = trend_data,
      x = ~year_month,
      y = ~n,
      type = 'scatter',  # In plotly, a line chart is a type of scatter plot
      mode = 'lines',    # We specify we want to see lines
      line = list(color = '#007bff'), # Set the line color
      
      # Set up the custom hover tooltip
      hoverinfo = 'text',
      text = ~tooltip_text
    ) |>
      # Add layout information like axis titles
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Number of Incidents")
      )
    
  })
  
  # --- Correct definition for the weekday bar chart ---
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
  
}


# ---- 4. Run the Application ----
shinyApp(ui = ui, server = server)