# app.R

# ---- 1. Load libraries and data ----
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)

app_data <- readRDS("data/app_data.rds")


# ---- 2. Define User Interface (UI) ----

# NEW: Let's prepare the choices for our dropdowns here for cleanliness
neighbourhood_choices <- c("All Neighbourhoods", 
                           sort(unique(na.omit(app_data$neighbourhood))))
gender_choices <- c("All Genders", "Male", "Female")

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
    
    # UPDATED: We are adding new controls to the sidebar
    sidebarPanel(
      h4("Filters"),
      
      dateRangeInput(
        inputId = "date_range",
        label = "Filter by Incident Date:",
        start = min(app_data$dispatch_date, na.rm = TRUE),
        end = max(app_data$dispatch_date, na.rm = TRUE)
      ),
      
      # NEW: Dropdown menu for neighbourhood
      selectInput(
        inputId = "neighbourhood_filter",
        label = "Filter by Neighbourhood:",
        choices = neighbourhood_choices
      ),
      
      # NEW: Radio buttons for gender
      radioButtons(
        inputId = "gender_filter",
        label = "Filter by Gender:",
        choices = gender_choices
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
      # UPDATED: We'll put the two smaller plots side-by-side
      fluidRow(
        column(width = 6,
               h3("Incidents by Day of the Week"),
               plotlyOutput("weekday_plot")
        ),
        column(width = 6,
               h3("Top Substances Involved"),
               plotlyOutput("substance_plot")
        )
      ),
      # NEW: A new row for our population pyramid
      hr(),
      fluidRow(
        column(width = 12,
               h3("Demographic Breakdown by Age and Gender"),
               plotlyOutput("pyramid_plot") # Placeholder for the new plot
        )
      )
    )
  )
)
  # ---- 3. Define Server Logic ----
  server <- function(input, output) {
    
    # UPDATED: Our central reactive now listens to the new inputs
    filtered_data <- reactive({
      
      # Start with the full dataset
      data <- app_data
      
      # 1. Filter by date range (this is the same as before)
      data <- data |>
        filter(
          dispatch_date >= input$date_range[1],
          dispatch_date <= input$date_range[2]
        )
      
      # 2. Filter by neighbourhood, if a specific one is selected
      if (input$neighbourhood_filter != "All Neighbourhoods") {
        data <- data |>
          filter(neighbourhood == input$neighbourhood_filter)
      }
      
      # 3. Filter by gender, if a specific one is selected
      if (input$gender_filter != "All Genders") {
        data <- data |>
          filter(gender == input$gender_filter)
      }
      
      # Return the final, filtered data
      data
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
  
  # --- New plot for top substances involved ---
  output$substance_plot <- renderPlotly({
    
    # Data wrangling to count individual substances
    substance_counts <- filtered_data() |>
      # 1. Drop rows where substances_involved is NA
      filter(!is.na(substances_involved)) |>
      # 2. Split the strings by "; "
      separate_rows(substances_involved, sep = "; ") |>
      # 3. Count each individual substance
      count(substances_involved, sort = TRUE, name = "count") |>
      # 4. Take the top 10
      top_n(10, count)
    
    # Create the ggplot bar chart
    p <- ggplot(substance_counts, aes(x = count, y = reorder(substances_involved, count), text = paste("Count:", count))) +
      geom_col(fill = "#007bff") +
      labs(x = "Total Count", y = NULL) + # No y-axis label needed
      theme_minimal()
    
    # Convert to ggplotly
    # We use tooltip = "text" to get a cleaner hover label
    ggplotly(p, tooltip = "text")
    
  })
  
  # NEW: Code to render the population pyramid plot
  output$pyramid_plot <- renderPlotly({
    
    # Data wrangling for the pyramid
    pyramid_data <- filtered_data() |>
      # 1. Keep only Male and Female, and ensure age is not NA
      filter(gender %in% c("Male", "Female"), !is.na(age)) |>
      # 2. Count incidents by age and gender
      count(age, gender, name = "count") |>
      # 3. The key trick: make male counts negative
      mutate(
        count = if_else(gender == "Male", -count, count)
      )
    
    # Create the ggplot
    p <- ggplot(pyramid_data, aes(x = count, y = age, fill = gender, text = paste("Count:", abs(count)))) +
      geom_col() +
      # Use a specific color palette
      scale_fill_manual(values = c("Male" = "#007bff", "Female" = "#E13468")) +
      labs(x = "Number of Incidents", y = "Age Group") +
      # This is the trick to format the x-axis labels to be positive numbers
      scale_x_continuous(labels = function(x) abs(x), breaks = pretty(pyramid_data$count)) +
      theme_minimal() +
      theme(legend.title = element_blank()) # Remove the legend title
    
    # Convert to ggplotly
    ggplotly(p, tooltip = "text") |>
      layout(
        # This makes the hover labels look consistent
        hovermode = "y unified"
      )
    
  })
  
  }
  


# ---- 4. Run the Application ----
shinyApp(ui = ui, server = server)