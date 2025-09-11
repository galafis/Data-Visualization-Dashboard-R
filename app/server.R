# Data Visualization Dashboard - Server
# Gabriel Demetrios Lafis
# Professional Shiny Application Server Logic

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(leaflet)
library(corrplot)
library(readr)
library(lubridate)

# Source utility functions
source("../R/data_processing.R", local = TRUE)
source("../R/visualization_functions.R", local = TRUE)
source("../R/statistical_analysis.R", local = TRUE)
source("../R/utils.R", local = TRUE)

# Source modules
source("modules/data_module.R", local = TRUE)
source("modules/chart_module.R", local = TRUE)
source("modules/filter_module.R", local = TRUE)
source("modules/export_module.R", local = TRUE)

# Main server function
server <- function(input, output, session) {
  
  # Load data reactively
  data <- reactive({
    tryCatch({
      load_and_process_data()
    }, error = function(e) {
      # Fallback to sample data if no data file exists
      generate_sample_data()
    })
  })
  
  # Filtered data based on modules
  filtered_data <- filterServer("filters", data)
  
  # Value boxes
  output$total_records <- renderValueBox({
    valueBox(
      value = formatC(nrow(data()), format="d", big.mark=","),
      subtitle = "Total Records",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$avg_value <- renderValueBox({
    avg_val <- round(mean(data()$value, na.rm = TRUE), 2)
    valueBox(
      value = paste0("$", formatC(avg_val, format="f", digits=2, big.mark=",")),
      subtitle = "Average Value",
      icon = icon("calculator"),
      color = "green"
    )
  })
  
  output$growth_rate <- renderValueBox({
    growth <- calculate_growth_rate(data())
    valueBox(
      value = paste0(round(growth * 100, 1), "%"),
      subtitle = "Growth Rate",
      icon = icon(if(growth >= 0) "arrow-up" else "arrow-down"),
      color = if(growth >= 0) "green" else "red"
    )
  })
  
  # Trend plot
  output$trend_plot <- renderPlotly({
    req(data())
    create_trend_plot(data())
  })
  
  # Distribution plot
  output$distribution_plot <- renderPlot({
    req(data())
    create_distribution_plot(data())
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    req(data())
    DT::datatable(
      data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      extensions = 'Buttons',
      rownames = FALSE
    )
  })
  
  # Chart module server
  chartServer("scatter_chart", filtered_data)
  
  # Correlation heatmap
  output$correlation_heatmap <- renderPlotly({
    req(data())
    create_correlation_heatmap(data())
  })
  
  # Statistical summary
  output$statistical_summary <- renderPrint({
    req(data())
    perform_descriptive_analysis(data())
  })
  
  # Interactive map
  output$interactive_map <- renderLeaflet({
    req(data())
    # Generate sample location data if not available
    map_data <- data() %>%
      slice_sample(n = min(100, nrow(.))) %>%
      mutate(
        latitude = runif(n(), min = -30, max = 30),
        longitude = runif(n(), min = -60, max = 60),
        name = paste0("Location ", row_number())
      )
    
    create_interactive_map(map_data)
  })
  
  # Export module server
  exportServer("export_module", data)
  
  # Report preview
  output$report_preview <- renderUI({
    tags$div(
      h4("Dashboard Report Preview"),
      p("This section will show a preview of the generated report."),
      p(paste("Last updated:", Sys.time())),
      hr(),
      h5("Quick Stats:"),
      tags$ul(
        tags$li(paste("Total Records:", nrow(data()))),
        tags$li(paste("Data Range:", min(data()$date, na.rm = TRUE), "to", max(data()$date, na.rm = TRUE))),
        tags$li(paste("Categories:", length(unique(data()$category))))
      )
    )
  })
  
  # Theme reactivity
  observeEvent(input$theme, {
    if (input$theme == "dark") {
      addClass("body", "dark-theme")
    } else {
      removeClass("body", "dark-theme")
    }
  })
  
  # Auto-refresh functionality
  auto_refresh_timer <- reactiveTimer(30000) # 30 seconds default
  
  observeEvent(input$refresh_interval, {
    auto_refresh_timer <- reactiveTimer(input$refresh_interval * 1000)
  })
  
  observe({
    if (input$auto_refresh) {
      auto_refresh_timer()
      # Trigger data reload
      isolate({
        # This would reload data in a real application
        showNotification("Data refreshed", type = "message", duration = 3)
      })
    }
  })
  
  # Session info for debugging
  output$session_info <- renderPrint({
    sessionInfo()
  })
  
  # Error handling
  options(shiny.error = function() {
    showNotification(
      "An error occurred. Please check the console for details.", 
      type = "error",
      duration = 5
    )
  })
}
