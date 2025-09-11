chartModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,
      h4("Data Visualization"),
      fluidRow(
        column(4, selectInput(ns("chart_type"), "Chart Type:", 
                             choices = c("Scatter Plot" = "scatter", 
                                       "Bar Chart" = "bar", 
                                       "Line Chart" = "line",
                                       "Histogram" = "histogram"))),
        column(4, selectInput(ns("x_var"), "X Variable:", choices = NULL)),
        column(4, selectInput(ns("y_var"), "Y Variable:", choices = NULL))
      ),
      plotOutput(ns("plot"))
    )
  )
}
chartModule <- function(input, output, session, data) {
  output$plot <- renderPlot({
    if(!is.null(data()) && nrow(data()) > 0) {
      library(ggplot2)
      
      if(input$chart_type == "scatter") {
        ggplot(data(), aes_string(x = input$x_var, y = input$y_var)) + 
          geom_point() + theme_minimal()
      } else if(input$chart_type == "bar") {
        ggplot(data(), aes_string(x = input$x_var)) + 
          geom_bar() + theme_minimal()
      } else if(input$chart_type == "line") {
        ggplot(data(), aes_string(x = input$x_var, y = input$y_var)) + 
          geom_line() + theme_minimal()
      } else if(input$chart_type == "histogram") {
        ggplot(data(), aes_string(x = input$x_var)) + 
          geom_histogram(bins = 30) + theme_minimal()
      }
    } else {
      plot(1, type = "n", main = "No data available for plotting")
    }
  })
  
  # Update variable choices when data changes
  observe({
    if(!is.null(data())) {
      choices <- names(data())
      updateSelectInput(session, "x_var", choices = choices)
      updateSelectInput(session, "y_var", choices = choices)
    }
  })
}
