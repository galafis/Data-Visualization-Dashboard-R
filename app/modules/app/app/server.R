library(shiny)
library(plotly)
library(ggplot2)
library(DT)

function(input, output, session) {
  data <- reactive({
    read.csv("data/sample/demo_sample.csv")
  })

  output$total_records <- renderValueBox({
    valueBox(nrow(data()), "Total Records", icon = icon("database"), color = "blue")
  })
  output$avg_value <- renderValueBox({
    valueBox(round(mean(data()$value, na.rm = TRUE),2), "Average Value", icon = icon("calculator"), color = "green")
  })
  output$growth_rate <- renderValueBox({
    valueBox("15%", "Growth Rate", icon = icon("arrow-up"), color = "orange")
  })
  output$trend_plot <- renderPlotly({
    p <- ggplot(data(), aes(x = as.Date(date), y = value)) + geom_line() + theme_minimal()
    ggplotly(p)
  })
  output$distribution_plot <- renderPlot({
    ggplot(data(), aes(x = category, fill = category)) + geom_bar() + theme_minimal()
  })
  output$datatable <- DT::renderDataTable({
    data()
  })
}
