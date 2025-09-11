summaryModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, 
      h4("Data Summary"),
      verbatimTextOutput(ns("summary"))
    )
  )
}
summaryModule <- function(input, output, session, data) {
  output$summary <- renderText({
    if(!is.null(data())) {
      summary(data())
    } else {
      "No data available"
    }
  })
}
