filterModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4, selectInput(ns("filter"), "Filter:", choices = NULL)),
    column(4, checkboxInput(ns("enable"), "Enable Filter", value = TRUE))
  )
}
filterModule <- function(input, output, session, data) {
  # Return filtered data based on inputs
  reactive({
    if(input$enable && !is.null(input$filter)) {
      # Apply filter logic here
      data()
    } else {
      data()
    }
  })
}
