dataModuleUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table"))
}
dataModule <- function(input, output, session, data) {
  output$table <- DT::renderDataTable({data()})
}
