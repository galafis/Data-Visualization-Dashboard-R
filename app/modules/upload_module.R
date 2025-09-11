uploadModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12,
      h4("Data Upload"),
      fluidRow(
        column(6, 
          fileInput(ns("file"), "Choose CSV File:",
                   accept = c(".csv", ".txt")),
          checkboxInput(ns("header"), "Header", value = TRUE),
          checkboxInput(ns("stringsAsFactors"), "Strings as factors", value = FALSE)
        ),
        column(6,
          radioButtons(ns("sep"), "Separator:",
                      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                      selected = ","),
          radioButtons(ns("quote"), "Quote:",
                      choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                      selected = '"')
        )
      ),
      conditionalPanel(
        condition = paste0("output['", ns("dataUploaded"), "']"),
        h5("Data Preview:"),
        DT::dataTableOutput(ns("preview"))
      )
    )
  )
}
uploadModule <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    
    tryCatch({
      df <- read.csv(input$file$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote,
                    stringsAsFactors = input$stringsAsFactors)
      return(df)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$dataUploaded <- reactive({
    !is.null(data())
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  output$preview <- DT::renderDataTable({
    data()
  }, options = list(pageLength = 5, scrollX = TRUE))
  
  return(data)
}
