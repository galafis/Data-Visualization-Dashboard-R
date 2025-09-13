# Upload Module for Data Visualization Dashboard
# Author: galafis
# Description: Robust CSV upload with validation, preview, and metadata
# Date: September 2025

library(readr)
library(DT)
library(dplyr)

#' Upload Module UI
#' 
#' Creates the user interface for the upload module
#' @param id Character string. The module's namespace ID
#' @return A fluidRow containing upload controls and preview
#' @export
uploadModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      12,
      h3("Data Upload", class = "text-primary"),
      hr(),
      
      fluidRow(
        column(
          6,
          fileInput(
            ns("file"),
            "Choose CSV File:",
            accept = c(".csv", ".txt", "text/csv", "text/plain"),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
          ),
          checkboxInput(ns("header"), "Header", value = TRUE),
          checkboxInput(ns("show_preview"), "Show preview after upload", value = TRUE)
        ),
        column(
          6,
          radioButtons(
            ns("sep"),
            "Separator:",
            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
            selected = ",",
            inline = TRUE
          ),
          radioButtons(
            ns("quote"),
            "Quote:",
            choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
            selected = '"',
            inline = TRUE
          ),
          checkboxInput(ns("guess_types"), "Auto-detect column types", value = TRUE)
        )
      ),
      
      hr(),
      
      fluidRow(
        column(
          12,
          uiOutput(ns("meta"))
        )
      ),
      
      conditionalPanel(
        condition = paste0("output['", ns("dataUploaded"), "'] && input['", ns("show_preview"), "']"),
        h4("Data Preview"),
        DT::dataTableOutput(ns("preview"))
      )
    )
  )
}

#' Upload Module Server
#' 
#' Handles CSV upload with validation and preview
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @return Reactive expression with uploaded data frame
#' @export
uploadModule <- function(input, output, session) {
  ns <- session$ns
  
  # Validate file extension and size
  validate_file <- function(file) {
    req(file)
    ext <- tools::file_ext(file$name)
    if (!tolower(ext) %in% c("csv", "txt")) {
      stop("Unsupported file type. Please upload a CSV or TXT file.")
    }
    invisible(TRUE)
  }
  
  # Read data using readr for robustness
  read_data <- function(file) {
    if (isTRUE(input$guess_types)) {
      readr::read_delim(
        file$datapath,
        delim = input$sep,
        quote = input$quote,
        col_names = input$header,
        show_col_types = FALSE,
        progress = FALSE
      )
    } else {
      # Fall back to base R read.csv
      utils::read.csv(
        file$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        stringsAsFactors = FALSE
      )
    }
  }
  
  data <- reactive({
    req(input$file)
    
    tryCatch({
      validate_file(input$file)
      df <- read_data(input$file)
      # Convert tibble to data.frame for consistency in downstream modules
      if (inherits(df, "tbl_df")) df <- as.data.frame(df)
      df
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
  })
  
  # Output flag for conditionalPanel
  output$dataUploaded <- reactive({
    !is.null(data())
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  # Metadata UI
  output$meta <- renderUI({
    req(data())
    df <- data()
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    tagList(
      div(
        class = "alert alert-info",
        HTML(
          sprintf(
            "<b>File:</b> %s<br/><b>Rows:</b> %s &nbsp; <b>Columns:</b> %s",
            if (!is.null(input$file$name)) input$file$name else "(unknown)",
            format(n_rows, big.mark = "."),
            format(n_cols, big.mark = ".")
          )
        )
      )
    )
  })
  
  # Preview table
  output$preview <- DT::renderDataTable({
    req(data())
    DT::datatable(
      head(data(), 1000),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  return(data)
}
