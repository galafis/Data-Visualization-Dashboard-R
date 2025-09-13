# =============================================================================
# Data Module for Data Visualization Dashboard
# =============================================================================
# Description: Interactive data display and management module
# Author: galafis
# Last Updated: 2025-09-13
# Dependencies: shiny, DT, dplyr, readr
# =============================================================================

# Load required libraries
library(DT)
library(dplyr)
library(readr)

#' Data Module UI
#' 
#' Creates the user interface for the data display and management module
#' 
#' @param id Character string. The module's namespace ID
#' @return A tagList containing the UI elements
dataModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
      # Module header
      div(
        h4("Data Overview", class = "panel-title"),
        p("View and manage your dataset", class = "text-muted"),
        class = "panel-header"
      ),
      
      # Data summary panel
      conditionalPanel(
        condition = "output.data_summary",
        ns = ns,
        wellPanel(
          h5("Dataset Summary"),
          verbatimTextOutput(ns("data_summary")),
          class = "summary-panel"
        )
      ),
      
      # Data table controls
      fluidRow(
        column(3,
          numericInput(ns("rows_per_page"), 
                      "Rows per page:", 
                      value = 25, 
                      min = 10, 
                      max = 100, 
                      step = 5)
        ),
        column(3,
          checkboxInput(ns("show_row_numbers"), 
                       "Show row numbers", 
                       value = TRUE)
        ),
        column(3,
          checkboxInput(ns("enable_search"), 
                       "Enable search", 
                       value = TRUE)
        ),
        column(3,
          downloadButton(ns("download_data"), 
                        "Download CSV", 
                        class = "btn-primary")
        )
      ),
      
      br(),
      
      # Data table output with error handling
      conditionalPanel(
        condition = "output.has_data",
        ns = ns,
        div(
          class = "data-table-container",
          DT::dataTableOutput(ns("table"))
        )
      ),
      
      # No data message
      conditionalPanel(
        condition = "!output.has_data",
        ns = ns,
        wellPanel(
          div(
            class = "text-center",
            h4("No Data Available", class = "text-muted"),
            p("Please upload a dataset to view the data table.", class = "text-muted"),
            icon("table", class = "fa-3x text-muted")
          ),
          class = "no-data-panel"
        )
      ),
      
      # Data quality indicators
      conditionalPanel(
        condition = "output.data_quality",
        ns = ns,
        wellPanel(
          h5("Data Quality Indicators"),
          verbatimTextOutput(ns("data_quality")),
          class = "quality-panel"
        )
      )
    )
  )
}

#' Data Module Server
#' 
#' Server logic for the data display and management module
#' 
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param data Reactive expression containing the dataset
#' @return None (side effects only)
dataModule <- function(input, output, session, data) {
  
  # Reactive values for module state
  values <- reactiveValues(
    processed_data = NULL,
    error_message = NULL
  )
  
  # Check if data is available
  output$has_data <- reactive({
    !is.null(data()) && nrow(data()) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  # Process data when it changes
  observe({
    req(data())
    
    tryCatch({
      df <- data()
      if (!is.null(df) && nrow(df) > 0) {
        values$processed_data <- df
        values$error_message <- NULL
      } else {
        values$processed_data <- NULL
        values$error_message <- "Dataset is empty"
      }
    }, error = function(e) {
      values$processed_data <- NULL
      values$error_message <- paste("Error processing data:", e$message)
    })
  })
  
  # Data summary output
  output$data_summary <- renderText({
    req(values$processed_data)
    
    df <- values$processed_data
    
    # Basic statistics
    summary_text <- paste(
      sprintf("Rows: %s", format(nrow(df), big.mark = ",")),
      sprintf("Columns: %d", ncol(df)),
      sprintf("Memory usage: %.2f MB", object.size(df) / 1024^2),
      sep = " | "
    )
    
    # Column types summary
    col_types <- sapply(df, class)
    numeric_cols <- sum(sapply(df, is.numeric))
    character_cols <- sum(sapply(df, is.character))
    factor_cols <- sum(sapply(df, is.factor))
    date_cols <- sum(sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXt")))
    
    type_summary <- paste(
      sprintf("Numeric: %d", numeric_cols),
      sprintf("Character: %d", character_cols),
      sprintf("Factor: %d", factor_cols),
      sprintf("Date: %d", date_cols),
      sep = " | "
    )
    
    paste(summary_text, type_summary, sep = "\n")
  })
  
  # Data quality indicators
  output$data_quality <- renderText({
    req(values$processed_data)
    
    df <- values$processed_data
    
    # Missing values analysis
    total_cells <- nrow(df) * ncol(df)
    missing_cells <- sum(is.na(df))
    missing_percentage <- round((missing_cells / total_cells) * 100, 2)
    
    # Duplicate rows
    duplicate_rows <- sum(duplicated(df))
    
    # Empty strings in character columns
    char_cols <- sapply(df, is.character)
    empty_strings <- if(any(char_cols)) {
      sum(df[char_cols] == "", na.rm = TRUE)
    } else {
      0
    }
    
    quality_text <- paste(
      sprintf("Missing values: %s (%.2f%%)", format(missing_cells, big.mark = ","), missing_percentage),
      sprintf("Duplicate rows: %s", format(duplicate_rows, big.mark = ",")),
      sprintf("Empty strings: %s", format(empty_strings, big.mark = ",")),
      sep = "\n"
    )
    
    if (!is.null(values$error_message)) {
      quality_text <- paste(quality_text, "\nError:", values$error_message, sep = "")
    }
    
    quality_text
  })
  
  # Main data table
  output$table <- DT::renderDataTable({
    req(values$processed_data)
    
    tryCatch({
      df <- values$processed_data
      
      # Configure DataTable options
      options_list <- list(
        pageLength = input$rows_per_page %||% 25,
        searching = input$enable_search %||% TRUE,
        scrollX = TRUE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(
          className = "dt-center",
          targets = "_all"
        ))
      )
      
      # Create DataTable
      dt <- DT::datatable(
        df,
        options = options_list,
        rownames = input$show_row_numbers %||% TRUE,
        class = "table table-striped table-hover",
        escape = FALSE
      )
      
      # Format numeric columns
      numeric_cols <- which(sapply(df, is.numeric))
      if (length(numeric_cols) > 0) {
        dt <- DT::formatRound(dt, columns = numeric_cols, digits = 3)
      }
      
      # Format percentage columns (if any)
      pct_cols <- grep("%|percent|pct", names(df), ignore.case = TRUE)
      if (length(pct_cols) > 0) {
        dt <- DT::formatPercentage(dt, columns = pct_cols, digits = 2)
      }
      
      return(dt)
      
    }, error = function(e) {
      # Return empty table with error message
      DT::datatable(
        data.frame(Error = paste("Error rendering table:", e$message)),
        options = list(searching = FALSE, paging = FALSE, info = FALSE),
        rownames = FALSE
      )
    })
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_export_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$processed_data)
      
      tryCatch({
        write_csv(values$processed_data, file)
      }, error = function(e) {
        # Create error file if download fails
        writeLines(paste("Error exporting data:", e$message), file)
      })
    },
    contentType = "text/csv"
  )
  
  # Return reactive data for other modules
  return(reactive({
    values$processed_data
  }))
}

#' Get Data Summary
#' 
#' Helper function to generate a comprehensive data summary
#' 
#' @param data Data frame to summarize
#' @return Character string with summary information
get_data_summary <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return("No data available")
  }
  
  # Basic info
  basic_info <- sprintf(
    "Dataset: %d rows × %d columns\nSize: %.2f MB",
    nrow(data), ncol(data), object.size(data) / 1024^2
  )
  
  # Column types
  col_summary <- data %>%
    summarise_all(class) %>%
    gather(column, type) %>%
    count(type) %>%
    mutate(summary = sprintf("%s: %d", type, n)) %>%
    pull(summary) %>%
    paste(collapse = ", ")
  
  paste(basic_info, col_summary, sep = "\n\nColumn types: ")
}

#' Check Data Quality
#' 
#' Helper function to assess data quality
#' 
#' @param data Data frame to check
#' @return List with quality metrics
check_data_quality <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      missing_values = 0,
      duplicate_rows = 0,
      empty_strings = 0,
      quality_score = 0
    ))
  }
  
  # Missing values
  missing_values <- sum(is.na(data))
  missing_percentage <- (missing_values / (nrow(data) * ncol(data))) * 100
  
  # Duplicate rows
  duplicate_rows <- sum(duplicated(data))
  
  # Empty strings
  char_cols <- sapply(data, is.character)
  empty_strings <- if(any(char_cols)) {
    sum(data[char_cols] == "", na.rm = TRUE)
  } else {
    0
  }
  
  # Quality score (0-100)
  quality_score <- max(0, 100 - missing_percentage - (duplicate_rows / nrow(data)) * 10)
  
  list(
    missing_values = missing_values,
    missing_percentage = missing_percentage,
    duplicate_rows = duplicate_rows,
    empty_strings = empty_strings,
    quality_score = round(quality_score, 2)
  )
}
