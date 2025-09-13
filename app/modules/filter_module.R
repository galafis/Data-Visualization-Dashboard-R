# Filter Module for Data Visualization Dashboard
# Author: galafis
# Description: Provides UI and server logic for data filtering functionality
# Date: September 2025

#' Filter Module UI
#' 
#' Creates the user interface for the filter module
#' @param id Character string. The module's namespace ID
#' @return A fluidRow containing filter controls
#' @export
filterModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      4,
      selectInput(
        ns("filter"),
        "Filter by Category:",
        choices = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = '100%'
      )
    ),
    column(
      4,
      checkboxInput(
        ns("enable"),
        "Enable Filter",
        value = TRUE,
        width = '100%'
      )
    ),
    column(
      4,
      actionButton(
        ns("reset"),
        "Reset Filter",
        class = "btn-warning",
        style = "margin-top: 25px;",
        width = '100%'
      )
    )
  )
}

#' Filter Module Server
#' 
#' Provides server-side logic for data filtering
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param data Reactive expression containing the data to filter
#' @param filter_column Character string. Column name to filter on (optional)
#' @return Reactive expression with filtered data
#' @export
filterModule <- function(input, output, session, data, filter_column = NULL) {
  
  # Update filter choices when data changes
  observe({
    req(data())
    
    if (!is.null(filter_column) && filter_column %in% names(data())) {
      choices <- sort(unique(data()[[filter_column]]))
      choices <- choices[!is.na(choices)]  # Remove NA values
      
      updateSelectInput(
        session,
        "filter",
        choices = c("All" = "", setNames(choices, choices)),
        selected = ""
      )
    } else {
      # If no filter column specified, use first character/factor column
      char_cols <- names(data())[sapply(data(), function(x) is.character(x) || is.factor(x))]
      
      if (length(char_cols) > 0) {
        first_col <- char_cols[1]
        choices <- sort(unique(data()[[first_col]]))
        choices <- choices[!is.na(choices)]
        
        updateSelectInput(
          session,
          "filter",
          choices = c("All" = "", setNames(choices, choices)),
          selected = ""
        )
      }
    }
  })
  
  # Reset filter when reset button is clicked
  observeEvent(input$reset, {
    updateSelectInput(session, "filter", selected = "")
    updateCheckboxInput(session, "enable", value = TRUE)
  })
  
  # Return filtered data based on inputs
  filtered_data <- reactive({
    req(data())
    
    if (!input$enable || is.null(input$filter) || input$filter == "") {
      return(data())
    }
    
    # Determine which column to filter on
    col_to_filter <- filter_column
    if (is.null(col_to_filter)) {
      char_cols <- names(data())[sapply(data(), function(x) is.character(x) || is.factor(x))]
      if (length(char_cols) > 0) {
        col_to_filter <- char_cols[1]
      } else {
        return(data())  # No suitable column found
      }
    }
    
    # Apply filter
    if (col_to_filter %in% names(data())) {
      filtered <- data()[data()[[col_to_filter]] == input$filter, ]
      # Remove rows with NA in the filter column
      filtered <- filtered[!is.na(filtered[[col_to_filter]]), ]
      return(filtered)
    } else {
      return(data())
    }
  })
  
  return(filtered_data)
}
