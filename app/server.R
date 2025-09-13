# ==============================================================================
# SERVER.R - Data Visualization Dashboard
# ==============================================================================
# Purpose: Main server logic for the Shiny application
# Author: [Author Name]
# Date: 2025-09-13
# Version: 1.0
# ==============================================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Source module files
source("modules/data_import_module.R")
source("modules/data_filter_module.R")
source("modules/chart_config_module.R")
source("modules/export_module.R")
source("utils/data_validation.R")
source("utils/chart_helpers.R")

# ==============================================================================
# MAIN SERVER FUNCTION
# ==============================================================================
server <- function(input, output, session) {
  
  # Global reactive values
  values <- reactiveValues(
    data = NULL,
    filtered_data = NULL,
    chart_config = list(
      type = "scatter",
      x_var = NULL,
      y_var = NULL,
      color_var = NULL,
      title = "Data Visualization"
    )
  )
  
  # ==============================================================================
  # MODULE SERVERS
  # ==============================================================================
  
  # Data Import Module
  imported_data <- dataImportServer(
    "data_import",
    parent_session = session
  )
  
  # Update global data when new data is imported
  observe({
    req(imported_data())
    values$data <- imported_data()
    values$filtered_data <- imported_data()
    
    # Show success notification
    showNotification(
      "Data imported successfully!",
      type = "success",
      duration = 3
    )
  }) %>% bindEvent(imported_data())
  
  # Data Filter Module
  filtered_data <- dataFilterServer(
    "data_filter",
    data = reactive(values$data)
  )
  
  # Update filtered data
  observe({
    req(filtered_data())
    values$filtered_data <- filtered_data()
  }) %>% bindEvent(filtered_data())
  
  # Chart Configuration Module
  chart_config <- chartConfigServer(
    "chart_config",
    data = reactive(values$filtered_data)
  )
  
  # Update chart configuration
  observe({
    req(chart_config())
    values$chart_config <- chart_config()
  }) %>% bindEvent(chart_config())
  
  # Export Module
  exportServer(
    "export",
    data = reactive(values$filtered_data),
    chart_config = reactive(values$chart_config)
  )
  
  # ==============================================================================
  # DATA TABLE OUTPUT
  # ==============================================================================
  output$data_table <- DT::renderDataTable({
    req(values$filtered_data)
    
    tryCatch({
      DT::datatable(
        values$filtered_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          searchHighlight = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        filter = 'top',
        selection = 'multiple',
        class = 'cell-border stripe hover'
      )
    }, error = function(e) {
      showNotification(
        paste("Error rendering data table:", e$message),
        type = "error",
        duration = 5
      )
      return(NULL)
    })
  })
  
  # ==============================================================================
  # MAIN CHART OUTPUT
  # ==============================================================================
  output$main_chart <- plotly::renderPlotly({
    req(values$filtered_data, values$chart_config)
    
    tryCatch({
      # Validate required variables
      if (is.null(values$chart_config$x_var) || is.null(values$chart_config$y_var)) {
        return(
          plotly::plot_ly() %>%
            plotly::add_text(
              x = 0.5, y = 0.5,
              text = "Please select X and Y variables",
              textfont = list(size = 16, color = "gray")
            ) %>%
            plotly::layout(
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              showlegend = FALSE
            )
        )
      }
      
      # Generate chart based on type
      p <- generate_chart(
        data = values$filtered_data,
        config = values$chart_config
      )
      
      # Apply consistent styling
      p %>%
        plotly::layout(
          title = list(
            text = values$chart_config$title,
            font = list(size = 18, family = "Arial, sans-serif")
          ),
          xaxis = list(
            title = values$chart_config$x_var,
            titlefont = list(size = 14)
          ),
          yaxis = list(
            title = values$chart_config$y_var,
            titlefont = list(size = 14)
          ),
          hovermode = "closest",
          showlegend = !is.null(values$chart_config$color_var)
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = list(
            "pan2d", "lasso2d", "select2d", "autoScale2d"
          )
        )
      
    }, error = function(e) {
      showNotification(
        paste("Error generating chart:", e$message),
        type = "error",
        duration = 5
      )
      
      # Return error plot
      plotly::plot_ly() %>%
        plotly::add_text(
          x = 0.5, y = 0.5,
          text = paste("Chart Error:", e$message),
          textfont = list(size = 14, color = "red")
        ) %>%
        plotly::layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          showlegend = FALSE
        )
    })
  })
  
  # ==============================================================================
  # SUMMARY STATISTICS OUTPUT
  # ==============================================================================
  output$summary_stats <- renderUI({
    req(values$filtered_data)
    
    tryCatch({
      data <- values$filtered_data
      numeric_cols <- select_if(data, is.numeric)
      
      if (ncol(numeric_cols) == 0) {
        return(
          div(
            class = "alert alert-info",
            "No numeric variables available for summary statistics."
          )
        )
      }
      
      # Generate summary statistics
      summary_data <- numeric_cols %>%
        summarise_all(
          list(
            Count = ~ sum(!is.na(.)),
            Mean = ~ round(mean(., na.rm = TRUE), 3),
            Median = ~ round(median(., na.rm = TRUE), 3),
            SD = ~ round(sd(., na.rm = TRUE), 3),
            Min = ~ round(min(., na.rm = TRUE), 3),
            Max = ~ round(max(., na.rm = TRUE), 3)
          )
        ) %>%
        pivot_longer(
          cols = everything(),
          names_to = c("Variable", "Statistic"),
          names_sep = "_",
          values_to = "Value"
        ) %>%
        pivot_wider(
          names_from = Statistic,
          values_from = Value
        )
      
      # Render as DT table
      DT::renderDataTable({
        DT::datatable(
          summary_data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 't',
            ordering = FALSE
          ),
          rownames = FALSE,
          class = 'cell-border stripe hover compact'
        ) %>%
          DT::formatRound(
            columns = c("Mean", "Median", "SD", "Min", "Max"),
            digits = 3
          )
      })
      
    }, error = function(e) {
      showNotification(
        paste("Error generating summary statistics:", e$message),
        type = "error",
        duration = 5
      )
      
      div(
        class = "alert alert-danger",
        paste("Error generating summary statistics:", e$message)
      )
    })
  })
  
  # ==============================================================================
  # DATA INFO OUTPUT
  # ==============================================================================
  output$data_info <- renderUI({
    req(values$data)
    
    total_rows <- nrow(values$data)
    filtered_rows <- if (!is.null(values$filtered_data)) nrow(values$filtered_data) else total_rows
    total_cols <- ncol(values$data)
    
    # Column type summary
    col_types <- sapply(values$data, class) %>%
      table() %>%
      as.data.frame()
    names(col_types) <- c("Type", "Count")
    
    tagList(
      div(
        class = "row",
        div(
          class = "col-md-6",
          div(
            class = "info-box bg-blue",
            div(class = "info-box-icon", icon("table")),
            div(
              class = "info-box-content",
              span(class = "info-box-text", "Total Rows"),
              span(class = "info-box-number", format(total_rows, big.mark = ","))
            )
          )
        ),
        div(
          class = "col-md-6",
          div(
            class = "info-box bg-green",
            div(class = "info-box-icon", icon("filter")),
            div(
              class = "info-box-content",
              span(class = "info-box-text", "Filtered Rows"),
              span(class = "info-box-number", format(filtered_rows, big.mark = ","))
            )
          )
        )
      ),
      div(
        class = "row",
        div(
          class = "col-md-12",
          h4("Column Types"),
          DT::renderDataTable({
            DT::datatable(
              col_types,
              options = list(
                pageLength = 10,
                dom = 't',
                ordering = FALSE
              ),
              rownames = FALSE,
              class = 'cell-border stripe hover compact'
            )
          })
        )
      )
    )
  })
  
  # ==============================================================================
  # SESSION MANAGEMENT
  # ==============================================================================
  
  # Handle session disconnect
  session$onSessionEnded(function() {
    # Clean up any resources if needed
    message("Session ended at ", Sys.time())
  })
  
  # Initialize application
  observe({
    showNotification(
      "Dashboard initialized successfully!",
      type = "success",
      duration = 3
    )
  }, once = TRUE)
  
  # ==============================================================================
  # ERROR HANDLING
  # ==============================================================================
  
  # Global error handler
  observe({
    # Monitor for any unhandled errors
    if (exists("last.warning", envir = .GlobalEnv)) {
      warning_msg <- get("last.warning", envir = .GlobalEnv)
      if (!is.null(warning_msg) && length(warning_msg) > 0) {
        showNotification(
          paste("Warning:", warning_msg),
          type = "warning",
          duration = 4
        )
      }
    }
  })
  
}

# ==============================================================================
# END OF FILE
# ==============================================================================
