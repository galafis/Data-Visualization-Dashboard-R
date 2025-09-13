# =============================================================================
# Chart Module for Data Visualization Dashboard
# =============================================================================
# Description: Interactive chart module with multiple visualization types
# Author: galafis
# Last Updated: 2025-09-13
# Dependencies: shiny, ggplot2, dplyr
# =============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)

#' Chart Module UI
#' 
#' Creates the user interface for the chart visualization module
#' 
#' @param id Character string. The module's namespace ID
#' @return A tagList containing the UI elements
chartModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
      # Module header with improved styling
      div(
        h4("Data Visualization", class = "panel-title"),
        p("Create interactive charts from your data", class = "text-muted"),
        class = "panel-header"
      ),
      
      # Chart configuration panel
      wellPanel(
        fluidRow(
          column(3, 
            selectInput(ns("chart_type"), 
                       "Chart Type:", 
                       choices = c(
                         "Scatter Plot" = "scatter", 
                         "Bar Chart" = "bar", 
                         "Line Chart" = "line",
                         "Histogram" = "histogram",
                         "Box Plot" = "boxplot",
                         "Density Plot" = "density"
                       ),
                       selected = "scatter")
          ),
          column(3, 
            selectInput(ns("x_var"), 
                       "X Variable:", 
                       choices = NULL)
          ),
          column(3, 
            conditionalPanel(
              condition = "input.chart_type == 'scatter' || input.chart_type == 'line' || input.chart_type == 'boxplot'",
              ns = ns,
              selectInput(ns("y_var"), 
                         "Y Variable:", 
                         choices = NULL)
            )
          ),
          column(3,
            conditionalPanel(
              condition = "input.chart_type == 'scatter'",
              ns = ns,
              selectInput(ns("color_var"), 
                         "Color By (Optional):", 
                         choices = c("None" = ""))
            )
          )
        )
      ),
      
      # Chart output with loading indicator
      div(
        class = "chart-container",
        conditionalPanel(
          condition = "$('html').hasClass('shiny-busy')",
          div(class = "loading", "Loading chart...")
        ),
        plotOutput(ns("plot"), height = "500px")
      ),
      
      # Chart information panel
      conditionalPanel(
        condition = "output.chart_info",
        ns = ns,
        wellPanel(
          h5("Chart Information"),
          verbatimTextOutput(ns("chart_info"))
        )
      )
    )
  )
}

#' Chart Module Server
#' 
#' Server logic for the chart visualization module
#' 
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param data Reactive expression containing the dataset
#' @return None (side effects only)
chartModule <- function(input, output, session, data) {
  
  # Reactive values for error handling
  values <- reactiveValues(
    error_message = NULL,
    chart_data = NULL
  )
  
  # Update variable choices when data changes
  observe({
    req(data())
    
    tryCatch({
      df <- data()
      if (!is.null(df) && nrow(df) > 0) {
        # Get numeric and character columns
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        all_cols <- names(df)
        
        # Update X variable choices
        updateSelectInput(session, "x_var", 
                         choices = all_cols,
                         selected = if(length(all_cols) > 0) all_cols[1] else NULL)
        
        # Update Y variable choices (numeric only for most chart types)
        updateSelectInput(session, "y_var", 
                         choices = numeric_cols,
                         selected = if(length(numeric_cols) > 1) numeric_cols[2] 
                                   else if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        
        # Update color variable choices
        color_choices <- c("None" = "", setNames(all_cols, all_cols))
        updateSelectInput(session, "color_var", 
                         choices = color_choices)
        
        values$error_message <- NULL
      }
    }, error = function(e) {
      values$error_message <- paste("Error updating variables:", e$message)
    })
  })
  
  # Generate plot
  output$plot <- renderPlot({
    req(data(), input$x_var)
    
    tryCatch({
      df <- data()
      
      # Validate data
      if (is.null(df) || nrow(df) == 0) {
        return(create_empty_plot("No data available"))
      }
      
      # Validate required variables exist
      if (!input$x_var %in% names(df)) {
        return(create_empty_plot("Selected X variable not found in data"))
      }
      
      # Create base plot
      p <- create_chart(df, input$chart_type, input$x_var, input$y_var, input$color_var)
      
      # Store chart data for info panel
      values$chart_data <- df
      
      return(p)
      
    }, error = function(e) {
      values$error_message <- e$message
      return(create_error_plot(e$message))
    })
  })
  
  # Chart information output
  output$chart_info <- renderText({
    req(values$chart_data)
    
    df <- values$chart_data
    info <- paste(
      sprintf("Data points: %d", nrow(df)),
      sprintf("Variables: %d", ncol(df)),
      if(!is.null(input$x_var)) sprintf("X-axis: %s", input$x_var) else "",
      if(!is.null(input$y_var) && input$y_var != "") sprintf("Y-axis: %s", input$y_var) else "",
      sep = "\n"
    )
    
    if (!is.null(values$error_message)) {
      info <- paste(info, "\nError:", values$error_message, sep = "")
    }
    
    return(info)
  })
}

#' Create Chart Function
#' 
#' Helper function to create different types of charts
#' 
#' @param data Data frame containing the data
#' @param chart_type Character string indicating chart type
#' @param x_var Character string for X variable
#' @param y_var Character string for Y variable (optional)
#' @param color_var Character string for color variable (optional)
#' @return ggplot object
create_chart <- function(data, chart_type, x_var, y_var = NULL, color_var = NULL) {
  
  # Base theme
  base_theme <- theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  
  # Handle color mapping
  if (!is.null(color_var) && color_var != "" && color_var %in% names(data)) {
    color_aes <- rlang::sym(color_var)
  } else {
    color_aes <- NULL
  }
  
  # Create chart based on type
  switch(chart_type,
    "scatter" = {
      req(y_var)
      p <- ggplot(data, aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var)))
      if (!is.null(color_aes)) {
        p <- p + aes(color = !!color_aes)
      }
      p + geom_point(alpha = 0.7, size = 2) +
        labs(title = "Scatter Plot",
             subtitle = paste("Relationship between", x_var, "and", y_var)) +
        base_theme
    },
    
    "bar" = {
      p <- ggplot(data, aes(x = !!rlang::sym(x_var)))
      if (!is.null(color_aes)) {
        p <- p + aes(fill = !!color_aes)
      }
      p + geom_bar(alpha = 0.8) +
        labs(title = "Bar Chart",
             subtitle = paste("Distribution of", x_var)) +
        base_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },
    
    "line" = {
      req(y_var)
      p <- ggplot(data, aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var)))
      if (!is.null(color_aes)) {
        p <- p + aes(color = !!color_aes)
        p + geom_line(size = 1) + geom_point(size = 2, alpha = 0.7)
      } else {
        p + geom_line(size = 1, color = "steelblue") + geom_point(size = 2, alpha = 0.7, color = "steelblue")
      }
      p + labs(title = "Line Chart",
               subtitle = paste("Trend of", y_var, "over", x_var)) +
        base_theme
    },
    
    "histogram" = {
      ggplot(data, aes(x = !!rlang::sym(x_var))) +
        geom_histogram(bins = 30, alpha = 0.8, fill = "steelblue", color = "white") +
        labs(title = "Histogram",
             subtitle = paste("Distribution of", x_var),
             y = "Frequency") +
        base_theme
    },
    
    "boxplot" = {
      req(y_var)
      p <- ggplot(data, aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var)))
      if (!is.null(color_aes)) {
        p <- p + aes(fill = !!color_aes)
      }
      p + geom_boxplot(alpha = 0.8) +
        labs(title = "Box Plot",
             subtitle = paste("Distribution of", y_var, "by", x_var)) +
        base_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },
    
    "density" = {
      p <- ggplot(data, aes(x = !!rlang::sym(x_var)))
      if (!is.null(color_aes)) {
        p <- p + aes(fill = !!color_aes)
        p + geom_density(alpha = 0.6)
      } else {
        p + geom_density(alpha = 0.8, fill = "steelblue")
      }
      p + labs(title = "Density Plot",
               subtitle = paste("Density distribution of", x_var),
               y = "Density") +
        base_theme
    }
  )
}

#' Create Empty Plot
#' 
#' Helper function to create an empty plot with a message
#' 
#' @param message Character string with the message to display
#' @return ggplot object
create_empty_plot <- function(message) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = message, size = 6, color = "gray60") +
    theme_void() +
    theme(panel.background = element_rect(fill = "white", color = "gray90"))
}

#' Create Error Plot
#' 
#' Helper function to create a plot displaying error information
#' 
#' @param error_msg Character string with the error message
#' @return ggplot object
create_error_plot <- function(error_msg) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.6, 
             label = "Chart Error", 
             size = 8, color = "red", fontface = "bold") +
    annotate("text", x = 0.5, y = 0.4, 
             label = paste("Error:", error_msg), 
             size = 4, color = "darkred") +
    theme_void() +
    theme(panel.background = element_rect(fill = "#fff5f5", color = "red"))
}
