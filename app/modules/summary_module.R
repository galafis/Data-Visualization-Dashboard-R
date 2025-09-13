# Summary Module for Data Visualization Dashboard
# Author: galafis
# Description: Provides comprehensive data summary and statistics functionality
# Date: September 2025

library(DT)
library(dplyr)
library(plotly)

#' Summary Module UI
#' 
#' Creates the user interface for the summary module
#' @param id Character string. The module's namespace ID
#' @return A fluidRow containing summary display components
#' @export
summaryModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      12,
      h3("Data Summary & Statistics", class = "text-primary"),
      hr(),
      
      # Control panel
      fluidRow(
        column(
          6,
          selectInput(
            ns("summary_type"),
            "Summary Type:",
            choices = list(
              "Basic Summary" = "basic",
              "Detailed Statistics" = "detailed",
              "Data Structure" = "structure",
              "Missing Values" = "missing"
            ),
            selected = "basic"
          )
        ),
        column(
          6,
          conditionalPanel(
            condition = "input.summary_type == 'detailed'",
            ns = ns,
            checkboxInput(
              ns("include_plots"),
              "Include Distribution Plots",
              value = TRUE
            )
          )
        )
      ),
      
      hr(),
      
      # Summary output tabs
      tabsetPanel(
        id = ns("summary_tabs"),
        
        tabPanel(
          "Summary",
          br(),
          verbatimTextOutput(ns("summary_output")),
          conditionalPanel(
            condition = "input.summary_type == 'detailed' && input.include_plots",
            ns = ns,
            br(),
            h4("Distribution Plots"),
            plotlyOutput(ns("distribution_plots"), height = "400px")
          )
        ),
        
        tabPanel(
          "Data Table",
          br(),
          DT::dataTableOutput(ns("data_table"))
        ),
        
        tabPanel(
          "Variable Info",
          br(),
          DT::dataTableOutput(ns("variable_info"))
        )
      )
    )
  )
}

#' Summary Module Server
#' 
#' Provides server-side logic for data summary and statistics
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param data Reactive expression containing the data to summarize
#' @return None (generates outputs)
#' @export
summaryModule <- function(input, output, session, data) {
  
  # Reactive summary based on selected type
  summary_data <- reactive({
    req(data())
    
    df <- data()
    
    switch(input$summary_type,
      "basic" = {
        capture.output(summary(df), type = "output")
      },
      "detailed" = {
        # Detailed statistics for numeric columns
        numeric_cols <- sapply(df, is.numeric)
        
        if (any(numeric_cols)) {
          numeric_summary <- df %>%
            select(where(is.numeric)) %>%
            summarise_all(list(
              Count = ~length(.),
              Mean = ~round(mean(., na.rm = TRUE), 3),
              Median = ~round(median(., na.rm = TRUE), 3),
              SD = ~round(sd(., na.rm = TRUE), 3),
              Min = ~min(., na.rm = TRUE),
              Max = ~max(., na.rm = TRUE),
              Missing = ~sum(is.na(.))
            )) %>%
            pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
            separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
            pivot_wider(names_from = Statistic, values_from = Value)
          
          capture.output(print(numeric_summary), type = "output")
        } else {
          "No numeric columns found for detailed statistics."
        }
      },
      "structure" = {
        capture.output(str(df), type = "output")
      },
      "missing" = {
        missing_summary <- df %>%
          summarise_all(~sum(is.na(.))) %>%
          pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
          mutate(
            Total_Count = nrow(df),
            Missing_Percentage = round((Missing_Count / Total_Count) * 100, 2)
          ) %>%
          arrange(desc(Missing_Count))
        
        capture.output(print(missing_summary), type = "output")
      }
    )
  })
  
  # Summary output
  output$summary_output <- renderText({
    if(!is.null(data())) {
      paste(summary_data(), collapse = "\n")
    } else {
      "No data available. Please upload or load data first."
    }
  })
  
  # Distribution plots for numeric variables
  output$distribution_plots <- renderPlotly({
    req(data(), input$summary_type == "detailed", input$include_plots)
    
    df <- data()
    numeric_cols <- sapply(df, is.numeric)
    
    if (any(numeric_cols)) {
      numeric_data <- df %>% select(where(is.numeric))
      
      # Create histograms for up to 6 numeric variables
      plot_vars <- names(numeric_data)[1:min(6, ncol(numeric_data))]
      
      plot_list <- list()
      
      for (i in seq_along(plot_vars)) {
        var_name <- plot_vars[i]
        
        p <- plot_ly(
          x = ~numeric_data[[var_name]],
          type = "histogram",
          name = var_name,
          showlegend = FALSE
        ) %>%
          layout(
            title = list(text = var_name, font = list(size = 12)),
            xaxis = list(title = var_name),
            yaxis = list(title = "Frequency")
          )
        
        plot_list[[i]] <- p
      }
      
      # Arrange plots in a grid
      if (length(plot_list) == 1) {
        plot_list[[1]]
      } else {
        subplot(plot_list, nrows = ceiling(length(plot_list)/2), shareY = FALSE, titleX = TRUE) %>%
          layout(title = "Distribution of Numeric Variables")
      }
    } else {
      # Empty plot if no numeric data
      plot_ly() %>%
        layout(
          title = "No numeric variables available for plotting",
          xaxis = list(showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE)
        )
    }
  })
  
  # Data table output
  output$data_table <- DT::renderDataTable({
    req(data())
    
    DT::datatable(
      data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = "top",
      class = 'cell-border stripe'
    )
  })
  
  # Variable information table
  output$variable_info <- DT::renderDataTable({
    req(data())
    
    df <- data()
    
    var_info <- data.frame(
      Variable = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Percentage = round(sapply(df, function(x) sum(is.na(x))/length(x) * 100), 2),
      Unique_Values = sapply(df, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    # Add sample values for character/factor columns
    var_info$Sample_Values <- sapply(names(df), function(col_name) {
      col_data <- df[[col_name]]
      if (is.character(col_data) || is.factor(col_data)) {
        unique_vals <- unique(col_data)[!is.na(unique(col_data))]
        if (length(unique_vals) > 0) {
          paste(head(unique_vals, 3), collapse = ", ")
        } else {
          "All NA"
        }
      } else if (is.numeric(col_data)) {
        range_vals <- range(col_data, na.rm = TRUE)
        if (all(is.finite(range_vals))) {
          paste("Range:", round(range_vals[1], 2), "to", round(range_vals[2], 2))
        } else {
          "All NA"
        }
      } else {
        "Mixed/Other"
      }
    })
    
    DT::datatable(
      var_info,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      DT::formatStyle(
        "Missing_Percentage",
        backgroundColor = DT::styleInterval(
          cuts = c(0, 10, 25, 50),
          values = c("lightgreen", "yellow", "orange", "red", "darkred")
        )
      )
  })
}
