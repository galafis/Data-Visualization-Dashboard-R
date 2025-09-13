# Export Module for Data Visualization Dashboard
# Handles data export functionality (PDF, HTML, Excel, CSV)
# Author: Gabriel Demetrios Lafis
# Created: 2024

library(shiny)
library(DT)
library(openxlsx)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(plotly)

# Export Module UI
exportUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    wellPanel(
      h4("Export Options", class = "text-primary"),
      
      # Export format selection
      radioButtons(ns("export_format"), 
                   "Export Format:",
                   choices = list(
                     "CSV Data" = "csv",
                     "Excel File" = "xlsx",
                     "PDF Report" = "pdf",
                     "HTML Report" = "html"
                   ),
                   selected = "csv"),
      
      # Report options (only shown for PDF/HTML)
      conditionalPanel(
        condition = "input.export_format == 'pdf' || input.export_format == 'html'",
        ns = ns,
        
        checkboxGroupInput(ns("report_sections"),
                          "Include in Report:",
                          choices = list(
                            "Summary Statistics" = "summary",
                            "Data Table" = "table",
                            "Charts" = "charts",
                            "Analysis" = "analysis"
                          ),
                          selected = c("summary", "charts"))
      ),
      
      # File name input
      textInput(ns("filename"), 
                "File Name:", 
                value = paste0("dashboard_export_", Sys.Date())),
      
      # Export button
      downloadButton(ns("download_btn"), 
                     "Download", 
                     class = "btn btn-primary btn-block",
                     icon = icon("download")),
      
      br(), br(),
      
      # Quick export buttons
      h5("Quick Export:"),
      fluidRow(
        column(6, 
               downloadButton(ns("quick_csv"), 
                             "CSV", 
                             class = "btn btn-outline-secondary btn-sm btn-block")),
        column(6, 
               downloadButton(ns("quick_excel"), 
                             "Excel", 
                             class = "btn btn-outline-secondary btn-sm btn-block"))
      )
    )
  )
}

# Export Module Server
exportServer <- function(id, data, charts = NULL, summary_stats = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Main download handler
    output$download_btn <- downloadHandler(
      filename = function() {
        ext <- switch(input$export_format,
                     "csv" = ".csv",
                     "xlsx" = ".xlsx",
                     "pdf" = ".pdf",
                     "html" = ".html")
        paste0(input$filename, ext)
      },
      
      content = function(file) {
        switch(input$export_format,
               "csv" = export_csv(data(), file),
               "xlsx" = export_excel(data(), file),
               "pdf" = export_pdf(data(), charts, summary_stats, input$report_sections, file),
               "html" = export_html(data(), charts, summary_stats, input$report_sections, file)
        )
      }
    )
    
    # Quick CSV export
    output$quick_csv <- downloadHandler(
      filename = function() {
        paste0("data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        export_csv(data(), file)
      }
    )
    
    # Quick Excel export
    output$quick_excel <- downloadHandler(
      filename = function() {
        paste0("data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        export_excel(data(), file)
      }
    )
  })
}

# Helper Functions

# Export to CSV
export_csv <- function(data, file) {
  write.csv(data, file, row.names = FALSE)
}

# Export to Excel
export_excel <- function(data, file) {
  # Create workbook
  wb <- createWorkbook()
  
  # Add data sheet
  addWorksheet(wb, "Data")
  writeData(wb, "Data", data)
  
  # Style headers
  headerStyle <- createStyle(
    textDecoration = "bold",
    fgFill = "#3498db",
    fontColour = "white",
    border = "bottom"
  )
  
  addStyle(wb, "Data", headerStyle, rows = 1, cols = 1:ncol(data))
  
  # Auto-adjust column widths
  setColWidths(wb, "Data", cols = 1:ncol(data), widths = "auto")
  
  # Add summary sheet if numeric data exists
  numeric_cols <- sapply(data, is.numeric)
  if (any(numeric_cols)) {
    addWorksheet(wb, "Summary")
    
    summary_data <- data[, numeric_cols, drop = FALSE] %>%
      summarise_all(list(
        Mean = ~round(mean(., na.rm = TRUE), 2),
        Median = ~round(median(., na.rm = TRUE), 2),
        SD = ~round(sd(., na.rm = TRUE), 2),
        Min = ~min(., na.rm = TRUE),
        Max = ~max(., na.rm = TRUE)
      ))
    
    writeData(wb, "Summary", summary_data)
    addStyle(wb, "Summary", headerStyle, rows = 1, cols = 1:ncol(summary_data))
  }
  
  saveWorkbook(wb, file)
}

# Export to PDF Report
export_pdf <- function(data, charts, summary_stats, sections, file) {
  # Create temporary Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")
  
  # Create report content
  report_content <- create_report_content(data, charts, summary_stats, sections)
  
  # Write to temporary file
  writeLines(report_content, temp_rmd)
  
  # Render to PDF
  rmarkdown::render(
    temp_rmd,
    output_format = "pdf_document",
    output_file = file,
    quiet = TRUE
  )
}

# Export to HTML Report
export_html <- function(data, charts, summary_stats, sections, file) {
  # Create temporary Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")
  
  # Create report content
  report_content <- create_report_content(data, charts, summary_stats, sections)
  
  # Write to temporary file
  writeLines(report_content, temp_rmd)
  
  # Render to HTML
  rmarkdown::render(
    temp_rmd,
    output_format = "html_document",
    output_file = file,
    quiet = TRUE
  )
}

# Create report content
create_report_content <- function(data, charts, summary_stats, sections) {
  
  header <- c(
    "---",
    "title: 'Dashboard Export Report'",
    "date: '`r Sys.Date()`'",
    "output:",
    "  html_document:",
    "    theme: flatly",
    "    toc: true",
    "    toc_float: true",
    "  pdf_document:",
    "    toc: true",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(ggplot2)",
    "library(plotly)",
    "library(DT)",
    "library(knitr)",
    "```",
    "",
    "## Dashboard Export Report",
    "",
    paste("Generated on:", Sys.time()),
    ""
  )
  
  content <- header
  
  # Add summary section
  if ("summary" %in% sections && !is.null(summary_stats)) {
    summary_section <- c(
      "## Summary Statistics",
      "",
      "```{r summary}",
      "# Display summary statistics",
      "summary_data <- data.frame(",
      paste0("  Total_Records = ", nrow(data), ","),
      paste0("  Variables = ", ncol(data)),
      ")",
      "kable(summary_data, caption = 'Dataset Overview')",
      "```",
      ""
    )
    content <- c(content, summary_section)
  }
  
  # Add data table section
  if ("table" %in% sections) {
    table_section <- c(
      "## Data Table",
      "",
      "```{r table}",
      "# Display data table",
      "DT::datatable(head(data, 100), options = list(scrollX = TRUE, pageLength = 10))",
      "```",
      ""
    )
    content <- c(content, table_section)
  }
  
  # Add charts section
  if ("charts" %in% sections && !is.null(charts)) {
    charts_section <- c(
      "## Visualizations",
      "",
      "```{r charts, fig.width=10, fig.height=6}",
      "# Display charts",
      "if(exists('charts') && !is.null(charts)) {",
      "  print(charts)",
      "}",
      "```",
      ""
    )
    content <- c(content, charts_section)
  }
  
  # Add analysis section
  if ("analysis" %in% sections) {
    analysis_section <- c(
      "## Statistical Analysis",
      "",
      "```{r analysis}",
      "# Perform basic analysis",
      "numeric_cols <- sapply(data, is.numeric)",
      "if(any(numeric_cols)) {",
      "  analysis_data <- summary(data[, numeric_cols, drop = FALSE])",
      "  print(analysis_data)",
      "}",
      "```",
      ""
    )
    content <- c(content, analysis_section)
  }
  
  return(content)
}

# Export utility function for standalone use
export_dashboard_data <- function(data, format = "csv", filename = NULL) {
  if (is.null(filename)) {
    filename <- paste0("dashboard_export_", Sys.Date())
  }
  
  switch(format,
         "csv" = {
           file_path <- paste0(filename, ".csv")
           write.csv(data, file_path, row.names = FALSE)
           message("Data exported to: ", file_path)
         },
         "xlsx" = {
           file_path <- paste0(filename, ".xlsx")
           export_excel(data, file_path)
           message("Data exported to: ", file_path)
         },
         stop("Unsupported format. Use 'csv' or 'xlsx'")
  )
}

# Example usage:
# In your main Shiny app:
# 
# # In UI:
# exportUI("export")
# 
# # In Server:
# exportServer("export", 
#              data = reactive(your_data), 
#              charts = reactive(your_charts),
#              summary_stats = reactive(your_summary))
