# ==============================================================================
# DATA VISUALIZATION DASHBOARD - USER INTERFACE
# ==============================================================================
# Author: Gabriel Demetrios Lafis
# Description: Professional Shiny application UI with modular architecture
# Version: 2.0
# Last Modified: 2025-09-13
# ==============================================================================

# ======================
# DEPENDENCIES & LIBRARIES
# ======================

# Core Shiny libraries
library(shiny)
library(shinydashboard)

# Visualization libraries
library(plotly)
library(DT)
library(leaflet)

# UI enhancement libraries
library(shinyWidgets)
library(shinycssloaders)

# ======================
# MODULE IMPORTS
# ======================

# Data processing module
source("modules/data_module.R")

# Chart generation module
source("modules/chart_module.R")

# Filtering functionality module
source("modules/filter_module.R")

# Export functionality module
source("modules/export_module.R")

# ==============================================================================
# DASHBOARD HEADER CONFIGURATION
# ==============================================================================

header <- dashboardHeader(
  title = "Data Visualization Dashboard",
  
  # External link to GitHub repository
  tags$li(
    class = "dropdown",
    tags$a(
      href = "https://github.com/galafis/Data-Visualization-Dashboard-R",
      target = "_blank",
      icon("github"),
      "GitHub Repository",
      title = "View source code on GitHub"
    )
  )
)

# ==============================================================================
# DASHBOARD SIDEBAR CONFIGURATION
# ==============================================================================

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    
    # Main navigation menu items
    menuItem(
      text = "Overview",
      tabName = "overview",
      icon = icon("dashboard")
    ),
    
    menuItem(
      text = "Analytics",
      tabName = "analytics",
      icon = icon("chart-line")
    ),
    
    menuItem(
      text = "Geographic Maps",
      tabName = "maps",
      icon = icon("map-marked-alt")
    ),
    
    menuItem(
      text = "Reports",
      tabName = "reports",
      icon = icon("file-alt")
    ),
    
    menuItem(
      text = "Settings",
      tabName = "settings",
      icon = icon("cog")
    )
  )
)

# ==============================================================================
# DASHBOARD BODY CONFIGURATION
# ==============================================================================

body <- dashboardBody(
  # Custom CSS and JavaScript files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$script(src = "js/custom.js"),
    
    # Meta tags for responsive design
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  # Tab items container
  tabItems(
    
    # ==========================================================================
    # OVERVIEW TAB - Key Metrics and Summary
    # ==========================================================================
    
    tabItem(
      tabName = "overview",
      
      # Key Performance Indicators row
      fluidRow(
        valueBoxOutput(
          outputId = "total_records",
          width = 4
        ),
        valueBoxOutput(
          outputId = "avg_value",
          width = 4
        ),
        valueBoxOutput(
          outputId = "growth_rate",
          width = 4
        )
      ),
      
      # Main visualization row
      fluidRow(
        # Trend analysis visualization
        box(
          title = "Trend Analysis",
          status = "primary",
          solidHeader = TRUE,
          width = 8,
          collapsible = TRUE,
          withSpinner(
            plotlyOutput(
              outputId = "trend_plot",
              height = "400px"
            ),
            type = 6,
            color = "#0073b7"
          )
        ),
        
        # Distribution chart
        box(
          title = "Data Distribution",
          status = "warning",
          solidHeader = TRUE,
          width = 4,
          collapsible = TRUE,
          withSpinner(
            plotOutput(
              outputId = "distribution_plot",
              height = "400px"
            ),
            type = 6,
            color = "#f39c12"
          )
        )
      ),
      
      # Data table row
      fluidRow(
        box(
          title = "Data Explorer",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          DT::dataTableOutput(
            outputId = "data_table"
          )
        )
      )
    ),
    
    # ==========================================================================
    # ANALYTICS TAB - Advanced Analysis Tools
    # ==========================================================================
    
    tabItem(
      tabName = "analytics",
      
      # Filters and main chart row
      fluidRow(
        # Filter controls panel
        box(
          title = "Analysis Filters",
          status = "primary",
          solidHeader = TRUE,
          width = 3,
          collapsible = TRUE,
          filterUI(
            id = "filters"
          )
        ),
        
        # Interactive scatter plot
        box(
          title = "Interactive Scatter Plot",
          status = "success",
          solidHeader = TRUE,
          width = 9,
          collapsible = TRUE,
          chartUI(
            id = "scatter_chart"
          )
        )
      ),
      
      # Statistical analysis row
      fluidRow(
        # Correlation heatmap
        box(
          title = "Correlation Analysis",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          withSpinner(
            plotlyOutput(
              outputId = "correlation_heatmap",
              height = "400px"
            ),
            type = 6,
            color = "#f39c12"
          )
        ),
        
        # Statistical summary
        box(
          title = "Statistical Summary",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          collapsible = TRUE,
          verbatimTextOutput(
            outputId = "statistical_summary"
          )
        )
      )
    ),
    
    # ==========================================================================
    # MAPS TAB - Geographic Visualization
    # ==========================================================================
    
    tabItem(
      tabName = "maps",
      
      # Interactive map container
      fluidRow(
        box(
          title = "Geographic Data Visualization",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = "650px",
          collapsible = TRUE,
          withSpinner(
            leafletOutput(
              outputId = "interactive_map",
              height = "600px"
            ),
            type = 6,
            color = "#0073b7"
          )
        )
      )
    ),
    
    # ==========================================================================
    # REPORTS TAB - Export and Reporting
    # ==========================================================================
    
    tabItem(
      tabName = "reports",
      
      # Report generation and preview
      fluidRow(
        # Report generation controls
        box(
          title = "Report Generator",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          collapsible = TRUE,
          exportUI(
            id = "export_module"
          )
        ),
        
        # Report preview
        box(
          title = "Report Preview",
          status = "info",
          solidHeader = TRUE,
          width = 8,
          collapsible = TRUE,
          htmlOutput(
            outputId = "report_preview"
          )
        )
      )
    ),
    
    # ==========================================================================
    # SETTINGS TAB - Application Configuration
    # ==========================================================================
    
    tabItem(
      tabName = "settings",
      
      # Application settings panel
      fluidRow(
        box(
          title = "Application Configuration",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          # Theme settings section
          h4(
            "Theme Configuration",
            icon("palette")
          ),
          radioButtons(
            inputId = "theme",
            label = "Select Application Theme:",
            choices = c(
              "Default Blue" = "default",
              "Dark Mode" = "dark",
              "Light Mode" = "light"
            ),
            selected = "default",
            inline = TRUE
          ),
          
          hr(),
          
          # Data refresh settings section
          h4(
            "Data Refresh Settings",
            icon("sync-alt")
          ),
          numericInput(
            inputId = "refresh_interval",
            label = "Auto-refresh interval (seconds):",
            value = 30,
            min = 10,
            max = 300,
            step = 5
          ),
          
          switchInput(
            inputId = "auto_refresh",
            label = "Enable automatic data refresh",
            value = FALSE,
            onLabel = "ON",
            offLabel = "OFF",
            onStatus = "success",
            offStatus = "danger"
          ),
          
          hr(),
          
          # Performance settings section
          h4(
            "Performance Settings",
            icon("tachometer-alt")
          ),
          
          sliderInput(
            inputId = "max_rows",
            label = "Maximum rows to display in tables:",
            min = 100,
            max = 10000,
            value = 1000,
            step = 100
          ),
          
          switchInput(
            inputId = "enable_animations",
            label = "Enable plot animations",
            value = TRUE,
            onLabel = "ON",
            offLabel = "OFF",
            onStatus = "success",
            offStatus = "danger"
          )
        )
      )
    )
  )
)

# ==============================================================================
# MAIN UI ASSEMBLY
# ==============================================================================

#' Main Dashboard UI
#'
#' Assembles the complete Shiny dashboard UI with header, sidebar, and body
#'
#' @return dashboardPage object containing the complete UI
#' @export

ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue",
  title = "Data Visualization Dashboard"
)

# ==============================================================================
# END OF FILE
# ==============================================================================
