# Data Visualization Dashboard - UI
# Gabriel Demetrios Lafis
# Professional Shiny Application UI

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(leaflet)
library(shinyWidgets)
library(shinycssloaders)

# Source modules
source("modules/data_module.R")
source("modules/chart_module.R")
source("modules/filter_module.R")
source("modules/export_module.R")

# Header
header <- dashboardHeader(
  title = "Data Visualization Dashboard",
  tags$li(
    class = "dropdown",
    tags$a(
      href = "https://github.com/galafis/Data-Visualization-Dashboard-R",
      target = "_blank",
      icon("github"),
      "GitHub"
    )
  )
)

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Analytics", tabName = "analytics", icon = icon("chart-line")),
    menuItem("Maps", tabName = "maps", icon = icon("map")),
    menuItem("Reports", tabName = "reports", icon = icon("file-alt")),
    menuItem("Settings", tabName = "settings", icon = icon("cog"))
  )
)

# Body
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$script(src = "js/custom.js")
  ),
  
  tabItems(
    # Overview Tab
    tabItem(
      tabName = "overview",
      fluidRow(
        valueBoxOutput("total_records"),
        valueBoxOutput("avg_value"),
        valueBoxOutput("growth_rate")
      ),
      fluidRow(
        box(
          title = "Trend Analysis", 
          status = "primary", 
          solidHeader = TRUE, 
          width = 8,
          withSpinner(plotlyOutput("trend_plot"))
        ),
        box(
          title = "Distribution", 
          status = "warning",
          solidHeader = TRUE, 
          width = 4,
          withSpinner(plotOutput("distribution_plot"))
        )
      ),
      fluidRow(
        box(
          title = "Data Table",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          DT::dataTableOutput("data_table")
        )
      )
    ),
    
    # Analytics Tab
    tabItem(
      tabName = "analytics",
      fluidRow(
        box(
          title = "Filters",
          status = "primary",
          solidHeader = TRUE,
          width = 3,
          filterUI("filters")
        ),
        box(
          title = "Interactive Scatter Plot",
          status = "success",
          solidHeader = TRUE,
          width = 9,
          chartUI("scatter_chart")
        )
      ),
      fluidRow(
        box(
          title = "Correlation Heatmap",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          withSpinner(plotlyOutput("correlation_heatmap"))
        ),
        box(
          title = "Statistical Summary",
          status = "info",
          solidHeader = TRUE,
          width = 6,
          verbatimTextOutput("statistical_summary")
        )
      )
    ),
    
    # Maps Tab
    tabItem(
      tabName = "maps",
      fluidRow(
        box(
          title = "Interactive Map",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          height = "600px",
          withSpinner(leafletOutput("interactive_map", height = "550px"))
        )
      )
    ),
    
    # Reports Tab
    tabItem(
      tabName = "reports",
      fluidRow(
        box(
          title = "Generate Reports",
          status = "primary",
          solidHeader = TRUE,
          width = 4,
          exportUI("export_module")
        ),
        box(
          title = "Report Preview",
          status = "info",
          solidHeader = TRUE,
          width = 8,
          htmlOutput("report_preview")
        )
      )
    ),
    
    # Settings Tab
    tabItem(
      tabName = "settings",
      fluidRow(
        box(
          title = "Application Settings",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          h4("Theme Settings"),
          radioButtons(
            "theme",
            "Select Theme:",
            choices = c("Default" = "default", "Dark" = "dark", "Light" = "light"),
            selected = "default"
          ),
          h4("Data Settings"),
          numericInput(
            "refresh_interval",
            "Auto-refresh interval (seconds):",
            value = 30,
            min = 10,
            max = 300
          ),
          switchInput(
            "auto_refresh",
            "Enable auto-refresh",
            value = FALSE
          )
        )
      )
    )
  )
)

# UI
ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue"
)
