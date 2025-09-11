library(shiny)
library(shinydashboard)
dashboardPage(
  dashboardHeader(title = "Data Visualization Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Reports", tabName = "reports", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview",
        fluidRow(
          valueBoxOutput("total_records"),
          valueBoxOutput("avg_value"),
          valueBoxOutput("growth_rate")
        ),
        fluidRow(
          box(title = "Trend Analysis", status = "primary", width = 8, plotlyOutput("trend_plot")),
          box(title = "Distribution", status = "warning", width = 4, plotOutput("distribution_plot"))
        )
      ),
      tabItem("analytics", 
        DT::dataTableOutput("datatable")
      ),
      tabItem("reports", 
        h3("Relatório automático R Markdown gerado em 'reports/'")
      )
    )
  )
)
