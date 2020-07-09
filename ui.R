library(shiny)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(DT)
body_colwise <- dashboardBody(
  fluidRow(
    valueBox(
      value = 2,
      subtitle = "Total Cases",
      width = 3
    ),
    valueBox(
      value = 5,
      subtitle = "Total Fatality",
      width = 3
    ),
    valueBox(
      value = 6,
      subtitle = "New Cases",
      width = 3
    ),
    valueBox(
      value = 2,
      subtitle = "New Fatality",
      width = 3
    )
  ),
  
  fluidRow(
    tabBox(
      title = "Heatmap",
      width = 8,
      height = '500',
      tabPanel(
        "Cases",
        leafletOutput("heatmap_cases")
      ),
      tabPanel(
        "Fatality",
        leafletOutput("heatmap_deaths")
      )
    ),
    tabBox(
      title = "State Data",
      width = 4,
      height = '500',
      tabPanel(
        "Cases",
        style = 'overflow-y: scroll',
        plotlyOutput("state_cases_barplot")
      ),
      tabPanel(
        "Fatality",
        style = 'overflow-y: scroll',
        plotlyOutput("state_deaths_barplot")
      )
    )
  ),
  
  fluidRow(
    tabBox(
      title = "Time-series Data",
      width = 6,
      tabPanel(
        "New Cases",
        plotlyOutput("cases_timeseries")
      ),
      tabPanel(
        "New Fatality",
        plotlyOutput("deaths_timeseries")
      )
    ),
    box(
      width = 6,
      title = "Fatality Demographic"
    )
  )
)

ui <- dashboardPage(
  
  dashboardHeader(title = "Summary of COVID 19 in United States"),
  dashboardSidebar(disable = TRUE),
  body_colwise
)
