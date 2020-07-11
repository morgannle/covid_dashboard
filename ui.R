library(shiny)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(DT)
body_colwise <- dashboardBody(
  
  fluidRow(
    column(
        width = 3,
        selectizeInput("select", 
                        label = h3("State: "), 
                        choices = NULL, 
                        selected = 14)
        )
    ),
  
  fluidRow(
    valueBoxOutput("valuebox_total_case"),
    valueBoxOutput("valuebox_total_death"),
  ),
  
  fluidRow(
      box(
      title = "Heatmap",
      width = 8,
      height = '500',
      leafletOutput("heatmap_cases")
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
