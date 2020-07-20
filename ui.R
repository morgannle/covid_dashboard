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
        selectInput("select", 
                    label = h3("State: "), 
                    choices = state.name, 
                    selected = "Alabama")
        )
    ),
  
  fluidRow(
    valueBoxOutput("valuebox_total_case", width = 3),
    valueBoxOutput("valuebox_new_case", width = 3),
    valueBoxOutput("valuebox_total_death", width = 3),
    valueBoxOutput("valuebox_new_death", width = 3)
  ),
  
  fluidRow(
      box(
      title = "Heatmap",
      width = 8,
      height = '500',
      leafletOutput("heatmap")
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
    tabBox(
      width = 6,
      title = "Fatality Demographic",
      tabPanel(
        "By Gender",
        plotlyOutput("pie_chart")
        ),
      tabPanel(
        "By Age Group",
        plotlyOutput("bar_plot")
        ),
      tabPanel(
        "Comparison",
        plotlyOutput("compare")
        )
      )
    ),
  fluidRow(tableOutput("obs"))
  )

ui <- dashboardPage(
  
  dashboardHeader(title = "Summary of COVID 19 in United States"),
  dashboardSidebar(disable = TRUE),
  body_colwise
)
