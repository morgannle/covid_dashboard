library(shiny)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(DT)
body_colwise <- dashboardBody(
  fluidRow(
        box(
          width = 3,
          height = 120,
          selectInput("select", 
                      label = h3("Select a state: "), 
                      choices = state.name, 
                      selected = "Missouri")
          ),
        box(
          width = 9,
          height = 120,
          "- This dashboard would not be possible without 
          data from", strong("The New York Times"), 
          "based on reports from state and local health agencies, 
          and", strong("Centers for Disease Control and Prevention (CDC)"),".",
          br(),
          "-",
          strong("The New York Times"),
          "also has their own reporting platform, 
          you can access them at", 
          tags$a("this link.", href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html"),
          br(),
          "- Data powering this dashboard can be found",
          tags$a("here", href = "https://github.com/nytimes/covid-19-data"),
          "and",
          tags$a("here.", href = "https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku"),
          br(),
          "- This dashboard reports the situation of COVID 19 in", strong("United States"), "as of",
          strong(paste0(Sys.Date() - 1)),"."
          )
        ),
  
  fluidRow(
    valueBoxOutput("valuebox_national_total_case", width = 3),
    valueBoxOutput("valuebox_national_new_case", width = 3),
    valueBoxOutput("valuebox_national_total_death", width = 3),
    valueBoxOutput("valuebox_national_new_death", width = 3)
  ),
  
  fluidRow(
    box(
      width = 12,
      title = "7-day Average Cases Growth (percent) ",
      height = '500',
      shinycssloaders::withSpinner(leafletOutput("national_heatmap"))
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
      shinycssloaders::withSpinner(leafletOutput("heatmap"))
      ),
    tabBox(
      title = "State Data",
      width = 4,
      height = '500',
      tabPanel(
        "Cases",
        style = 'overflow-y: scroll',
        shinycssloaders::withSpinner(plotlyOutput("state_cases_barplot"))
      ),
      tabPanel(
        "Fatality",
        style = 'overflow-y: scroll',
        shinycssloaders::withSpinner(plotlyOutput("state_deaths_barplot"))
      )
    )
  ),
  
  fluidRow(
    tabBox(
      width = 6,
      tabPanel(
        "State New Cases",
        shinycssloaders::withSpinner(plotlyOutput("cases_timeseries"))
      ),
      tabPanel(
        "State New Fatality",
        shinycssloaders::withSpinner(plotlyOutput("deaths_timeseries"))
      ),
      tabPanel(
        "National New Cases",
        shinycssloaders::withSpinner(plotlyOutput("national_cases_timeseries"))
      ),
      tabPanel(
        "National New Fatality",
        shinycssloaders::withSpinner(plotlyOutput("national_deaths_timeseries"))
      )
    ),
    tabBox(
      width = 6,
      title = "Fatality Demographic",
      tabPanel(
        "By Gender",
        shinycssloaders::withSpinner(plotlyOutput("pie_chart"))
        ),
      tabPanel(
        "By Age Group",
        shinycssloaders::withSpinner(plotlyOutput("bar_plot"))
        )
      )
    )
  )

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  body_colwise
)
