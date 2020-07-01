library(shiny)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(DT)
body_colwise <- dashboardBody(
  tabsetPanel(
    tabPanel("General overview",
             fluidRow(
               tabBox(
                 title = "Heatmap of COVID 19 in US",
                 id = "tabset1", 
                 height = "450",
                 width = 7,
                 tabPanel("Distribution of cases", plotlyOutput("heatmap_cases")),
                 tabPanel("Distribution of deaths", plotlyOutput("heatmap_deaths"))
                ),
               tabBox(
                 title = "State ranking",
                 id = "tabset2", 
                 height = "450",
                 width = 5,
                 tabPanel("Sort by positive sases", style = "overflow-y:scroll", plotlyOutput('state_cases_barplot')),
                 tabPanel("Sort by Deaths", style = "overflow-y:scroll", plotlyOutput('state_deaths_barplot'))
                )
              )
            ),
    tabPanel("State ranking",
             fluidRow(tabBox(
               title = "Time-series data",
               id = "tabset2", 
               height = "200",
               width = 6,
               tabPanel("Time-series of new cases", plotlyOutput("cases_timeseries")),
               tabPanel("Time-series of new deaths", plotlyOutput("deaths_timeseries"))
                  )
                )
              ),
    tabPanel("State data")
  )
)
ui <- dashboardPage(
  
  dashboardHeader(title = "Summary of COVID 19 in United States"),
  dashboardSidebar(disable = TRUE),
  body_colwise
)
