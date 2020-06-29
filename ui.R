library(shiny)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
ui <- fluidPage(
  
  # App title ----
    # Main panel for displaying outputs ----
      navbarPage("COVID-19 DASHBOARD",
                 tabPanel("Component 1",
                          {
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                
                                # search bar
                                numericInput(inputId = "n", 
                                             label = "Length of simulation (day)", 
                                             value = 365, 
                                             min = 0)),
                              mainPanel()
                            )
                          }),
                 tabPanel("Component 2"),
                 navbarMenu("NATIONAL DATA",
                            tabPanel("HEATMAP",
                                     plotlyOutput("heatmap")),
                            tabPanel("TIME-SERIES",
                                     plotlyOutput('nation_timeseries')))
  )
)
