library(shiny)
library(plotly)
library(shinyWidgets)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Outbreak Simulation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Plot
      # plotlyOutput("summary")
      tabsetPanel(
      )
    )
  )
)