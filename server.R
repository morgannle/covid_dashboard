library('shinydashboard')
options(shiny.sanitize.errors = FALSE)

server <- function(input, output, session) {

  output$national_cases_timeseries = renderPlotly()
  output$national_deaths_timeseries = renderPlotly()
  output$national_heatmap = renderLeaflet()
  output$pie_chart = renderPlotly()
  output$bar_plot = renderPlotly()
  
  output$valuebox_national_total_case = renderValueBox(
    valueBox(total_case_valuebox(covid19_nation_data), 
             width = 2, 
             subtitle = "National Total Cases",
             color = I("navy"))
  )
  output$valuebox_national_new_case = renderValueBox(
    valueBox(new_case_valuebox(covid19_nation_data), 
             width = 2, 
             subtitle = "National New Cases",
             color = I("navy")
    )
  )
  output$valuebox_national_total_death = renderValueBox(
    valueBox(total_death_valuebox(covid19_nation_data), 
             width = 2, 
             icon = icon("<abacus"),
             subtitle = "National Total Fatality",
             color = I("navy")
    )
  )
  output$valuebox_national_new_death = renderValueBox(
    valueBox(new_death_valuebox(covid19_nation_data), 
             width = 2, 
             subtitle = "National New Fatality",
             color = I("navy")
    )
  )
}