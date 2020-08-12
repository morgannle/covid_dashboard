options(shiny.sanitize.errors = FALSE)

wd = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard"
#wd = "/srv/shiny-server/myapp/"
setwd(wd)
source("functions.R")

server <- function(input, output, session) {
  
  covid19_county_data = read.csv(county_file_url,
                                 stringsAsFactors = FALSE, 
                                 header = TRUE) 
  covid19_nation_data = read.csv(us_file_url, 
                                 stringsAsFactors = FALSE, 
                                 header = TRUE)
  fatality_by_gender = read.csv(fatality_by_gender_url,
                                stringsAsFactors = FALSE, 
                                header = TRUE)
  growth_by_county = read.csv(growth_by_county_url,
                              stringsAsFactors = FALSE, 
                              row.names = NULL,
                              header = TRUE)
  
  covid19_county_data$date = as.Date(covid19_county_data$date)
  covid19_nation_data$date = as.Date(covid19_nation_data$date)
  
  
  covid19_national_timeseries = diff(covid19_nation_data)
  covid19_national_timeseries = replaceNA(covid19_national_timeseries)
  
 
  #fatality by gender for selected state
  fatality_by_gender_data = fatality_by_gender[fatality_by_gender$state == "United States", ] 
  
  pie_chart_fatality_by_gender = by_gender(fatality_by_gender_data)
  bar_plot_fatality_by_gender = by_age(fatality_by_gender_data)
  
  output$national_cases_timeseries = renderPlotly(
    plot_case(covid19_national_timeseries)
  )
  output$national_deaths_timeseries = renderPlotly(
    plot_death(covid19_national_timeseries)
  )
  
  
  output$national_heatmap = renderLeaflet(plot_growth(growth_by_county))
  
  output$valuebox_national_total_case = renderValueBox(
    valueBox(total_case_valuebox(covid19_nation_data), 
             width = 2, 
             subtitle = "National Total Cases",
             color = I("navy")
    )
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
  
  output$pie_chart = renderPlotly(
    pie_chart_fatality_by_gender
  )
  output$bar_plot = renderPlotly(
    bar_plot_fatality_by_gender
  )
}