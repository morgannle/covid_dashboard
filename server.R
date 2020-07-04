wd = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard"
setwd(wd)
source("functions.R")

library('readr')
library('dplyr')
library('plotly')
library('zoo')
library('shinydashboard')

county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
us_file_url     = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
json_url        = "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"

server <- function(input, output) {
  
  county_geo_fips = rjson::fromJSON(file = json_url)
  
  covid19_county_data = read_csv(
    url(county_file_url)
  )
  
  covid19_county_data$fips = as.character(covid19_county_data$fips)
  
  #covid19 county data todate
  todate_covid19_county_data = covid19_county_data[covid19_county_data$date == Sys.Date()-2, ] 
  
  covid19_nation_data = read_csv(
    url(us_file_url)
  )
  
  #this variable contains time series data of all state
  covid19_state_data = covid19_county_data %>%
    group_by(state, date) %>%
    summarise(cases = sum(cases),
              deaths = sum(deaths))
  
  #get state names and number of states
  state_name = unique(covid19_county_data$state)
  state_ID = seq(1:length(state_name))
  state = data.frame(state_name, state_ID)
  colnames(state) = c("Name", "ID")
  
  #number of states
  state_count = nrow(state)
  
  #each sub-object of these objects contains time series of each states
  covid19_day_to_day_state_data = list()

  for (i in 1:state_count){
    covid19_day_to_day_state_data[[i]] = covid19_state_data[covid19_state_data$state == state_name[i], ]
  }
  
  covid19_day_to_day_state_data = lapply(covid19_day_to_day_state_data, diff) #calculate new cases and deaths
  covid19_day_to_day_state_data = lapply(covid19_day_to_day_state_data, replaceNA) #replace NA with 0
  
  covid19_day_to_day_nation_data = diff(covid19_nation_data) #calculate new cases and deaths
  covid19_day_to_day_nation_data = replaceNA(covid19_day_to_day_nation_data) #replace NA with 0
  
  covid19_day_to_day_case_state_data = lapply(covid19_day_to_day_state_data, plot_case)
  covid19_day_to_day_death_state_data = lapply(covid19_day_to_day_state_data, plot_death)
  
  covid_19_day_to_day_case_nation_data = plot_case(covid19_day_to_day_nation_data)
  covid_19_day_to_day_death_nation_data = plot_death(covid19_day_to_day_nation_data)
  
  
  
  
  
  
  
  
  
  
  #get most recent data
  current_us_state_cases_deaths = us_state_cases_deaths[us_state_cases_deaths$date == Sys.Date()-2, ]
  
  
  
  
  
  
  
  #split the most current data into positive cases and deaths
  current_us_state_cases = current_us_state_cases_deaths[ ,c(1,3)]
  current_us_state_deaths = current_us_state_cases_deaths[,c(1,4)]
  
  #sorting
  current_us_state_cases$state = factor(current_us_state_cases$state,
                                        levels = unique(current_us_state_cases$state[order(current_us_state_cases$cases, decreasing = FALSE)]))
  current_us_state_deaths$state = factor(current_us_state_deaths$state,
                                        levels = unique(current_us_state_deaths$state[order(current_us_state_deaths$deaths, decreasing = FALSE)]))
  
  #barplot for cases
  current_us_state_cases_barplot = plot_ly(current_us_state_cases,
                                           y = ~state,
                                           x = ~cases,
                                           color = I("blue"),
                                           type = 'bar',
                                           orientation = 'h') %>%
                                              layout(
                                                      title = "COVID 19 Cases in each State",
                                                      xaxis=list(fixedrange=TRUE),
                                                      height = 1000,  
                                                      yaxis=list(fixedrange=TRUE)
                                                      ) %>%
                                              config(displayModeBar = FALSE)
 
  #barplot for deaths
  current_us_state_deaths_barplot = plot_ly(current_us_state_deaths,
                                           y = ~state,
                                           x = ~deaths,
                                           color = I("gray"),
                                           type = 'bar',
                                           orientation = 'h') %>%
                                              layout(
                                                      title = "COVID 19 Fatality in each State",
                                                      xaxis=list(fixedrange=TRUE),
                                                      height = 1000,
                                                      yaxis=list(fixedrange=TRUE)
                                                      ) %>%
                                              config(displayModeBar = FALSE)
  
  
  output$cases_timeseries = renderPlotly(us_cases_perday)
  output$deaths_timeseries = renderPlotly(us_deaths_perday)
  output$heatmap_cases = renderPlotly(heatmap_cases)
  output$heatmap_deaths = renderPlotly(heatmap_deaths)
  output$state_cases_barplot = renderPlotly(current_us_state_cases_barplot)
  output$state_deaths_barplot = renderPlotly(current_us_state_deaths_barplot)
}