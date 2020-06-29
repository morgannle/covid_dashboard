wd = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard"
setwd(wd)
source("functions.R")

library('readr')
library('dplyr')
library('plotly')
library('zoo')
library('shinydashboard')

county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
us_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
json_url = 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'

server <- function(input, output) {
  county = rjson::fromJSON(file = json_url)
  
  county_data = read_csv(url(county_file_url))
  county_data$fips = as.character(county_data$fips)
  
  current_county_data = county_data[county_data$date == Sys.Date()-2, ]
  
  us_data = read_csv(url(us_file_url))
  
  #this variable contains time series of all states
  us_cases_deaths = county_data %>%
    group_by(state, date) %>%
    summarise(cases = sum(cases),
              deaths = sum(deaths))
  #get state names and number of states
  state_name = unique(county_data$state)
  state_ID = seq(1:length(state_name))
  state = data.frame(state_name, state_ID)
  colnames(state) = c("Name", "ID")
  
  state_count = length(state_name)
  states_cases_deaths = list() #each sub-object of these objects contains time series of each states
  
  for (i in 1:state_count){
    states_cases_deaths[[i]] = us_cases_deaths[us_cases_deaths$state == state_name[i], ]
  }
  
  states_data = lapply(states_cases_deaths, diff) #calculate new cases and deaths
  states_data = lapply(states_data, replace_) #replace NA with 0
  
  us_data = diff(us_data)
  us_data = replace_(us_data)
  
  states_cases_perday = lapply(states_data, new_case)
  states_deaths_perday = lapply(states_data, new_deaths)
  states_deaths_perday = lapply(states_data, new_deaths)
  
  us_cases_perday = new_case(us_data)
  us_deaths_perday = new_deaths(us_data)
  us_perday = subplot(us_cases_perday,
                      us_deaths_perday,
                      nrows = 2,
                      shareX = TRUE,
                      titleY = TRUE)
  
  us_cases_perday_change = diff_percent(us_data)
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  heatmap = plot_ly()
  heatmap = heatmap %>% add_trace(
    type = "choropleth",
    geojson = county,
    locations = current_county_data$fips,
    z = current_county_data$cases,
    colorscale = "Reds",
    zmin = 0,
    zmax=  max(county_data$cases)*0.05,
    marker = list(line=list(
      width = 0.5)),
    hoverinfo = 'text',
    text = ~paste('</br> State: ', current_county_data$state,
                  '</br> County: ', current_county_data$county,
                  '</br> Number of cases: ', current_county_data$cases)
  )
  heatmap = heatmap %>% colorbar(title = "COVID 19 cases in US")
  heatmap = heatmap %>% layout(title = "2019 Corona Cases",
                               geo = g,
                               autosize = F,
                               width = 1000,
                               height = 650,
                               margin = m)
  
  output$nation_timeseries = renderPlotly(us_perday)
  output$heatmap = renderPlotly(heatmap)
}