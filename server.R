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
  
  current_county_data = county_data[county_data$date == Sys.Date()-1, ] #data
  
  us_data = read_csv(url(us_file_url))
  
  #this variable contains time series data of all state
  us_state_cases_deaths = county_data %>%
                            group_by(state, date) %>%
                            summarise(cases = sum(cases),
                            deaths = sum(deaths))
  
  #get state names and number of states
  state_name = unique(county_data$state)
  state_ID = seq(1:length(state_name))
  state = data.frame(state_name, state_ID)
  colnames(state) = c("Name", "ID")
  
  #number of states
  state_count = length(state_name)
  
  #each sub-object of these objects contains time series of each states
  county_new_cases_deaths_sep = list() 

  for (i in 1:state_count){
    county_new_cases_deaths_sep[[i]] = us_state_cases_deaths[us_state_cases_deaths$state == state_name[i], ]
  }
  
  county_new_cases_deaths = lapply(county_new_cases_deaths_sep, diff) #calculate new cases and deaths
  county_new_cases_deaths = lapply(county_new_cases_deaths, replace_) #replace NA with 0
  
  us_data = diff(us_data)
  us_data = replace_(us_data)
  
  states_cases_timeseries = lapply(county_new_cases_deaths, new_case)
  states_deaths_timeseries = lapply(county_new_cases_deaths, new_deaths)
  
  us_cases_perday = new_case(us_data)
  us_deaths_perday = new_deaths(us_data)
  
  us_cases_perday_change = diff_percent(us_data)
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  heatmap_cases = plot_ly()
  heatmap_cases = heatmap_cases %>% add_trace(
                                      type = "choropleth",
                                      geojson = county,
                                      locations = current_county_data$fips,
                                      z = current_county_data$cases,
                                      colorscale = "Reds",
                                      zmin = 0,
                                      zmax=  max(county_data$cases)*0.05,
                                      marker = list(line=list(
                                                  width = 0)),
                                      hoverinfo = 'text',
                                      showscale = FALSE,
                                      text = ~paste('</br> State: ', current_county_data$state,
                                                    '</br> County: ', current_county_data$county,
                                                    '</br> Number of cases: ', current_county_data$cases))
  heatmap_cases = heatmap_cases %>% colorbar(title = "COVID 19 cases in US")
  heatmap_cases = heatmap_cases %>% layout(geo = g) %>% 
                                        config(modeBarButtonsToRemove = c("zoomInGeo",
                                                                          "zoomOutGeo",
                                                                          "hoverClosestGeo",
                                                                          "select2d",
                                                                          "lasso2d",
                                                                          "toImage",
                                                                          "pan2d"),
                                               displaylogo = FALSE)
  
  heatmap_deaths = plot_ly()
  heatmap_deaths = heatmap_deaths %>% add_trace(
                                        type = "choropleth",
                                        geojson = county,
                                        locations = current_county_data$fips,
                                        z = current_county_data$deaths,
                                        colorscale = "Reds",
                                        zmin = 0,
                                        showscale = FALSE,
                                        zmax=  max(county_data$deaths)*0.05,
                                        marker = list(line=list(
                                                                width = 0)),
                                        hoverinfo = 'text',
                                        text = ~paste('</br> State: ', current_county_data$state,
                                                      '</br> County: ', current_county_data$county,
                                                      '</br> Number of fatality: ', current_county_data$deaths))
  heatmap_deaths = heatmap_deaths %>% colorbar(title = "COVID 19 deaths in US")
  heatmap_deaths = heatmap_deaths %>% layout(geo = g) %>% 
                                        config(modeBarButtonsToRemove = c("zoomInGeo",
                                                                          "zoomOutGeo",
                                                                          "hoverClosestGeo",
                                                                          "select2d",
                                                                          "lasso2d",
                                                                          "toImage",
                                                                          "pan2d"),
                                                displaylogo = FALSE)
  
  #get most recent data
  current_us_state_cases_deaths = us_state_cases_deaths[us_state_cases_deaths$date == Sys.Date()-1, ]
  
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
                                          
                                           type = 'bar',
                                           orientation = 'h') %>%
                                              layout(xaxis=list(fixedrange=TRUE),
                                                     height = 1000) %>% 
                                              layout(yaxis=list(fixedrange=TRUE)) %>%
                                              config(displayModeBar = FALSE)
 
  #barplot for deaths
  current_us_state_deaths_barplot = plot_ly(current_us_state_deaths,
                                           y = ~state,
                                           x = ~deaths,
                                           color = '#CC1480',
                                           type = 'bar',
                                           orientation = 'h') %>%
                                              layout(xaxis=list(fixedrange=TRUE),
                                                     height = 1000) %>% 
                                              layout(yaxis=list(fixedrange=TRUE)) %>%
                                              config(displayModeBar = FALSE)
  
  
  output$cases_timeseries = renderPlotly(us_cases_perday)
  output$deaths_timeseries = renderPlotly(us_deaths_perday)
  output$heatmap_cases = renderPlotly(heatmap_cases)
  output$heatmap_deaths = renderPlotly(heatmap_deaths)
  output$state_cases_barplot = renderPlotly(current_us_state_cases_barplot)
  output$state_deaths_barplot = renderPlotly(current_us_state_deaths_barplot)
}