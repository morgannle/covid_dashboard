wd = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard"
setwd(wd)

source("functions.R")
library('readr')
library('dplyr')
library('plotly')
library('zoo')
library('shinydashboard')
library('blscrapeR')
library('leaflet')
library('tigris')

county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
us_file_url     = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
json_url        = "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"

server <- function(input, output) {
  
  county_geo_fips = rjson::fromJSON(file = json_url)
  
  covid19_county_data = read_csv(
    url(county_file_url)
  )
  
  covid19_county_data$fips = as.character(covid19_county_data$fips)
  
  covid19_nation_data = read_csv(
    url(us_file_url)
  )
  
  #this variable contains time series data of all state
  covid19_state_data = covid19_county_data %>%
    group_by(state, date) %>%
    summarise(cases = sum(cases),
              deaths = sum(deaths)
              )
  
  #get state names and number of states
  state_name = unique(covid19_county_data$state)
  state_name[length(state_name) + 1] = "United States"
  state_ID = seq(1:length(state_name))
  state = data.frame(state_name, state_ID)
  colnames(state) = c("Name", "ID")
  
  #number of states
  state_count = nrow(state)
  
  #each sub-object of these objects contains time series of each states
  covid19_state_data_list_of_list = list()
  covid19_county_data_list_of_list = list()

  for (i in 1:state_count){
    covid19_state_data_list_of_list[[i]] = covid19_state_data[covid19_state_data$state == state_name[i], ]
    covid19_county_data_list_of_list[[i]] = covid19_county_data[covid19_county_data$state == state_name[i], ]
  }
  
  covid19_day_to_day_state_data = lapply(covid19_state_data_list_of_list, diff) #calculate new cases and deaths
  covid19_day_to_day_state_data = lapply(covid19_state_data_list_of_list, replaceNA) #replace NA with 0
  
  covid19_day_to_day_nation_data = diff(covid19_nation_data) #calculate new cases and deaths
  covid19_day_to_day_nation_data = replaceNA(covid19_day_to_day_nation_data) #replace NA with 0
  
  covid19_day_to_day_case_plot = lapply(covid19_day_to_day_state_data, plot_case) #plot new case of all state  
  covid19_day_to_day_death_plot = lapply(covid19_day_to_day_state_data, plot_death) #plot new death of all state
  
  covid_19_day_to_day_case_nation_plot = plot_case(covid19_day_to_day_nation_data) #plot new case for US
  covid_19_day_to_day_death_nation_plot = plot_death(covid19_day_to_day_nation_data) #plot new death for US
  
  #this objects store all time-series plot for every state in United States
  covid19_day_to_day_case_plot[[length(covid19_day_to_day_case_plot)+1]] = covid_19_day_to_day_case_nation_plot
  covid19_day_to_day_death_plot[[length(covid19_day_to_day_death_plot)+1]] = covid_19_day_to_day_death_nation_plot
  
  #drawing heat map of case and death in every states
  #covid19 county data to date
  cal = covid19_county_data[covid19_county_data$state == "Texas", ]
  
  #output$cases_timeseries = renderPlotly(us_cases_perday)
  #output$deaths_timeseries = renderPlotly(us_deaths_perday)
  output$heatmap_cases = renderLeaflet(plot_case_map(cal))
  output$heatmap_deaths = renderLeaflet(plot_death_map(cal))
  #output$heatmap_deaths = renderPlotly(heatmap_deaths)
  #output$state_cases_barplot = renderPlotly(current_us_state_cases_barplot)
  #output$state_deaths_barplot = renderPlotly(current_us_state_deaths_barplot)
}