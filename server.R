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

server <- function(input, output, session) {
  
  covid19_county_data = read_csv(
    url(county_file_url)
  )
  
  selected_state = reactive(input$select)
  
  covid19_county_data$fips = as.character(covid19_county_data$fips)
  
  covid19_nation_data = read_csv(
    url(us_file_url)
  )
  covid19_nation_data$state = "National"
  covid19_nation_data = covid19_nation_data[ ,c(4,1,2,3)] #reorder the dataframe
  
  #this variable contains time series data of all state
  covid19_state_data = covid19_county_data %>%
    group_by(state, date) %>%
    summarise(cases = sum(cases),
              deaths = sum(deaths)
              )
  
  #get state names and number of states
  state_name = unique(covid19_county_data$state)
  state_name = state_name[order(state_name)]
  state_name = state_name[-c(37, 42, 12)]
  state_ID = seq(1:length(state_name))
  state = data.frame(state_name, state_name)
  colnames(state) = c("ID", "Name")
  #reorder the state names
  state = state[order(state$Name), ]
  #making named list
  selection_list = setNames(as.list(state$ID), as.list(state$Name))
  #number of states
  state_count = nrow(state)
  
  #display selection box
  updateSelectizeInput(session, 
                       'select', 
                       choices = selection_list, 
                       server = TRUE)
  
  #each sub-object of these objects contains time series data of each states
  covid19_timeseries_data = list()

  for (i in 1:state_count){
    covid19_timeseries_data[[i]] = covid19_state_data[covid19_state_data$state == state_name[i], ]
    }
  
  covid19_timeseries_data = lapply(covid19_timeseries_data, diff) #calculate new cases and deaths
  covid19_timeseries_data = lapply(covid19_timeseries_data, replaceNA) #replace NA with 0
  #include nation data in the list
  covid19_timeseries_data[[length(covid19_timeseries_data) + 1]] = replaceNA(
    diff(
      covid19_nation_data
      )
    )
  #collapse the list of list
  covid19_timeseries_data = do.call(rbind, covid19_timeseries_data)
  
  #drawing heat map of case and death in every states
  selected_state_heatmap_data = reactive(
    covid19_county_data[covid19_county_data$state == selected_state(), ]
    )
  temp_state_heatmap = reactive(
    plot_map_state(selected_state_heatmap_data())
    )
  
  #drawing timeseries plot of case and death in every states
  selected_state_timeseries_data = reactive(
    covid19_timeseries_data[covid19_timeseries_data$state == selected_state(), ]
    )
  temp_timeplot_cases = reactive(
    plot_case(selected_state_timeseries_data())
    )
  temp_timeplot_deaths = reactive(
    plot_death(selected_state_timeseries_data())
    )
  
  #
  selected_state_data = reactive(
    covid19_state_data[covid19_state_data$state == selected_state(), ]
    )

  output$cases_timeseries = renderLeaflet(temp_timeplot_cases())
  output$deaths_timeseries = renderLeaflet(temp_timeplot_deaths())
  output$heatmap_cases = renderLeaflet(temp_state_heatmap())                                   
  #output$state_cases_barplot = renderPlotly(current_us_state_cases_barplot)
  #output$state_deaths_barplot = renderPlotly(current_us_state_deaths_barplot)
  output$valuebox_total_case = renderValueBox(
    valueBox(total_case(selected_state_data()), 
             width = 3, 
             icon = icon("stethoscope"),
             subtitle = "Total Cases",
             )
    )
  output$valuebox_total_death = renderValueBox(
    valueBox(total_death(selected_state_data()), 
             width = 3, 
             icon = icon("stethoscope"),
             subtitle = "Total Fatality",
    )
  )
  
}