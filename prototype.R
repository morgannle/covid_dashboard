sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()

library('readr')
library('dplyr')
library('plotly')

file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
data = read_csv(url(file_url))

#this variable contains time series of all states
us_cases_death = data %>%
                      group_by(state, date) %>%
                      summarise(cases = sum(cases),
                                death = sum(deaths))
#get state names and number of states
state_name = unique(data$state)
state_ID = seq(1:length(state_name))
state = data.frame(state_name, state_ID)
colnames(state) = c("Name", "ID")

state_count = length(state_name)
#each sub-object of these objects contains time series of each states
states_cases_death = list()
states_new_cases_death
#this object contains visualization of each states
states_cases_death_visualization = list()

for (i in 1:state_count){
  states_cases_death[[i]] = us_cases_death[us_cases_death$state == state_name[i], ]
  
  temp_state = as.data.frame(states_cases_death[[i]])
  temp_state$date = as.Date(temp_state$date)
  
  temp_fig = plot_ly(
                     x = temp_state$date,
                     y = temp_state$cases,
                     name = "Infected",
                     type = 'scatter',
                     mode = 'lines',
                     hoverinfo = 'text',
                     text = ~paste('</br> Day: ', temp_state$date,
                                   '</br> Total number of covid-19 Positive: ', temp_state$cases))
  temp_fig = temp_fig %>% add_trace(
                                    x = temp_state$date,
                                    y = temp_state$death,
                                    name = "Fatality",
                                    type = 'scatter',
                                    mode = 'lines',
                                    text = ~paste('Total number of fatality: ', temp_state$death))
  temp_fig = temp_fig %>% layout(hovermode = 'x')
  
  states_cases_death_visualization[[i]] = plotly_build(temp_fig)
  }
end_time <- Sys.time()

running_time  = end_time - start_time

