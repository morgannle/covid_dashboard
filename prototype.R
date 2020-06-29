start_time <- Sys.time()

wd = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard"
setwd(wd)
source("functions.R")

library('readr')
library('dplyr')
library('plotly')
library('zoo')

county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
us_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
county_data = read_csv(url(county_file_url))
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

us_cases_perday = new_case(us_data)
us_deaths_perday = new_deaths(us_data)

end_time <- Sys.time()
end_time - start_time



