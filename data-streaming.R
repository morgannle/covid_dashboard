library('tidyr')
library('tidyverse')
library('plotly')
library('readr')
library('dplyr')
library('zoo')
library('leaflet')
library('tigris')
library('blscrapeR')
library('flexdashboard')
#wd = "/srv/shiny-server/myapp/"
wd = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard"
setwd(wd)
source("functions.R")

county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
state_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
us_file_url     = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
fatality_by_gender_url = "http://data.cdc.gov/resource/9bhg-hcku.csv?$limit=10000&$$app_token=Y21ef2T1w3Ub7VVJAF8l3sGGd"
population_by_county_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/63d02b9d1073eff62827daf155a4fe1ef4ab7188/pop_est_2019.csv"

covid19_county_data = read_csv(
  url(county_file_url)
  )
covid19_nation_data = read_csv(
  url(us_file_url)
  )
covid19_state_data = read_csv(
  url(state_file_url)
  )
fatality_by_gender = read_csv(
  url(fatality_by_gender_url)
  )
population_by_county = read_csv(
  url(population_by_county_url)
  )

covid19_county_data$date = as.Date(covid19_county_data$date)
covid19_nation_data$date = as.Date(covid19_nation_data$date)
covid19_county_data$fips = as.character(covid19_county_data$fips)

#Time series for national data
covid19_national_timeseries = diff(covid19_nation_data)
covid19_national_timeseries = replaceNA(covid19_national_timeseries)
#fatality by gender for national data
fatality_by_gender_data = fatality_by_gender[fatality_by_gender$state == "United States", ] 
#national new cases
temp_data = covid19_county_data
temp_data[ ,c("X", "deaths")] = NULL
temp_data = temp_data[complete.cases(temp_data), ]
temp_data = temp_data %>% pivot_wider(names_from = date, values_from = cases)
index = ncol(temp_data)
temp_new_cases = list()
for(i in (index - 7):index){
  temp_new_cases[[i]] = temp_data[,i] - temp_data[,i-1]
  }
new_cases = data.frame(temp_new_cases[[index-6]],
                            temp_new_cases[[index-5]],
                            temp_new_cases[[index-4]],
                            temp_new_cases[[index-3]],
                            temp_new_cases[[index-2]],
                            temp_new_cases[[index-1]],
                            temp_new_cases[[index]])
total_cases = temp_data[ ,ncol(temp_data)]
new_cases_7days_average = rowMeans(new_cases)
new_cases_by_county = data.frame(temp_data$state,
                                 temp_data$county,
                                 temp_data$fips,
                                 new_cases_7days_average,
                                 total_cases,
                                 row.names = NULL)
colnames(new_cases_by_county) = c("State", "County", "fips", "new_cases_7day_average", "total_cases")
new_cases_per_100k = merge(new_cases_by_county, population_by_county, by = "fips")
new_cases_per_100k$new_cases_7day_average[new_cases_per_100k$new_cases_7day_average < 0] = 0
new_cases_per_100k = new_cases_per_100k %>% mutate(new_cases_per_100k = new_cases_7day_average*100000/population,
                                                    total_cases_per_100k = total_cases*100000/population)
#plotting
pie_chart_fatality_by_gender = by_gender(fatality_by_gender_data)
bar_plot_fatality_by_gender = by_age(fatality_by_gender_data)
new_cases_timeseries = plot_case(covid19_national_timeseries)
new_deaths_time_series = plot_death(covid19_national_timeseries)
heatmap_new_cases_by_county = plot_growth(new_cases_per_100k)