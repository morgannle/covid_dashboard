library(dplyr)
library(tidyr)
library(tidyverse)

county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
state_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
us_file_url     = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
fatality_by_gender_url = "http://data.cdc.gov/resource/9bhg-hcku.csv?$limit=10000&$$app_token=Y21ef2T1w3Ub7VVJAF8l3sGGd"

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

covid19_county_data$date = as.Date(covid19_county_data$date)
covid19_nation_data$date = as.Date(covid19_nation_data$date)
covid19_county_data$fips = as.character(covid19_county_data$fips)

temp_data = covid19_county_data
temp_data[ ,c("X", "deaths")] = NULL
temp_data = temp_data[complete.cases(temp_data), ]
temp_data = temp_data %>% pivot_wider(names_from = date, values_from = cases)
index = ncol(temp_data)
temp_growth_percent = list()
for(i in (index - 7):index){
  temp_growth_percent[[i]] = (temp_data[,i] - temp_data[,i-1])*100/temp_data[,i-1]
  
}
growth_percent = data.frame(temp_growth_percent[[index-6]],
                            temp_growth_percent[[index-5]],
                            temp_growth_percent[[index-4]],
                            temp_growth_percent[[index-3]],
                            temp_growth_percent[[index-2]],
                            temp_growth_percent[[index-1]],
                            temp_growth_percent[[index]])

growth_percent_7days_average = rowMeans(growth_percent)
growth_percent_by_county = data.frame(temp_data$state,
                                      temp_data$county,
                                      temp_data$fips,
                                      growth_percent_7days_average,
                                      row.names = NULL)
colnames(growth_percent_by_county) = c("State", "County", "fips", "7_day_average_case_growth")
  
write.csv(covid19_nation_data,
          file = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard/covid19_nation_data.csv")
write.csv(covid19_state_data,
          file = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard/covid19_state_data.csv")
write.csv(covid19_county_data,
          file = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard/covid19_county_data.csv")
write.csv(fatality_by_gender,
          file = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard/fatality_by_gender.csv")
write.csv(growth_percent_by_county,
          file = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard/growth_percent_by_county.csv")
