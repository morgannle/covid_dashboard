county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
us_file_url     = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
fatality_by_gender_url = "http://data.cdc.gov/resource/9bhg-hcku.csv?$limit=10000&$$app_token=Y21ef2T1w3Ub7VVJAF8l3sGGd"

covid19_county_data = read_csv(
  url(county_file_url)
  )
covid19_nation_data = read_csv(
  url(us_file_url)
  )
fatality_by_gender = read_csv(
  url(fatality_by_gender_url)
  )

covid19_county_data$date = as.Date(covid19_county_data$date)
covid19_nation_data$date = as.Date(covid19_nation_data$date)
covid19_county_data$fips = as.character(covid19_county_data$fips)
  
write.csv(covid19_nation_data,
          file = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard/covid19_nation_data.csv")
write.csv(covid19_county_data,
          file = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard/covid19_county_data.csv")
write.csv(fatality_by_gender,
          file = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard/fatality_by_gender.csv")
