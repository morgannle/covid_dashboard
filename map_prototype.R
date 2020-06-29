library('readr')
library('dplyr')
library('plotly')
library('zoo')
library('shinydashboard')

county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
json_url = 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'

county = rjson::fromJSON(file = json_url)

county_data = as.data.frame(read_csv(url(county_file_url)))
county_data$date = as.Date(county_data$date)
county_data$fips = as.character(county_data$fips)

current_county_data = county_data[county_data$date == Sys.Date()-2, ]

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig = plot_ly()
fig = fig %>% add_trace(
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
fig = fig %>% colorbar(title = "COVID 19 cases in US")
fig = fig %>% layout(
  title = "2019 Corona Cases"
)

fig = fig %>% layout(
  geo = g
)

fig