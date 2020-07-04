#plot cumulative cases and deaths
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

#plot new cases
plot_case <- function(x){
  temp_state = as.data.frame(x)
  temp_state$date = as.Date(temp_state$date)
  
  temp_fig = plot_ly(
    x = temp_state$date,
    y = temp_state$cases_diff,
    name = "New Infected",
    type = 'scatter',
    mode = 'line',
    color = I("darkslategray4"),
    hoverinfo = 'text',
    text = ~paste('</br> Day: ', temp_state$date,
                  '</br> New case: ', temp_state$cases_diff)
    )
  
  temp_fig = temp_fig %>% add_trace(
    x = temp_state$date,
    y = rollmean(temp_state$cases_diff, 
                 7, 
                 na.pad = TRUE, 
                 align = 'right'),
    name = "7-day Moving Average",
    type = 'scatter',
    mode = 'line',
    line = list(width = 3,
                color = "#FF0101"),
    hoverinfo = 'text',
    text = ~paste('Value: ', temp_state$cases_diff)
  )
  temp_fig = temp_fig %>% layout(hovermode = 'x',
                                 legend = list(x = 0, y = 1),
                                 title = "Day-Over-Day New Cases",
                                 xaxis = list(fixedrange = TRUE,
                                              #title = "",
                                              #zeroline = FALSE,
                                              showline = FALSE,
                                              #showticklabels = FALSE,
                                              showgrid = FALSE),
                                 yaxis = list(fixedrange = TRUE,
                                              title = "",
                                              #zeroline = FALSE,
                                              showline = FALSE,
                                              #showticklabels = FALSE,
                                              showgrid = FALSE),
                                 showlegend = TRUE)
  
  temp_fig = temp_fig %>% config(modeBarButtonsToRemove = c("zoomInGeo",
                                                            "zoomOutGeo",
                                                            "hoverClosestGeo",
                                                            'hoverClosestCartesian',
                                                            "select2d",
                                                            "lasso2d",
                                                            "toImage",
                                                            "pan2d",
                                                            'toggleSpikelines',
                                                            'hoverCompareCartesian'),
                                 displaylogo = FALSE)
  return(
    plotly_build(temp_fig)
    )
}

#plot new deaths
plot_death <- function(x){
  temp_state = as.data.frame(x)
  temp_state$date = as.Date(temp_state$date)
  
  temp_fig = plot_ly(
    x = temp_state$date,
    y = temp_state$deaths_diff,
    name = "New Fatality",
    type = 'scatter',
    mode = "line",
    color = I("steelblue"),
    hoverinfo = 'text',
    text = ~paste('</br> Day: ', temp_state$date,
                  '</br> New Fatality: ', temp_state$deaths_diff)
  )
  
  temp_fig = temp_fig %>% add_trace(
    x = temp_state$date,
    y = rollmean(temp_state$deaths_diff, 
                 7, 
                 na.pad = TRUE, 
                 align = 'right'),
    name = "7-day Moving Average",
    type = 'scatter',
    mode = 'line',
    color = I("black"),
    hoverinfo = 'text',
    text = ~paste('Value: ', temp_state$deaths_diff)
  )
  
  temp_fig = temp_fig %>% layout(hovermode = 'x',
                                 legend = list(x = 0, y = 1),
                                 title = "Day-Over-Day New Deaths",
                                 xaxis = list(fixedrange = TRUE,
                                              title = "",
                                              #zeroline = FALSE,
                                              showline = FALSE,
                                              #showticklabels = FALSE,
                                              showgrid = FALSE),
                                 yaxis = list(fixedrange = TRUE,
                                              title = "",
                                              #zeroline = FALSE,
                                              showline = FALSE,
                                              #showticklabels = FALSE,
                                              showgrid = FALSE),
                                 showlegend = TRUE)
  
  temp_fig = temp_fig %>% config(modeBarButtonsToRemove = c("zoomInGeo",
                                                            "zoomOutGeo",
                                                            "hoverClosestGeo",
                                                            'hoverClosestCartesian',
                                                            "select2d",
                                                            "lasso2d",
                                                            "toImage",
                                                            "pan2d",
                                                            'toggleSpikelines',
                                                            'hoverCompareCartesian'),
                                 displaylogo = FALSE)
  return(
    plotly_build(temp_fig)
    )
}

#this function calculate new cases and new deaths
diff <- function(x){
  temp = as.data.frame(x)
  temp = x %>% mutate(
    cases_diff = cases - lag(cases),
    deaths_diff = deaths - lag(deaths)
  )
  return(temp)
}

#this function replace NA with 0
replaceNA <- function(x){
  temp = x %>% replace(is.na(.), 0)
  return(temp)
}

#this function plot heatmap for total cases
plot_case_map <- function(x){
  
  temp_data = x
  
  temp_heatmap_case = plot_ly()
  
  temp_heatmap_case = temp_heatmap_case %>% add_trace(
    type = "choropleth",
    geojson = county_geo_fips,
    locations = temp_data$fips,
    z = temp_data$cases,
    colorscale = "Reds",
    zmin = 0,
    zmax=  max(temp_data$cases)*0.05,
    marker = list(
      line=list(width = 0)
    ),
    hoverinfo = 'text',
    showscale = TRUE,
    text = ~paste('</br> State: ', temp_data$state,
                  '</br> County: ',temp_data$county,
                  '</br> Number of cases: ', temp_data$cases)
    )
  
  temp_heatmap_case = temp_heatmap_case %>% layout(geo = g,
                                                   title = "COVID 19 Cases in United States") %>% 
    config(modeBarButtonsToRemove = c("zoomInGeo",
                                      "zoomOutGeo",
                                      "hoverClosestGeo",
                                      "select2d",
                                      "lasso2d",
                                      "toImage",
                                      "pan2d"),
           displaylogo = FALSE)
  return(
    plotly_build(temp_heatmap_case)
         )
}

plot_death_map <- function(x){
  temp_data = x
  
  temp_heatmap_death = plot_ly()
  
  temp_heatmap_death = temp_heatmap_death %>% add_trace(
    type = "choropleth",
    geojson = county_geo_fips,
    locations = temp_data$fips,
    z = temp_data$deaths,
    colorscale = "Reds",
    zmin = 0,
    showscale = TRUE,
    zmax=  max(temp_data$deaths)*0.05,
    marker = list(
      line = list(width = 0)
      ),
    hoverinfo = 'text',
    text = ~paste('</br> State: ', temp_data$state,
                  '</br> County: ', temp_data$county,
                  '</br> Number of fatality: ', temp_data$deaths)
    )
  
  temp_heatmap_death = temp_heatmap_death %>% layout(geo = g,
                                                     title = "COVID 19 Fatality in United States") %>% 
    config(modeBarButtonsToRemove = c("zoomInGeo",
                                      "zoomOutGeo",
                                      "hoverClosestGeo",
                                      "select2d",
                                      "lasso2d",
                                      "toImage",
                                      "pan2d"),
           displaylogo = FALSE)
  return(
    plotly_build(temp_heatmap_case)
  )
}