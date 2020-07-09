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

state_bounding_box = read.csv("state_bounding_box.csv")

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
  data = x
  #get state name
  state_name = unique(data$state)
  #get state bounding box
  state_bounding = state_bounding_box[state_bounding_box$NAME == state_name, ]
  #get most recent data
  data = data[data$date == Sys.Date() - 2, ]
  
  temp_data <- subset(data, 
                      select = c("cases", "fips")
                      )
  
  colnames(temp_data) <- c("cases", "GEOID")
  fips = substring(data$fips[1], 1, 2) #get fips code of the state
  
  #download map shape
  map.shape = tigris::counties(state = fips, 
                               cb = TRUE,
                               resolution='500k')
  
  #convert tabular data into geo-spatial data
  leafmap <- geo_join(map.shape, 
                      temp_data, 
                      by = "GEOID")
  
  #color color palette
  roundUp <- function(x) 10^ceiling(log10(x))
  risk.bins <-c(0, 
                ceiling(max(temp_data$cases) * 1/1000), 
                ceiling(max(temp_data$cases) * 1/100), 
                ceiling(max(temp_data$cases) * 1/50), 
                ceiling(max(temp_data$cases) * 1/30), 
                ceiling(max(temp_data$cases) * 1/10), 
                ceiling(max(temp_data$cases))
  )
  pal <- colorBin("YlOrRd", 
                  domain = temp_data$cases,
                  bins = risk.bins
                  )
  #make map
  temp_fig = leaflet(data = leafmap,
                     options = leafletOptions(minZoom = 5)
                     ) %>% 
    addTiles() %>% 
    addPolygons(fillColor = ~pal(cases), 
                fillOpacity = 1, 
                color = "#b2aeae",
                weight = 1,
                smoothFactor = 0.5,
                popup = paste0("County: ", 
                               leafmap$NAME,
                               "<br>",
                               "Number of cases: ",
                               prettyNum(leafmap$cases, 
                                         big.mark=",", 
                                         scientific = FALSE)
                               ),
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)
                ) %>%
    addLegend(pal = pal, 
              values = data$cases, 
              opacity = 1) %>%
    setMaxBounds( lng1 = state_bounding$xmin,
                  lat1 = state_bounding$ymin,
                  lng2 = state_bounding$xmax,
                  lat2 = state_bounding$ymax)

  return(temp_fig)
}

plot_death_map <- function(x){
  data = x
  #get state name
  state_name = unique(data$state)
  #get state bounding box
  state_bounding = state_bounding_box[state_bounding_box$NAME == state_name, ]
  #get most recent data
  data = data[data$date == Sys.Date() - 2, ]
  
  temp_data <- subset(data, 
                      select = c("deaths", "fips")
  )
  
  colnames(temp_data) <- c("deaths", "GEOID")
  fips = substring(data$fips[1], 1, 2) #get fips code of the state
  
  #download map shape
  map.shape = tigris::counties(state = fips, 
                               cb = TRUE,
                               resolution='500k')
  
  #convert tabular data into geo-spatial data
  leafmap <- geo_join(map.shape, 
                      temp_data, 
                      by = "GEOID")
  
  #color color palette
  roundUp <- function(x) 10^ceiling(log10(x))
  risk.bins <-c(0, 
                ceiling(max(temp_data$deaths) * 1/1000), 
                ceiling(max(temp_data$deaths) * 1/100), 
                ceiling(max(temp_data$deaths) * 1/50), 
                ceiling(max(temp_data$deaths) * 1/30), 
                ceiling(max(temp_data$deaths) * 1/10), 
                ceiling(max(temp_data$deaths))
  )
  pal <- colorBin("YlOrRd", 
                  domain = temp_data$deaths,
                  bins = risk.bins
  )
  #make map
  temp_fig = leaflet(data = leafmap,
                     options = leafletOptions(minZoom = 5)
  ) %>% 
    addTiles() %>% 
    addPolygons(fillColor = ~pal(deaths), 
                fillOpacity = 1, 
                color = "#b2aeae",
                weight = 1,
                smoothFactor = 0.5,
                popup = paste0("County: ", 
                               leafmap$NAME,
                               "<br>",
                               "Number of fatality: ",
                               prettyNum(leafmap$deaths, 
                                         big.mark=",", 
                                         scientific = FALSE)
                ),
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)
    ) %>%
    addLegend(pal = pal, 
              values = data$deaths, 
              opacity = 1) %>%
    setMaxBounds( lng1 = state_bounding$xmin,
                  lat1 = state_bounding$ymin,
                  lng2 = state_bounding$xmax,
                  lat2 = state_bounding$ymax)
  
  return(temp_fig)
}