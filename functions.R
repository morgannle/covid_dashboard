state_bounding_box = read.csv("state_bounding_box.csv")
day = Sys.Date() - 2
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
    color = I("black"),
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
plot_map_state <- function(x){
  data = x
  #get state name
  state_name = unique(data$state)
  #get state bounding box
  state_bounding = state_bounding_box[state_bounding_box$NAME == state_name, ]
  #get most recent data
  data = data[data$date == day, ]
  
  temp_data_cases = subset(data, 
                           select = c("cases", "fips")
  )
  temp_data_deaths = subset(data, 
                            select = c("deaths", "fips")
  )
  
  colnames(temp_data_cases) <- c("cases", "GEOID")
  colnames(temp_data_deaths) <- c("deaths", "GEOID")
  
  fips = substring(data$fips[1], 1, 2) #get fips code of the state
  
  #download map shape
  map.shape = tigris::counties(state = fips, 
                               cb = TRUE,
                               resolution='500k')
  
  #convert tabular data into geo-spatial data
  map_cases <- geo_join(map.shape, 
                        temp_data_cases, 
                        by = "GEOID")
  map_deaths <- geo_join(map.shape, 
                         temp_data_deaths, 
                         by = "GEOID")
  
  #color palette
  roundUp <- function(x) 10^ceiling(log10(x))
  risk_bins_cases <-c(
    0, 
    ceiling(max(temp_data_cases$cases) * 0.01), 
    ceiling(max(temp_data_cases$cases) * 0.05), 
    ceiling(max(temp_data_cases$cases) * 0.1), 
    ceiling(max(temp_data_cases$cases) * 0.3), 
    ceiling(max(temp_data_cases$cases) * 0.5), 
    ceiling(max(temp_data_cases$cases))
  )
  
  pal_cases <- colorBin("YlOrRd", 
                        domain = temp_data_cases$cases,
                        bins = risk_bins_cases
                        )
  pal_deaths <- colorNumeric("Purples", 
                            domain = temp_data_deaths$deaths
                            )
  #make map
  temp_fig = leaflet(options = leafletOptions(minZoom = 5)) %>% 
    addTiles() %>% 
    #add positive cases layer
    addPolygons(
      data = map_cases,
      fillColor = ~pal_cases(cases), 
      fillOpacity = 1,
      group = "Infected",
      color = "#b2aeae",
      weight = 1,
      smoothFactor = 0.5,
      popup = paste0("County: ", 
                     map_cases$NAME,
                     "<br>",
                     "Number of cases: ",
                     prettyNum(map_cases$cases, 
                               big.mark=",", 
                               scientific = FALSE)
      ),
      highlightOptions = highlightOptions(color = "white", 
                                          weight = 2,
                                          bringToFront = TRUE)
    ) %>%
    #add deaths layer
    addPolygons(
      data = map_deaths,
      fillColor = ~pal_deaths(deaths), 
      fillOpacity = 1, 
      group = "Fatality",
      color = "#b2aeae",
      weight = 1,
      smoothFactor = 0.5,
      popup = paste0("County: ", 
                     map_deaths$NAME,
                     "<br>",
                     "Number of fatality: ",
                     prettyNum(map_deaths$deaths, 
                               big.mark=",", 
                               scientific = FALSE)
      ),
      highlightOptions = highlightOptions(color = "white", 
                                          weight = 2,
                                          bringToFront = TRUE)
    ) %>%
    #add legends
    addLegend(
      title = "Positive Cases",
      pal = pal_cases, 
      values = map_cases$cases, 
      group = "Infected",
      opacity = 1) %>%
    addLegend(
      title = "Fatality",
      pal = pal_deaths, 
      values = map_deaths$deaths,
      group = "Fatality",
      opacity = 1) %>%
    #add layer control
    addLayersControl(
      baseGroups = c ("Infected", "Fatality"),
      #overlayGroups = c ("Infected", "Fatality"),
      position = "topleft",
      options = layersControlOptions(collapsed = FALSE)
      ) %>% 
    #set max bound
    setMaxBounds( lng1 = state_bounding$xmin,
                  lat1 = state_bounding$ymin,
                  lng2 = state_bounding$xmax,
                  lat2 = state_bounding$ymax)   
  return(temp_fig)
}

total_case_valuebox <- function(x){
  todate_temp_data = x[x$date == day, ]
  temp = prettyNum(todate_temp_data$cases, 
                                    big.mark=",", 
                                    scientific = FALSE)
}

new_case_valuebox <- function(x){
  todate_temp_data = x[x$date == day, ]$cases - x[x$date == day - 1, ]$cases
  
  temp = prettyNum(todate_temp_data, 
                   big.mark=",", 
                   scientific = FALSE)
}

total_death_valuebox <- function(x){
  todate_temp_data = x[x$date == day, ]
  temp = prettyNum(todate_temp_data$deaths, 
                   big.mark=",", 
                   scientific = FALSE)
}

new_death_valuebox <- function(x){
  todate_temp_data = x[x$date == day, ]$deaths - x[x$date == day - 1, ]$deaths
  
  temp = prettyNum(todate_temp_data, 
                   big.mark=",", 
                   scientific = FALSE)
}

barplot_case <- function(x){
  temp_data = x[x$date == day, ]
  temp_data$county = factor(temp_data$county,
                           levels = unique(temp_data$county[order(temp_data$cases, decreasing = FALSE)]))
  temp_fig = plot_ly(temp_data,
                      y = ~county,
                      x = ~cases,
                      color = I("darkslategray4"),
                      type = 'bar',
                      orientation = 'h') %>%
                                         layout(
                                                title = "COVID 19 cases in each county",
                                                xaxis=list(fixedrange=TRUE,
                                                            showline = FALSE),
                                                yaxis=list(fixedrange=TRUE,
                                                            title = " ",
                                                            showline = FALSE),
                                                            height = 1000
                                                            ) %>%
                                                              config(displayModeBar = FALSE)
  return(temp_fig)
}

barplot_death <- function(x){
  temp_data = x[x$date == day, ]
  temp_data$county = factor(temp_data$county,
                            levels = unique(temp_data$county[order(temp_data$deaths, decreasing = FALSE)]))
  temp_fig = plot_ly(temp_data,
                     y = ~county,
                     x = ~deaths,
                     color = I("steelblue"),
                     type = 'bar',
                     orientation = 'h') %>%
                                        layout(
                                                title = "COVID 19 fatality in each county",
                                                xaxis=list(fixedrange=TRUE,
                                                            showline = FALSE),
                                                yaxis=list(fixedrange=TRUE,
                                                            title = " ",
                                                            showline = FALSE),
                                                            height = 1000
                                                ) %>%
                                                  config(displayModeBar = FALSE)
  return(temp_fig)
}