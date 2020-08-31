f = list(
  family = "Courier New, monospace",
  size = 18,
  color = "#00008b"
)
#plot new cases
plot_case = function(x){
  temp_state = as.data.frame(x)
  temp_state$date = as.Date(
   temp_state$date
  )
  temp_rollmean =  round(
    rollmean(temp_state$cases_diff, 
             7, 
             na.pad = TRUE, 
             align = 'right')
  )
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
    y = temp_rollmean,
    name = "7-day Moving Average",
    type = 'scatter',
    mode = 'line',
    color = I("black"),
    hoverinfo = 'text',
    text = ~paste('Value: ', temp_rollmean)
  )
  temp_fig = temp_fig %>% layout(hovermode = 'x',
                                 legend = list(x = 0, y = 1),
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
plot_death = function(x){
  temp_state = as.data.frame(x)
  temp_state$date = as.Date(temp_state$date)
  temp_rollmean = round(
    rollmean(temp_state$deaths_diff, 
             7, 
             na.pad = TRUE, 
             align = 'right')
  )
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
    y = temp_rollmean,
    name = "7-day Moving Average",
    type = 'scatter',
    mode = 'line',
    color = I("black"),
    hoverinfo = 'text',
    text = ~paste('Value: ', temp_rollmean)
  )
  
  temp_fig = temp_fig %>% layout(hovermode = 'x',
                                 legend = list(x = 0, y = 1),
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
diff = function(x){
  temp = as.data.frame(x)
  temp = x %>% mutate(
    cases_diff = cases - lag(cases),
    deaths_diff = deaths - lag(deaths)
  )
  return(temp)
}

#this function replace NA with 0
replaceNA = function(x){
  temp = x %>% replace(is.na(.), 0)
  return(temp)
}

#this function plot new case growth
plot_growth = function(x){
  #format data
  fips = x$fips
  fips = lapply(fips, function(x){
    if(is.na(x) == TRUE) 
      return(0) 
    else
      return(x)
  })
  fips = lapply(fips, function(x){
    if (nchar(x) < 5)
      return(paste0("0", x))
    else return(x)
  })
  x$fips = fips
  data = x
  temp_data_cases = subset(data, 
                           select = c("State", "total_cases", "new_cases_7day_average", "new_cases_per_100k", "fips")
  )
  temp_data_cases$fips = as.character(temp_data_cases$fips)
  colnames(temp_data_cases) = c("state", "total_cases", "new_cases_7day", "cases", "GEOID")
  map.shape = tigris::counties(
                               cb = TRUE,
                               resolution='500k',
                               year = 2019)
  map_cases = geo_join(map.shape, 
                       temp_data_cases, 
                       by = "GEOID")
  
  bin = c(0, 8, 24, 40, 56, 1000000)
  pal = colorBin("YlOrRd", 
                 domain = temp_data_cases$cases,
                 bins = bin)
  temp_fig = leaflet() %>% 
    addTiles() %>% 
    #add positive cases layer
    addPolygons(
      data = map_cases,
      fillColor = ~pal(cases), 
      fillOpacity = 0.9,
      color = "#b2aeae",
      weight = 1,
      smoothFactor = 0.5,
      popup = paste0(paste0(map_cases$NAME, " county", ", ", map_cases$state),
                     "<br>",
                     "Total cases: ", prettyNum(map_cases$total_cases, 
                                                big.mark = ",",
                                                scientific = FALSE),
                     "<br>",
                     "Average daily cases: ", prettyNum(round(map_cases$new_cases_7day,3), 
                                                        big.mark = ",",
                                                        scientific = FALSE),
                     "<br>",
                     "per 100,000: ", round(map_cases$cases,3)),
      highlightOptions = highlightOptions(color = "white", 
                                          weight = 2,
                                          bringToFront = TRUE)) %>%
    setView(lng = -97.922211, lat = 39.381266, zoom = 4) %>%
    addLegend(title = "Average daily cases per 100,000", 
              position = "bottomright", 
              colors = c( "#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
              labels = c("0 - 8", "8 - 24", "24 - 40", "40 - 56", "56 and greater"))
  return(temp_fig)
}

total_case_valuebox = function(x){
  todate_temp_data = x[x$date == day, ]
  temp = prettyNum(todate_temp_data$cases, 
                   big.mark=",", 
                   scientific = FALSE)
}

new_case_valuebox = function(x){
  todate_temp_data = x[x$date == day, ]$cases - x[x$date == day - 1, ]$cases
  
  temp = prettyNum(todate_temp_data, 
                   big.mark=",", 
                   scientific = FALSE)
}

total_death_valuebox = function(x){
  todate_temp_data = x[x$date == day, ]
  temp = prettyNum(todate_temp_data$deaths, 
                   big.mark=",", 
                   scientific = FALSE)
}

new_death_valuebox = function(x){
  todate_temp_data = x[x$date == day, ]$deaths - x[x$date == day - 1, ]$deaths
  
  temp = prettyNum(todate_temp_data, 
                   big.mark=",", 
                   scientific = FALSE)
}

barplot_case = function(x){
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
                 showticklabels = TRUE),
      height = nrow(temp_data) * 25
      
    ) %>%
    config(displayModeBar = FALSE)
  return(temp_fig)
}

barplot_death = function(x){
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
                 showline = FALSE,
                 automargin = TRUE),
      height =  nrow(temp_data) * 25
    ) %>%
    config(displayModeBar = FALSE)
  return(temp_fig)
}

by_gender = function(x){
  temp_data = as.data.frame(x)
  temp_data = subset(
    temp_data[temp_data$data_as_of == max(temp_data$data_as_of), ],
    select = c("sex", "age_group", "covid_19_deaths")
  )
  male_total = temp_data[(temp_data$sex == "Male") & (temp_data$age_group == "All ages"), "covid_19_deaths"]
  female_total = temp_data[(temp_data$sex == "Female") & (temp_data$age_group == "All ages"), "covid_19_deaths"]
  temp_fig = plot_ly(labels = c("Male", "Female"),
                     values = c(male_total, female_total),
                     marker = list(
                       colors = c(I("steelblue"), I("pink"))
                     ),
                     type = 'pie') %>%
    config(displayModeBar = FALSE)
  temp_fig = temp_fig 
  return(temp_fig)
}

by_age = function(x){
  temp_data = as.data.frame(x)
  #get most recent "sex", "age_group" and "covid_19_deaths" column
  temp_data = subset(temp_data[temp_data$data_as_of == max(temp_data$data_as_of), ],
                     select = c("sex", "age_group", "covid_19_deaths"))
  temp_data = temp_data[!((temp_data$age_group == "All ages") | (temp_data$sex == "All")), ] 
  temp_data_male = temp_data[temp_data$sex == "Male", ]
  #setting order for x-axis label
  temp_data_male$order = c(1:nrow(temp_data_male))
  temp_data_male$age_group = factor(temp_data_male$age_group,
                                    levels = unique(temp_data_male$age_group[order(temp_data_male$order, 
                                                                                   decreasing = FALSE)]))
  temp_data_female = temp_data[temp_data$sex == "Female", ]
  temp_fig = plot_ly(x = ~temp_data_male$age_group,
                     y = ~temp_data_male$covid_19_deaths,
                     type = "bar",
                     name = "Male",
                     color = I("steelblue"),
                     hoverinfo = 'text',
                     text = ~paste( '</br>Gender: Male',
                                    '</br>Age group: ', temp_data_male$age_group,
                                    '</br>Fatality: ', temp_data_male$covid_19_deaths)
  ) %>%
    add_trace(
      y = ~temp_data_female$covid_19_deaths,
      name = "Female",
      color = I("pink"),
      text = ~paste('</br>Gender: Female',
                    '</br>Age group: ', temp_data_female$age_group,
                    '</br>Fatality: ', temp_data_female$covid_19_deaths)
    ) %>%
    layout(barmode = "group",
           legend = list(x = 0, y = 1))
  temp_fig = temp_fig %>%
    layout(
      xaxis=list(
        title = "",
        fixedrange = TRUE,
        showline = FALSE,
        showgrid = FALSE
      ),
      yaxis=list(
        title = "",
        fixedrange=TRUE,
        showticklabels = TRUE,
        showline = FALSE,
        showgrid = FALSE
      )
    ) %>%
    config(displayModeBar = FALSE)
  return(temp_fig)
}