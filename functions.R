library('readr')
library('dplyr')
library('plotly')
library('zoo')
library('shinydashboard')
library('blscrapeR')
library('leaflet')
library('tigris')
county_file_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
us_file_url     = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
fatality_by_gender_url = "http://data.cdc.gov/resource/9bhg-hcku.csv?$limit=10000&$$app_token=Y21ef2T1w3Ub7VVJAF8l3sGGd"

state_bounding_box = read.csv("state_bounding_box.csv")
day = Sys.Date() - 2

f = list(
  family = "Courier New, monospace",
  size = 18,
  color = "#00008b"
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
  return_object = list()
  return_object[[1]] = leaflet(options = leafletOptions(minZoom = 5)) %>% 
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
  return(return_object)
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
                                                            showticklabels = TRUE),
                                                height = nrow(temp_data) * 25
                                                
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
                                                            showline = FALSE,
                                                            automargin = TRUE),
                                                            height =  nrow(temp_data) * 25
                                                ) %>%
                                                  config(displayModeBar = FALSE)
  return(temp_fig)
}

by_gender <- function(x){
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
<<<<<<< HEAD
<<<<<<< HEAD
                       colors = c(I("steelblue"), I("pink"))
                     ),
                     type = 'pie') %>%
    config(displayModeBar = FALSE)
=======
=======
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
                                    colors = c(I("steelblue"), I("pink"))
                                   ),
                     type = 'pie') %>%
                                    config(displayModeBar = FALSE)
<<<<<<< HEAD
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
=======
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
  temp_fig = temp_fig %>% layout(title = "Fatality by Gender")
  return(temp_fig)
}

by_age <- function(x){
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
<<<<<<< HEAD
<<<<<<< HEAD
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
      title = "Fatality by Age Group",
      xaxis=list(
        title = "",
        fixedrange = TRUE,
        showline = FALSE,
        showgrid = FALSE
      ),
      yaxis=list(
        title = "Fatality",
        fixedrange=TRUE,
        showticklabels = TRUE,
        showline = FALSE,
        showgrid = FALSE
      )
    ) %>%
    config(displayModeBar = FALSE)
=======
=======
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
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
                              title = "Fatality by Age Group",
                              xaxis=list(
                                          title = "",
                                          fixedrange = TRUE,
                                          showline = FALSE,
                                          showgrid = FALSE
                                          ),
                              yaxis=list(
                                          title = "Fatality",
                                          fixedrange=TRUE,
                                          showticklabels = TRUE,
                                          showline = FALSE,
                                          showgrid = FALSE
                                          )
                              ) %>%
                                config(displayModeBar = FALSE)
<<<<<<< HEAD
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
=======
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
  return(temp_fig)
}

compare <- function(x){
  temp_data = as.data.frame(x)
  #get most recent "sex", "age_group" and "covid_19_deaths" column
  temp_data = subset(temp_data[temp_data$data_as_of == max(temp_data$data_as_of), ],
                     select = c("sex", "age_group", "covid_19_deaths"))
  temp_data = temp_data[!((temp_data$age_group == "All ages") | (temp_data$sex == "All")), ] 
  temp_data_male = temp_data[temp_data$sex == "Male", ]
  temp_data_male = temp_data_male[!((temp_data_male$covid_19_deaths == 0) | (is.na(temp_data_male$covid_19_deaths) == TRUE)), ]
  temp_data_female = temp_data[temp_data$sex == "Female", ]
  temp_data_female = temp_data_female[!((temp_data_female$covid_19_deaths == 0) | (is.na(temp_data_female$covid_19_deaths) == TRUE)), ]
  temp_fig = plot_ly()
  temp_fig = temp_fig %>% add_pie(
<<<<<<< HEAD
<<<<<<< HEAD
    labels = temp_data_male$age_group,
    values = temp_data_male$covid_19_deaths,
    name = "Male",
    domain = list(
      x = c(0, 0.5),
      y = c(0.25, 0.75)
    ),
    title = list(text = 'Male Fatality', 
                 font = f)
  )
  temp_fig = temp_fig %>% add_pie(
    labels = temp_data_female$age_group,
    values = temp_data_female$covid_19_deaths,
    name = "Female",
    domain = list(
      x = c(0.5, 1),
      y = c(0.25, 0.75)
    ),
    title = list(text = 'Female Fatality', 
                 font = f)
  )
  temp_fig = temp_fig %>% layout(
    title = "Fatality Between Genders and Age Groups Comparison", 
    showlegend = FALSE,
    xaxis = list(showgrid = FALSE, 
                 zeroline = FALSE, 
                 showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, 
                 zeroline = FALSE, 
                 showticklabels = FALSE)
  ) %>%
    config(displayModeBar = FALSE)
=======
=======
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
                                  labels = temp_data_male$age_group,
                                  values = temp_data_male$covid_19_deaths,
                                  name = "Male",
                                  domain = list(
                                    x = c(0, 0.5),
                                    y = c(0.25, 0.75)
                                    ),
                                  title = list(text = 'Male Fatality', 
                                               font = f)
                                  )
  temp_fig = temp_fig %>% add_pie(
                                  labels = temp_data_female$age_group,
                                  values = temp_data_female$covid_19_deaths,
                                  name = "Female",
                                  domain = list(
                                    x = c(0.5, 1),
                                    y = c(0.25, 0.75)
                                    ),
                                  title = list(text = 'Female Fatality', 
                                               font = f)
                                  )
  temp_fig = temp_fig %>% layout(
                                  title = "Fatality Between Genders and Age Groups Comparison", 
                                  showlegend = FALSE,
                                  xaxis = list(showgrid = FALSE, 
                                               zeroline = FALSE, 
                                               showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, 
                                               zeroline = FALSE, 
                                               showticklabels = FALSE)
                                 ) %>%
                                    config(displayModeBar = FALSE)
<<<<<<< HEAD
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
=======
>>>>>>> parent of 29afdd4... Revert "Update 7/12/2020"
  return(temp_fig)
}