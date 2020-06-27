#plot cumulative cases and deathss
cumulate <- function(x){
  temp_state = as.data.frame(x)
  temp_state$date = as.Date(temp_state$date)
  temp_fig = plot_ly(
    x = temp_state$date,
    y = temp_state$cases,
    name = "Infected",
    type = 'scatter',
    mode = 'lines',
    fill = 'tozeroy',
    hoverinfo = 'text',
    text = ~paste('</br> Day: ', temp_state$date,
                  '</br> Total number of covid-19 Positive: ', temp_state$cases))
  temp_fig = temp_fig %>% add_trace(
    x = temp_state$date,
    y = temp_state$deaths,
    name = "Fatality",
    type = 'scatter',
    mode = 'lines',
    text = ~paste('Total number of fatality: ', temp_state$deaths))
  temp_fig = temp_fig %>% layout(hovermode = 'x')
  return(plotly_build(temp_fig)) 
}

#plot new cases
new_case <- function(x){
  temp_state = as.data.frame(x)
  temp_state$date = as.Date(temp_state$date)
  
  temp_fig = plot_ly(
    x = temp_state$date,
    y = temp_state$cases_diff,
    name = "New Infected",
    type = 'bar',
    color = "#bdbdbd",
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
  temp_fig = temp_fig %>% layout(hovermode = 'x')
  return(plotly_build(temp_fig))
}

#plot new deaths
new_deaths <- function(x){
  temp_state = as.data.frame(x)
  temp_state$date = as.Date(temp_state$date)
  
  temp_fig = plot_ly(
    x = temp_state$date,
    y = temp_state$deaths_diff,
    name = "New Fatality",
    type = 'bar',
    color = I("gray"),
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
    line = list(width = 3,
                color = "#0B0B0B"),
    hoverinfo = 'text',
    text = ~paste('Value: ', temp_state$deaths_diff)
  )
  temp_fig = temp_fig %>% layout(hovermode = 'x')
  return(plotly_build(temp_fig))
}

#this function calculate new cases and new deaths
diff <- function(x){
  temp = as.data.frame(x)
  temp$date = as.Date(temp$date)
  temp = x %>% mutate(cases_diff = cases - lag(cases),
                      deaths_diff = deaths - lag(deaths))
  return(temp)
}

#this function check if a value is negative
is.negative <- function(x){
  
}

#this function replace NA and negative numbers with 0
replace_ <- function(x){
  temp = x %>% replace(is.na(.), 0)
  return(temp)
}