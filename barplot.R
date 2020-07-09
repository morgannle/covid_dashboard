
#split the most current data into positive cases and deaths
current_us_state_cases = current_us_state_cases_deaths[ ,c(1,3)]
current_us_state_deaths = current_us_state_cases_deaths[,c(1,4)]

#sorting
current_us_state_cases$state = factor(current_us_state_cases$state,
                                      levels = unique(current_us_state_cases$state[order(current_us_state_cases$cases, decreasing = FALSE)]))
current_us_state_deaths$state = factor(current_us_state_deaths$state,
                                       levels = unique(current_us_state_deaths$state[order(current_us_state_deaths$deaths, decreasing = FALSE)]))

#barplot for cases
current_us_state_cases_barplot = plot_ly(current_us_state_cases,
                                         y = ~state,
                                         x = ~cases,
                                         color = I("blue"),
                                         type = 'bar',
                                         orientation = 'h') %>%
  layout(
    title = "COVID 19 Cases in each State",
    xaxis=list(fixedrange=TRUE),
    height = 1000,  
    yaxis=list(fixedrange=TRUE)
  ) %>%
  config(displayModeBar = FALSE)

#barplot for deaths
current_us_state_deaths_barplot = plot_ly(current_us_state_deaths,
                                          y = ~state,
                                          x = ~deaths,
                                          color = I("gray"),
                                          type = 'bar',
                                          orientation = 'h') %>%
  layout(
    title = "COVID 19 Fatality in each State",
    xaxis=list(fixedrange=TRUE),
    height = 1000,
    yaxis=list(fixedrange=TRUE)
  ) %>%
  config(displayModeBar = FALSE)