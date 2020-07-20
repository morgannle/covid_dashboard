  options(shiny.sanitize.errors = FALSE)
  
  wd = "C:/Users/nghia/OneDrive/Documents/GitHub/covid_dashboard"
  #wd = "/srv/shiny-server/myapp/"
  setwd(wd)
  source("functions.R")
  
  server <- function(input, output, session) {
    selected_state = reactive(input$select)
    
    covid19_county_data = read.csv(county_file_url,
                                   stringsAsFactors=FALSE, 
                                   header=TRUE) 
    fatality_by_gender = read.csv(fatality_by_gender_url,
                                  stringsAsFactors=FALSE, 
                                  header=TRUE)
    covid19_nation_data = read.csv(us_file_url, 
                                   stringsAsFactors=FALSE, 
                                   header=TRUE)
    
    #format data
    fips = covid19_county_data$fips
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
    covid19_county_data$fips = fips
    
    covid19_county_data$date = as.Date(covid19_county_data$date)
    covid19_nation_data$date = as.Date(covid19_nation_data$date)
    
    #this variable contains time series data of all state
    covid19_state_data = covid19_county_data %>%
      group_by(state, date) %>%
      summarise(cases = sum(cases),
                deaths = sum(deaths)
                )
    
    state_name = unique(covid19_state_data$state)
    #each sub-object of these objects contains time series data of each states
    covid19_timeseries_data = list()
  
    for (i in 1:length(state.name)){
      covid19_timeseries_data[[i]] = covid19_state_data[covid19_state_data$state == state.name[i], ]
      }
    
    covid19_timeseries_data = lapply(covid19_timeseries_data, diff) #calculate new cases and deaths
    covid19_timeseries_data = lapply(covid19_timeseries_data, replaceNA) #replace NA with 0
    #include nation data in the list
    covid19_timeseries_data[[length(covid19_timeseries_data) + 1]] = replaceNA(
      diff(
        covid19_nation_data
        )
      )
    #collapse the list of list
    covid19_timeseries_data = do.call(rbind, covid19_timeseries_data)
    
    # 
    selected_state_data = reactive(
      covid19_state_data[covid19_state_data$state == selected_state(), ]
      )
    
    selected_county_data = reactive(
      covid19_county_data[covid19_county_data$state == selected_state(), ]
    )
    
    #drawing heat map of case and death in every states
    selected_state_heatmap_data = reactive(
      covid19_county_data[covid19_county_data$state == selected_state(), ]
      )
    temp_state_object = reactive(
      plot_map(selected_state_heatmap_data())
      )
    temp_state_map = reactive(
      temp_state_object()[[5]]
      )
    
    #drawing timeseries plot of case and death in every states
    selected_state_timeseries_data = reactive(
      covid19_timeseries_data[covid19_timeseries_data$state == selected_state(), ]
      )
    temp_timeplot_cases = reactive(
      plot_case(selected_state_timeseries_data())
      )
    temp_timeplot_deaths = reactive(
      plot_death(selected_state_timeseries_data())
      )
    
    #fatality by gender for selected state
    selected_state_fatality_by_gender_data = reactive(
      fatality_by_gender[fatality_by_gender$state == selected_state(), ]
      )
    pie_chart_fatality_by_gender = reactive(
      by_gender(selected_state_fatality_by_gender_data())
    )
    bar_plot_fatality_by_gender = reactive(
      by_age(selected_state_fatality_by_gender_data())
    )
    comparison_fatality_by_gender = reactive(
      compare(selected_state_fatality_by_gender_data())
    )
    
    output$cases_timeseries = renderPlotly(temp_timeplot_cases())
    output$deaths_timeseries = renderPlotly(temp_timeplot_deaths())
    
    output$heatmap = renderLeaflet(temp_state_map())
    observeEvent(input$heatmap_groups,{
      heatmap <- leafletProxy("heatmap")
      heatmap %>% clearControls()
      if (input$heatmap_groups == 'Infected') {
        heatmap %>% addLegend(position="bottomright", 
                              pal = temp_state_object()[[2]], 
                              values = temp_state_object()[[1]]$cases, 
                              title = "Infected")
      }
      else if (input$heatmap_groups == 'Fatality') {
        heatmap %>% addLegend(position="bottomright", 
                              pal = temp_state_object()[[4]], 
                              values = temp_state_object()[[3]]$deaths, 
                              title="Fatality")}
      }
    )
    
    output$state_cases_barplot = renderPlotly(
                                              barplot_case(selected_county_data())
                                              )
    output$state_deaths_barplot = renderPlotly(
                                              barplot_death(selected_county_data())
                                               )
    output$valuebox_total_case = renderValueBox(
      valueBox(total_case_valuebox(selected_state_data()), 
               width = 2, 
               subtitle = "Number of Cases",
               )
      )
    output$valuebox_new_case = renderValueBox(
      valueBox(new_case_valuebox(selected_state_data()), 
               width = 2, 
               subtitle = "New Cases",
      )
    )
    output$valuebox_total_death = renderValueBox(
      valueBox(total_death_valuebox(selected_state_data()), 
               width = 2, 
               icon = icon("<abacus"),
               subtitle = "Total Fatality",
      )
    )
    output$valuebox_new_death = renderValueBox(
      valueBox(new_death_valuebox(selected_state_data()), 
               width = 2, 
               subtitle = "New Fatality",
      )
    )
    
    output$pie_chart = renderPlotly(
      pie_chart_fatality_by_gender()
    )
    output$bar_plot = renderPlotly(
      bar_plot_fatality_by_gender()
    )
    output$compare =renderPlotly(
      comparison_fatality_by_gender()
    )
  }