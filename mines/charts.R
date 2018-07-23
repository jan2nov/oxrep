stacked_hc_chart <- function(data = NA,
                             categories_column = NA,
                             measure_columns = NA,
                             stacking_type = NA,
                             ordering_function = NA) {
  ordered_measure <-
    order(unlist(lapply(measure_columns, function(x) {
      ordering_function(data[, x])
    })),
    decreasing = TRUE) - 1
  
  chart <- highchart() %>%
    hc_xAxis(categories = data[, categories_column],
             title = categories_column)
  
  invisible(lapply(1:length(measure_columns), function(colNumber) {
    chart <<-
      hc_add_series(
        hc = chart,
        name = measure_columns[colNumber],
        data = data[, measure_columns[colNumber]],
        index = ordered_measure[colNumber]
      )
  }))
  
  chart %>%
    hc_chart(type = "bar") %>%
    hc_plotOptions(series = list(stacking = as.character(stacking_type))) %>%
    hc_legend(reversed = TRUE)
}


output$groupby <- renderUI({
  selectInput(
    "group_by",
    label = "Group by",
    choices = choices_group
  )
})
output$countby <- renderUI({
  selectInput(
    "count_by",
    label = "Count by",
    choices = choices_count
  )
})

output$stackby <- renderUI({
  if (input$count_by == "Number of Mines") {
    return()
  }
  selectInput(
    "stack_by",
    label = "Stack by",
    choices = choices_stack
  )
})

output$chart <- renderHighchart({
  
      #filter data by the timeline
      if (is.null(input$timeperiod_data)) {
        return()
      } else {
        display_main_data <- filter_time_data(display_main_data,
                                              input$timeperiod_data[1],
                                              input$timeperiod_data[2])
      }
      
      #filter data by metals
      selected_metals <- input$metals_mined
      if (!is.null(selected_metals)) {
        display_main_data <- filter_data(display_main_data,selected_metals)
      }
  
    switch(input$countby,
         "Number of Mines" = {
                  data_to_display <- display_main_data %>% group_by_(input$groupby) %>% select_(input$groupby) %>% count() %>% as.data.frame()
                  stacked_hc_chart(
                    data = data_to_display,
                    categories_column = input$groupby,
                    measure_columns = "n",
                    ordering_function = var
                  ) %>%  hc_yAxis(minTickInterval = 1, minRange = 4, min = 0) %>% 
                        hc_chart(zoomType = "x", panning = TRUE, panKey = 'shift')
         },
         {
           
         }
    )
})