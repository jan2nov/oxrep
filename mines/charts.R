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
    "by_group",
    label = "Group by",
    choices = choices_group
  )
})
output$countby <- renderUI({
  selectInput(
    "count_by",
    label = "Count by",
    choices = choices_count,
    selected = choices_count[1]
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
  
    print(input$count_by)
    print(input$by_group)
    switch(input$count_by,
         "Number of Mines" = {
                  data_to_display <- display_main_data %>% 
                    group_by_(input$by_group) %>% 
                    select_(input$by_group) %>% 
                    count() %>% as.data.frame()
                  data_to_display[is.na(data_to_display),input$by_group] <- "Unspecified"
                  stacked_hc_chart(
                    data = data_to_display,
                    categories_column = input$by_group,
                    measure_columns = "n",
                    ordering_function = var
                  ) %>%  hc_yAxis(minTickInterval = 1, minRange = 4, min = 0) %>% 
                        hc_chart(zoomType = "x", panning = TRUE, panKey = 'shift')
         },
         {
           desired_columns <- c(input$by_group,
                                metals
                                )
           
           data_to_display <- select(display_main_data,desired_columns) %>% 
             group_by_(input$by_group) %>% 
             summarise_all(funs(sum)) %>% as.data.frame()
           # dummy_count <- seq(1:length(data_to_display$n))
           # dummy_count2 <- seq(1:length(data_to_display$n))
           # data_to_display <- cbind(data_to_display,dummy_count)
           # data_to_display <- cbind(data_to_display,dummy_count2)
           data_to_display[is.na(data_to_display)] <- "Unspecified"
           
           stacked_hc_chart(
             data = data_to_display,
             categories_column = input$by_group,
             measure_columns = metals,
             stacking_type = input$stack_by,
             ordering_function = var
           ) %>%
             hc_yAxis(minTickInterval = 1, minRange = 4, min = 0)
           # keyword <- colnames(select(display_main_data,starts_with("metalMin")))
           # find <- c("country",
           #           colnames(select(display_main_data,starts_with("metalMin"))))
           # data_to_display <- filter(display_main_data, metals == input$countby) %>%
           #   group_by_(input$by_group) %>%
           #   select_(input$by_group, "keywrd") %>%
           #   count(keywrd) %>%
           #   spread(keywrd, n) %>%
           #   as.data.frame()
         }
    )
})