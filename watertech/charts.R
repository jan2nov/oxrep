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
    # selected = choices_group[1]
  )
})
output$stackby <- renderUI({
  selectInput(
    "stack_by",
    label = "Stack by",
    choices = choices_stack
    # selected = "percent"
  )
})

output$chart <- renderHighchart({
  
  # filter by type
  selected_filter_bar <- input$filterbar
  if (!is.null(selected_filter_bar)) {
    display_main_data <- filter_data(display_main_data,selected_filter_bar)
  }
  
  #time period selection
  if (is.null(input$timeperiod_data)) {
    return()
  } else {
    display_main_data <- filter_time_data(display_main_data,
                                          input$timeperiod_data[1],
                                          input$timeperiod_data[2])
  }
               req(input$by_group)
                             desired_columns <- c(input$by_group, "type")
                             data_to_display <- select(display_main_data,desired_columns) %>% 
                               group_by_(input$by_group)
                             colnames(data_to_display) <- c("name","value")
                             data_to_display[is.na(data_to_display$name),"name"] <- "Non Specified"
                             data_to_display[is.na(data_to_display$value),"value"] <- "Unknown"
                             choices_actual <- unique(data_to_display$value)
                             data_to_display <- as.data.frame.matrix(with(data_to_display,table(name,value)))
                             data_to_display$show <- rownames(data_to_display)
                             stacked_hc_chart(
                               data = data_to_display,
                               categories_column = "show",
                               measure_columns = choices_actual,
                               stacking_type = input$stack_by,
                               ordering_function = var
                         )
})