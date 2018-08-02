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

output$chart <- renderHighchart({
  
  #time period selection
  if (is.null(input$timeperiod_data)) {
    return()
  } else {
    display_main_data <- filter_time_data(display_main_data,
                                          input$timeperiod_data[1],
                                          input$timeperiod_data[2])
  }
  # filter by tax type    
  selected_type <- input$tax_type
  if (!is.null(selected_type)) {
    display_main_data <- filter_data(display_main_data,selected_type)
  }
               desired_columns <- c("year", "taxType")
               data_to_display <- select(display_main_data,desired_columns) %>% 
                 group_by_("year") %>%
                 summarise_all(funs(sum(.,na.rm=TRUE))) %>% 
                 as.data.frame()
               data_to_display[is.na(data_to_display)] <- "Unspecified"
               colnames(data_to_display) <- c(input$by_group, names(choices_press_type))
               total_sum_per_item <- colSums(data_to_display[2:ncol(data_to_display)])
               item_occurrence <- names(total_sum_per_item[unname(total_sum_per_item) != 0 ])
               stacked_hc_chart(
                 data = data_to_display,
                 categories_column = input$by_group,
                 measure_columns = item_occurrence,
                 stacking_type = input$stack_by,
                 ordering_function = var
               ) %>%
                 hc_yAxis(minTickInterval = 1, minRange = 4, min = 0)
})