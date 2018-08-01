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

output$countby <- renderUI({
  req(input$by_group)
  if (input$by_group == "press") {
    # return()
    selectInput(
      "count_by",
      label = "Count by",
      choices = choices_group[1:2]
      # selected = choices_count[1]
    )
  } else {
    selectInput(
      "count_by",
      label = "Count by",
      choices = choices_count
      # selected = choices_count[1]
    )
  }
})


output$stackby <- renderUI({
  req(input$count_by)
  if (input$count_by == "Number of Presses") {
    return()
  }
    selectInput(
    "stack_by",
    label = "Stack by",
    choices = choices_stack
    # selected = "percent"
  )
})

output$chart <- renderHighchart({
  
  #time period selection
  if (is.null(input$timeperiod_data)) {
    return()
  } else {
    display_main_data <- filter_time_data(display_main_data,
                                          input$timeperiod_data[1],
                                          input$timeperiod_data[2])
  }
  # filter by press type    
  selected_presses <- input$press_type
  if (!is.null(selected_presses)) {
    display_main_data <- filter_data(display_main_data,selected_presses)
  }

    print(paste(input$by_group, input$count_by, input$stack_by))
    req(input$by_group)
    switch(input$by_group,
           "press" = {
             desired_columns <- c(input$count_by, unname(choices_press_type))
             data_to_display <- select(display_main_data,desired_columns) %>% 
               group_by_(input$count_by) %>%
               summarise_all(funs(sum(.,na.rm=TRUE))) %>% 
               as.data.frame()
             data_to_display[is.na(data_to_display)] <- "Unspecified"
             colnames(data_to_display) <- c(input$count_by, names(choices_press_type))
             plot_col <- data_to_display[,1]
             data_to_display <- setNames(data.frame(t(data_to_display[,-1])), plot_col)
             data_to_display$categories <- rownames(data_to_display)
             stacked_hc_chart(
               data = data_to_display,
               categories_column = "categories",
               measure_columns = plot_col,
               stacking_type = input$stack_by,
               ordering_function = var
             )
           },
           {
             req(input$count_by)
      switch(input$count_by,     
             "Number of Presses" = {
               data_to_display <- display_main_data %>% 
                 group_by_(input$by_group) %>% 
                 select_(input$by_group) %>% 
                 count() %>% as.data.frame()
               data_to_display[is.na(data_to_display),input$by_group] <- "Unspecified"
               colnames(data_to_display) <- c(input$by_group, "Number of Presses")
               stacked_hc_chart(
                 data = data_to_display,
                 categories_column = input$by_group,
                 measure_columns = "Number of Presses",
                 ordering_function = var
               ) %>%  hc_yAxis(minTickInterval = 1, minRange = 4, min = 0) %>% 
                 hc_chart(zoomType = "x", panning = TRUE, panKey = 'shift')
             },
             {
               desired_columns <- c(input$by_group, unname(choices_press_type))
               data_to_display <- select(display_main_data,desired_columns) %>% 
                 group_by_(input$by_group) %>%
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
             }
      )
           }
    )
})