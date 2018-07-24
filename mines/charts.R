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
  if (input$by_group == "metals") {
    return()
  }

  selectInput(
    "count_by",
    label = "Count by",
    choices = choices_count
    # selected = choices_count[1]
  )
})

output$stackby <- renderUI({
  req(input$by_group)
  req(input$count_by)
  if (input$count_by == "Number of Mines" & input$by_group!="metals") {
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
  

    print(paste(input$by_group, input$count_by, input$stack_by))
    req(input$by_group)
    switch(input$by_group, 
           "metals" = {
             switch (input$stack_by,
                     "percent" = {
                       desired_columns <- metals
                       data_to_display <- select(display_main_data,desired_columns) %>% 
                         summarise_all(funs(sum(.,na.rm=TRUE))) %>%
                         as.data.frame() 
                       data_to_display[is.na(data_to_display)] <- "Unspecified"
                       colnames(data_to_display) <- metals_name
                       data_to_display <- data.frame(Metals = metals_name, "Number of Mines" = t(data_to_display),row.names = NULL, check.names = FALSE)
                       highchart() %>% 
                         hc_chart(type = "pie") %>% 
                         hc_add_series_labels_values(labels = data_to_display$Metals, values = data_to_display$`Number of Mines`)%>%    
                         
                         hc_tooltip(crosshairs = TRUE, borderWidth = 5, sort = TRUE, shared = TRUE, table = TRUE,
                                    pointFormat = paste('<b>: {point.percentage:.1f}&#8239;%</b>')
                         )
                     },
                     {        
             desired_columns <- metals
             data_to_display <- select(display_main_data,desired_columns) %>% 
               summarise_all(funs(sum(.,na.rm=TRUE))) %>%
               as.data.frame() 
             data_to_display[is.na(data_to_display)] <- "Unspecified"
             colnames(data_to_display) <- metals_name
             data_to_display <- data.frame(Metals = metals_name, "Number of Mines" = t(data_to_display),row.names = NULL, check.names = FALSE)
             stacked_hc_chart(
               data = data_to_display,
               categories_column = "Metals",
               measure_columns = "Number of Mines",
               ordering_function = var
             )
                     }
             )
           },
           {
             req(input$count_by)
    switch(input$count_by,
         "Number of Mines" = {
                  data_to_display <- display_main_data %>% 
                    group_by_(input$by_group) %>% 
                    select_(input$by_group) %>% 
                    count() %>% as.data.frame()
                  data_to_display[is.na(data_to_display),input$by_group] <- "Unspecified"
                  colnames(data_to_display) <- c(input$by_group, "Number of Mines")
                  stacked_hc_chart(
                    data = data_to_display,
                    categories_column = input$by_group,
                    measure_columns = "Number of Mines",
                    ordering_function = var
                  ) %>%  hc_yAxis(minTickInterval = 1, minRange = 4, min = 0) %>% 
                        hc_chart(zoomType = "x", panning = TRUE, panKey = 'shift')
         },
         "Metals" = {
           desired_columns <- c(input$by_group, metals)
           data_to_display <- select(display_main_data,desired_columns) %>% 
             group_by_(input$by_group) %>%
             summarise_all(funs(sum(.,na.rm=TRUE))) %>% 
             as.data.frame()
           data_to_display[is.na(data_to_display)] <- "Unspecified"
           colnames(data_to_display) <- c(input$by_group, metals_name)
           total_sum_per_metal <- colSums(data_to_display[2:ncol(data_to_display)])
           metal_occurrence <- names(total_sum_per_metal[unname(total_sum_per_metal) != 0 ])
           stacked_hc_chart(
             data = data_to_display,
             categories_column = input$by_group,
             measure_columns = metal_occurrence,
             stacking_type = input$stack_by,
             ordering_function = var
           ) %>%
             hc_yAxis(minTickInterval = 1, minRange = 4, min = 0)
         },
         "Mining Techniques" = {
           desired_columns <- c(input$by_group, technique)
           data_to_display <- select(display_main_data,desired_columns) %>% 
             group_by_(input$by_group) %>%
             summarise_all(funs(sum(.,na.rm=TRUE))) %>% 
             as.data.frame()
           data_to_display[is.na(data_to_display)] <- "Unspecified"
           colnames(data_to_display) <- c(input$by_group, technique_name)
           total_sum_per_technique <- colSums(data_to_display[2:ncol(data_to_display)])
           technique_occurrence <- names(total_sum_per_technique[unname(total_sum_per_technique) != 0 ])
           stacked_hc_chart(
             data = data_to_display,
             categories_column = input$by_group,
             measure_columns = technique_occurrence,
             stacking_type = input$stack_by,
             ordering_function = var
           ) %>%
             hc_yAxis(minTickInterval = 1, minRange = 4, min = 0)
         }
    )})
})