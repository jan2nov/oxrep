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
  if (input$by_group == "quarries") {
    # return()
    selectInput(
      "count_by",
      label = "Count by",
      choices = c("Number of Sites",choices_group[1:2])
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
  req(input$by_group)
  req(input$count_by)
  if (input$count_by == "Number of Sites" & input$by_group!="quarries") {
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
  
    # filter by stones    
    selected_quarries <- input$quarries_mined
    if (!is.null(selected_quarries)) {
      display_main_data <- filter_data(display_main_data,selected_quarries)
    }

    # print(paste(input$by_group, input$count_by, input$stack_by))
    req(input$by_group)
    switch(input$by_group, 
           "quarries" = {
             switch (input$count_by,
                     "Number of Sites" = {
                       req(input$stack_by)
                       switch(input$stack_by,
                         "percent" = {
                           desired_columns <- unname(quarries_choices)
                           data_to_display <- select(display_main_data,desired_columns) %>% 
                           summarise_all(funs(sum(.,na.rm=TRUE))) %>%
                           as.data.frame() 
                           data_to_display[is.na(data_to_display)] <- "Unspecified"
                           data_to_display <- cbind("Quarries",data_to_display)
                           colnames(data_to_display) <-c("type",names(quarries_choices))
                           # data_to_display <- data.frame(Items = names(quarries_choices), "Percent" = t(data_to_display),row.names = NULL, check.names = FALSE)
                           stacked_hc_chart(
                             data = data_to_display,
                             categories_column = "type",
                             measure_columns = names(quarries_choices),
                             stacking_type = "percent",
                             ordering_function = var
                           )
                         },
                         {
                           desired_columns <- unname(quarries_choices)
                           data_to_display <- select(display_main_data,desired_columns) %>% 
                             summarise_all(funs(sum(.,na.rm=TRUE))) %>%
                             as.data.frame() 
                           data_to_display[is.na(data_to_display)] <- "Unspecified"
                           colnames(data_to_display) <- names(quarries_choices)
                           data_to_display <- data.frame(Items = names(quarries_choices), "Number of Quarry Sites" = t(data_to_display),row.names = NULL, check.names = FALSE)
                           stacked_hc_chart(
                             data = data_to_display,
                             categories_column = "Items",
                             measure_columns = "Number of Quarry Sites",
                             ordering_function = var
                           )
                         }
                       )
                     },
                     {
                       desired_columns <- c(input$count_by, unname(quarries_choices))
                       data_to_display <- select(display_main_data,desired_columns) %>% 
                         group_by_(input$count_by) %>%
                         summarise_all(funs(sum(.,na.rm=TRUE))) %>% 
                         as.data.frame()
                       data_to_display[is.na(data_to_display)] <- "Unspecified"
                       colnames(data_to_display) <- c(input$count_by, names(quarries_choices))
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
                     }
             )
           },
         {
             req(input$count_by)
    switch(input$count_by,
         "Number of Sites" = {
                  data_to_display <- display_main_data %>% 
                    group_by_(input$by_group) %>% 
                    select_(input$by_group) %>% 
                    count() %>% as.data.frame()
                  data_to_display[is.na(data_to_display),input$by_group] <- "Unspecified"
                  colnames(data_to_display) <- c(input$by_group, "Number of Quarry Sites")
                  stacked_hc_chart(
                    data = data_to_display,
                    categories_column = input$by_group,
                    measure_columns = "Number of Quarry Sites",
                    ordering_function = var
                  ) %>%  hc_yAxis(minTickInterval = 1, minRange = 4, min = 0) %>% 
                        hc_chart(zoomType = "x", panning = TRUE, panKey = 'shift')
         },
         "Quarries" = {
           desired_columns <- c(input$by_group, unname(quarries_choices))
           data_to_display <- select(display_main_data,desired_columns) %>% 
             group_by_(input$by_group) %>%
             summarise_all(funs(sum(.,na.rm=TRUE))) %>% 
             as.data.frame()
           data_to_display[is.na(data_to_display)] <- "Unspecified"
           colnames(data_to_display) <- c(input$by_group, names(quarries_choices))
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
    )})
})