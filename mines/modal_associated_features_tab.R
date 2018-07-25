info_renderTable_features <- function(data, ...) {
  nice_col_headings <-
    plyr::mapvalues(
      colnames(data),
      from = features_table_labels$data.name,
      to = features_table_labels$display.name,
      warn_missing = FALSE
    ) %>%
    tools::toTitleCase()
  
  the_na_labels <- features_table_labels %>%
    filter(data.name %in% colnames(data)) %>%
    select(label.for.null.fields) %>%
    .[[1]] %>%
    as.list()
  
  the_na_labels <- setNames(the_na_labels, colnames(data))
  
  data <- data %>%
    replace_na(the_na_labels)
  
  colnames(data) <- nice_col_headings
  
  data
}


output$render_features <- renderTable({
  modal_row_data <- modal_row_data()
  table_to_render <- filter(dt_feature_data, mineID==modal_row_data$mineID)
  table_to_render %>%
    select() %>%
    info_renderTable()
})

output$modal_featureTab <- renderUI({
  
  fluidPage(
    tableOutput("render_features")
  )
})
