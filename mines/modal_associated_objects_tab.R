info_renderTable_objects <- function(data, ...) {
  nice_col_headings <-
    plyr::mapvalues(
      colnames(data),
      from = objects_table_labels$data.name,
      to = objects_table_labels$display.name,
      warn_missing = FALSE
    ) %>%
    tools::toTitleCase()
  
  the_na_labels <- objects_table_labels %>%
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


output$render_objects <- renderTable({
  modal_row_data <- modal_row_data()
  table_to_render <- filter(dt_objects_data, mineID==modal_row_data$mineID)
  table_to_render %>%
    select("type", "notBeforeFindDate", "notAfterFindDate", "description", "notes") %>%
    info_renderTable_objects()
  # table_to_render
})



output$modal_objectsTab <- renderUI({
    fluidPage(
    tableOutput("render_objects")
  )
})
