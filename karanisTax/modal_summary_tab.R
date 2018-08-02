info_renderTable <- function(data, ...) {
  nice_col_headings2 <-
    plyr::mapvalues(
      colnames(data),
      from = display_table_labels$data.name,
      to = display_table_labels$display.name,
      warn_missing = FALSE
    ) %>%
    tools::toTitleCase() %>% trimws()
  
  the_na_labels <- display_table_labels %>%
    filter(data.name %in% colnames(data)) %>%
    select(label.for.null.fields) %>%
    .[[1]] %>%
    as.list()
  
  the_na_labels <- setNames(the_na_labels, colnames(data))
  
  data <- data %>%
    replace_na(the_na_labels)
  
  colnames(data) <- nice_col_headings2
  
  data
}


output$renderTable_Summary <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(payer, payerGreekName) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')

output$renderTable_location <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(day, month, year) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')


output$renderTable_timeline <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(notBeforeTransactionDate,notAfterTransactionDate) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')


output$renderTable_recipients <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(recipient,recipientGreekName, taxType,value,currency) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')

output$notes <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(notes) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')

output$references <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(documentName, documentLocation,documentArea) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')

output$taxtype <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(taxType,value,currency) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')


output$modal_summaryTab <- renderUI({
  
  fluidPage(
    tableOutput("renderTable_Summary"),
    fluidRow(
      column(p("Date: "),
        tableOutput("renderTable_location"),width = 6),
      column(p("Transaction Date: "),
        tableOutput("renderTable_timeline"),width = 6)
    ),
    p("Tax Roll:"),
    tableOutput("renderTable_recipients"),
    tableOutput("notes"),
    tableOutput("references")
  )
  
})