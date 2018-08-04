info_reference <- function(data) {
  data_return <- paste0(data$author,". ",data$year,". \"",data$title,"\"") %>% as.data.frame(stringsAsFactors=FALSE)
  row_na <- which(!is.na(data$journal))
  data_return[row_na,] <- str_c(data_return[row_na,],paste("",data[row_na,]$journal))
  row_na <- which(!is.na(data$issue))
  data_return[row_na,] <- str_c(data_return[row_na,],paste0(" ",data[row_na,]$issue,","))
  row_na <- which(!is.na(data$start))
  data_return[row_na,] <- str_c(data_return[row_na,],paste("",data[row_na,]$start))
  row_na <- which(!is.na(data$end))
  data_return[row_na,] <- str_c(data_return[row_na,],paste0("-",data[row_na,]$end,"."))
  colnames(data_return) <- "References"
  data_return
}

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

############### map ##################
output$summary_leaflet_map <- renderLeaflet({
  modal_row_data <- modal_row_data()
  
  centre_latitude <- modal_row_data %>%
    select(latitude) %>%
    .[[1]]
  
  centre_longitude <- modal_row_data %>%
    select(longitude) %>%
    .[[1]]
  
  modal_row_data %>%
    collect() %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lat = ~ latitude,
      lng = ~ longitude,
      fillOpacity = 0.7,
      stroke = TRUE,
      color = "#de2d26",
      weight = 2,
      label = ~ name,
      popup = ~ map_point_labeller(name, province, country)
    ) %>%
    setView(zoom = 6,
            lat = centre_latitude,
            lng = centre_longitude)
})
######################################

output$renderTable_Summary <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(name, province, country) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')

output$renderTable_location <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(longitude, latitude, coordinatesAccuracy) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')

output$renderTable_1half_quarries <- DT::renderDataTable({
  modal_row_data <- modal_row_data()
  name_quarry <- names(quarries_choices[1:8])
  db_name_quarry <- unname(quarries_choices[1:8])
  test <- modal_row_data %>%
    select(db_name_quarry)
  # info_renderTable()
  datatable(data = test,rownames = FALSE, colnames = name_quarry, 
            options=list(
              columnDefs = list(list(className = 'dt-center', target= 0:7)),
              ordering = FALSE,
              dom= 't',
              width = '100%',
              align = 'l',
              na = '-',
              rowCallback = htmlwidgets::JS(
                "function(row, data, rowi) {
          data.forEach(function(d,i) {
          if(typeof(d) === 'boolean') {
          $($('td', row)[i]).html(
          [
          '<center><i class=\\'',
          d ? 'fa fa-circle' : 'fa fa-circle-o',
          '\\'>',
          '</i></center>'
          ].join('')
          )
          }
          })
          }"
              ) # rowcallback)
            ))
})

output$renderTable_2half_quarries <- DT::renderDataTable({
  modal_row_data <- modal_row_data()
  name_quarry <- names(quarries_choices[9:16])
  db_name_quarry <- unname(quarries_choices[9:16])
  test <- modal_row_data %>%
    select(db_name_quarry)
  # info_renderTable()
  datatable(data = test,rownames = FALSE, colnames = name_quarry, 
            options=list(
              ordering = FALSE,
              dom= 't',
              width = '100%',
              align = 'l',
              na = '-',
              rowCallback = htmlwidgets::JS(
                "function(row, data, rowi) {
          data.forEach(function(d,i) {
          if(typeof(d) === 'boolean') {
          $($('td', row)[i]).html(
          [
          '<center><i class=\\'',
          d ? 'fa fa-circle' : 'fa fa-circle-o',
          '\\'>',
          '</i></center>'
          ].join('')
          )
          }
          })
          }"
              ) # rowcallback)
            ))
})

output$location_notes <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(locationNotes) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'none')
#
output$material_notes <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(materialNotes) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'none')
#
output$references <- renderTable({
  modal_row_data <- modal_row_data()
  ref_data <- modal_row_data %>%
    select(quarryReferences)
  ref_data <- strsplit(ref_data$quarryReferences,";")[[1]] %>% trimws()
  ref_data <- sapply(strsplit(ref_data, ":"), "[", 1)
  ref_data <- filter(dt_pub_data,pubname %in% ref_data)
  if (nrow(ref_data) != 0) {
    info_reference(ref_data)
  }
  else {
    ref_data <- modal_row_data %>%
      select(quarryReferences) %>%
      info_renderTable()
  }
},
width = '100%', align = 'l', na = 'none')

output$modal_summaryTab <- renderUI({
  
  fluidPage(
    fluidRow(
      column(
        br(),
        tableOutput("renderTable_Summary"),
        tableOutput("renderTable_location"),
        DT::dataTableOutput("renderTable_1half_quarries"),
        br(),
        DT::dataTableOutput("renderTable_2half_quarries"),
      width = 8),
      column(leafletOutput("summary_leaflet_map"),width = 4)
    ),
    p(),
    tableOutput("location_notes"),
    tableOutput("material_notes"),
    tableOutput("references")
  )
  
})