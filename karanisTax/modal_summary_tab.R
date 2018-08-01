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
      color = "#756bb1",
      weight = 2,
      label = ~ site,
      popup = ~ map_point_labeller(site, province, country)
    ) %>%
    setView(zoom = 6,
            lat = centre_latitude,
            lng = centre_longitude)
})
######################################

output$renderTable_Summary <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(site, location, province, country) %>%
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


output$renderTable_timeline <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(notBeforeConstructionDate, notAfterConstructionDate, notBeforeAbandonmentDate, notAfterAbandonmentDate) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')


output$renderTable_presses <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(countOfOilPresses, countOfWinePresses, countOfUndefinedPresses, totalPresses) %>%
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
    select(PressReferences) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')

############## timeline plot ###################
output$summary_timeline <- renderPlot({
  modal_row_data <- modal_row_data()
  year_min <- modal_row_data$notBeforeConstructionDate
  year_max <- modal_row_data$notAfterConstructionDate
  year_plus <- ceiling((abs(year_min)+abs(year_max))/4)
  year_min <- year_min - year_plus
  year_max <- year_max + year_plus
  set_break <- floor((abs(year_min)+abs(year_max))/10)
  print(set_break)
  
  gg_timeline_plot(
    start = modal_row_data$notBeforeConstructionDate,
    end = modal_row_data$notAfterConstructionDate,
    minyear = year_min,
    maxyear = year_max,
    breaks = set_break
  )
})
################################################

output$modal_summaryTab <- renderUI({
  
  fluidPage(
    fluidRow(
      column(
        br(),
        tableOutput("renderTable_Summary"),
        tableOutput("renderTable_location"),
        tableOutput("renderTable_timeline"),
        plotOutput("summary_timeline", height = "50px"),
        tableOutput("renderTable_presses"),
      width = 8),
      column(leafletOutput("summary_leaflet_map"),width = 4)
    ),
    p(),
    tableOutput("notes")
    # tableOutput("references")
  )
  
})