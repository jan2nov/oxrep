info_renderTable <- function(data, ...) {
  nice_col_headings <-
    plyr::mapvalues(
      colnames(data),
      from = mines_table_labels$data.name,
      to = mines_table_labels$display.name,
      warn_missing = FALSE
    ) %>%
    tools::toTitleCase()
  
  the_na_labels <- mines_table_labels %>%
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



output$renderTable_summary <- renderTable({
  modal_row_data <- modal_row_data()
  
  modal_row_data %>%
    select(site, longitude, latitude,coordinateAccuracy) %>%
    info_renderTable()
},
width = '100%', align = 'l', na = 'missing')

output$summary_timeline <- renderPlot({
  modal_row_data <- modal_row_data()
  year_min <- modal_row_data$notBeforeOpeningDate - 500
  year_max <- modal_row_data$notBeforeClosingDate + 500
  set_break <- (abs(year_min)+abs(year_max))/10
  
  gg_timeline_plot(
    start = modal_row_data$notBeforeOpeningDate,
    end = modal_row_data$notBeforeClosingDate,
    minyear = year_min,
    maxyear = year_max,
    breaks = set_break
  )
})

output$renderTable_timeperiodBefore <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(notBeforeOpeningDate, notBeforeClosingDate,
           notAfterOpeningDate, notAfterClosingDate, inUseDate) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = 'missing')

output$renderTable_timeperiodAfter <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(notAfterOpeningDate, notAfterClosingDate) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = 'missing')

output$renderTable_locInfo <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(ancientName, province, country, region, miningDistrict) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = 'missing')

output$renderTable_metals <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(metals) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = '-')

output$renderTable_descr <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(description) %>%
    info_renderTable()
},
width = '100%', align = 'l',
)

output$renderTable_ref <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(references) %>%
    info_renderTable()
},
width = '100%', align = 'l',
)

output$renderTable_notes <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(notes) %>%
    info_renderTable()
},
width = '100%', align = 'l',
)

output$renderTable_geology <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(geology) %>%
    info_renderTable()
},
width = '100%', align = 'l',
)

output$renderTable_deposit <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(depositType) %>%
    info_renderTable()
},
width = '100%', align = 'l',
)

output$renderTable_explo <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(exploitationType) %>%
    info_renderTable()
},
width = '100%', align = 'l',
)

output$renderTable_technique <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(technique) %>%
    info_renderTable()
},
width = '100%', align = 'l',
na = '-')

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
      label = ~ site,
      popup = ~ map_point_labeller(
                    site,
                    province,
                    country,
                    region,
                    notBeforeOpeningDate,
                    notAfterClosingDate
                )
    ) %>%
    setView(zoom = 4,
            lat = centre_latitude,
            lng = centre_longitude)
})

output$modal_summaryTab <- renderUI({
  
  fluidPage(
    fluidRow(
      column(
        br(),
        tableOutput("renderTable_summary"),
        p("Possible operate time period:"),
        plotOutput("summary_timeline", height = "50px"),
        tableOutput("renderTable_timeperiodBefore"),
        # tableOutput("renderTable_timeperiodAfter"),
        p("Wreck location info;"),
        tableOutput("renderTable_locInfo"),
        width = 8
      ),
      column(leafletOutput("summary_leaflet_map"),width = 4)
    ),
    tableOutput("renderTable_metals"),
    tableOutput("renderTable_technique"),
    tableOutput("renderTable_descriptions"),
    tableOutput("renderTable_ref"),
    tableOutput("renderTable_notes"),
    tableOutput("renderTable_geology"),
    tableOutput("renderTable_deposit"),
    tableOutput("renderTable_explo")
  )
})