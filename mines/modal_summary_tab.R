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
  year_max <- modal_row_data$notAfterClosingDate + 500
  set_break <- floor((abs(year_min)+abs(year_max))/10)
  
  gg_timeline_plot(
    start = modal_row_data$notBeforeOpeningDate,
    end = modal_row_data$notAfterClosingDate,
    minyear = year_min,
    maxyear = year_max,
    breaks = set_break
  )
})

output$renderTable_timeperiodBefore <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(notBeforeOpeningDate, notAfterOpeningDate,
           notBeforeClosingDate, notAfterClosingDate, inUseDate) %>%
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

output$renderTable_metals <- DT::renderDataTable({
  modal_row_data <- modal_row_data()
  test <- modal_row_data %>%
    select(metals)
    # info_renderTable()
  datatable(data = test,rownames = FALSE, colnames = metals_name, 
            options=list(
              ordering = FALSE,
              dom= 't',
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
}
# width = '100%', align = 'l',
# na = '-'
)

output$renderTable_descriptions <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(description) %>%
    info_renderTable()
},
width = '100%', align = 'l')

output$renderTable_ref <- renderTable({
  modal_row_data <- modal_row_data()
  ref_data <- modal_row_data %>%
    select(references)
  ref_data <- strsplit(ref_data$references,";")[[1]] %>% trimws()
  ref_data <- sapply(strsplit(ref_data, ":"), "[", 1)
  ref_data <- filter(dt_pub_data,pubname %in% ref_data)
  if (nrow(ref_data) != 0) {
      info_reference(ref_data)
  }
  else {
    ref_data <- modal_row_data %>%
    select(references) %>%
    info_renderTable()
  }
}, width = '100%', align = 'l')

output$renderTable_notes <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(notes) %>%
    info_renderTable()
},
width = '100%', align = 'l')

output$renderTable_geology <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(geology) %>%
    info_renderTable()
},
width = '100%', align = 'l')

output$renderTable_deposit <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(depositType) %>%
    info_renderTable()
},
width = '100%', align = 'l')

output$renderTable_explo <- renderTable({
  modal_row_data <- modal_row_data()
  modal_row_data %>%
    select(exploitationType) %>%
    info_renderTable()
},
width = '100%', align = 'l')

output$renderTable_technique <- DT::renderDataTable({
  modal_row_data <- modal_row_data()
  test <- modal_row_data %>%
    select(technique)
  datatable(data = test,rownames = FALSE, colnames = technique_name, 
            options=list(
              ordering = FALSE,
              dom= 't',
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
}
# width = '100%', align = 'l', na = '-'
)

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
        p(),
        tableOutput("renderTable_locInfo"),
        tags$h4("Probable period mine is in use"),
        plotOutput("summary_timeline", height = "50px"),
        p(),
        tableOutput("renderTable_timeperiodBefore"),
        # tableOutput("renderTable_timeperiodAfter"),
        width = 8
      ),
      column(leafletOutput("summary_leaflet_map"),width = 4)
    ),
    # tableOutput("renderTable_metals"),
    p(),
    tags$h4("Metals mined"),
    DT::dataTableOutput("renderTable_metals"),
    p(),
    p(),
    tags$h4("Mining technique"),
    DT::dataTableOutput("renderTable_technique"),
    p(),
    fluidRow(
      column(tableOutput("renderTable_geology"),width=6),
      column(tableOutput("renderTable_deposit"), width=3),
      column(tableOutput("renderTable_explo"), width=3)
    ),
    tableOutput("renderTable_descriptions"),
    tableOutput("renderTable_notes"),
    tableOutput("renderTable_ref")
  )
})